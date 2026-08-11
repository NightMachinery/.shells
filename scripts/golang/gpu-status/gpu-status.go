// gpu-status reports NVIDIA GPU occupancy for a set of hosts.
//
// With no hosts it queries the local machine directly; otherwise it fans out
// over ssh, one connection per host, concurrently. It knows nothing about any
// particular cluster -- callers supply the host list (see the `cis-gpus` zsh
// wrapper for the CIS set).
//
// The point of the USERS column: on a shared cluster "busy" is far less
// actionable than "busy with someone else's job" vs "busy with mine".
package main

import (
	"bufio"
	"context"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"sort"
	"strconv"
	"strings"
	"text/tabwriter"
	"time"

	"github.com/spf13/pflag"
	"gopkg.in/yaml.v3"
)

// ---------------------------------------------------------------- data model

// GPU is one card on one host. Field order here is the column order below.
type GPU struct {
	Index     int      `json:"index"      yaml:"index"`
	Model     string   `json:"model"      yaml:"model"`
	FreeMiB   int64    `json:"free_mib"   yaml:"free_mib"`
	TotalMiB  int64    `json:"total_mib"  yaml:"total_mib"`
	UtilPct   int      `json:"util_pct"   yaml:"util_pct"`
	Users     []string `json:"users"      yaml:"users"`
	ProcCount int      `json:"proc_count" yaml:"proc_count"`
}

// Idle reports whether nothing at all is running on the card. Deliberately
// based on process count rather than free memory: a few hundred MiB of driver
// overhead should not make an unused card look occupied.
func (g GPU) Idle() bool { return g.ProcCount == 0 }

// HostStatus is one host's result. Exactly one of GPUs / Status is meaningful.
type HostStatus struct {
	Host   string `json:"host"             yaml:"host"`
	GPUs   []GPU  `json:"gpus,omitempty"   yaml:"gpus,omitempty"`
	Status string `json:"status,omitempty" yaml:"status,omitempty"`
	Err    string `json:"error,omitempty"  yaml:"error,omitempty"`
}

// The three ways a host can fail to produce GPU rows. Kept distinct because
// they mean different things: a timeout may clear, "no GPU" never will, and a
// missing driver is a machine that needs an admin.
const (
	statusOK         = ""
	statusUnreach    = "unreachable"
	statusNoGPU      = "no-gpu"
	statusNoDriver   = "driver-missing"
)

// MaxFree is the largest single free block on the host -- the number that
// decides whether one model fits, which is the usual question.
func (h HostStatus) MaxFree() int64 {
	var m int64
	for _, g := range h.GPUs {
		if g.FreeMiB > m {
			m = g.FreeMiB
		}
	}
	return m
}

// IdleCount is what multi-GPU jobs care about.
func (h HostStatus) IdleCount() int {
	n := 0
	for _, g := range h.GPUs {
		if g.Idle() {
			n++
		}
	}
	return n
}

func (h HostStatus) MaxUtil() int {
	m := 0
	for _, g := range h.GPUs {
		if g.UtilPct > m {
			m = g.UtilPct
		}
	}
	return m
}

// ------------------------------------------------------------------ querying

// The remote script. Two nvidia-smi calls plus a `ps` to resolve owners: the
// compute-apps query returns PIDs, and a PID alone does not tell you whose job
// it is. Emitted as two tagged sections so one round trip covers both.
const probe = `
if ! command -v nvidia-smi >/dev/null 2>&1; then echo "@@NOSMI@@"; exit 0; fi
if ! nvidia-smi -L >/dev/null 2>&1; then echo "@@NODRIVER@@"; exit 0; fi
echo "@@GPUS@@"
nvidia-smi --query-gpu=index,name,memory.free,memory.total,utilization.gpu --format=csv,noheader,nounits
echo "@@PROCS@@"
for line in $(nvidia-smi --query-compute-apps=gpu_bus_id,pid --format=csv,noheader,nounits | tr -d ' '); do
  bus=${line%%,*}; pid=${line##*,}
  idx=$(nvidia-smi --query-gpu=index,gpu_bus_id --format=csv,noheader,nounits | tr -d ' ' | awk -F, -v b="$bus" '$2==b{print $1}')
  usr=$(ps -o user= -p "$pid" 2>/dev/null | tr -d ' ')
  [ -n "$usr" ] && echo "$idx,$usr"
done
`

func queryHost(ctx context.Context, host string, timeout time.Duration) HostStatus {
	hs := HostStatus{Host: host}

	cctx, cancel := context.WithTimeout(ctx, timeout)
	defer cancel()

	var cmd *exec.Cmd
	if host == "" {
		// Local mode: run the probe under sh directly, no ssh.
		hs.Host = "localhost"
		cmd = exec.CommandContext(cctx, "sh", "-c", probe)
	} else {
		// -o BatchMode=yes so a host wanting a password fails fast instead of
		// hanging the whole sweep. Exec'd directly, never through a shell, so
		// the interactive `ssh` zsh wrapper cannot inject `stty sane` into the
		// captured output.
		cmd = exec.CommandContext(cctx, "ssh",
			"-o", "BatchMode=yes",
			"-o", "StrictHostKeyChecking=accept-new",
			"-o", fmt.Sprintf("ConnectTimeout=%d", int(timeout.Seconds())),
			host, probe)
	}

	out, err := cmd.Output()
	if err != nil {
		hs.Status = statusUnreach
		if cctx.Err() == context.DeadlineExceeded {
			hs.Err = "timeout"
		} else {
			hs.Err = firstLine(err.Error())
			var ee *exec.ExitError
			if ok := asExitError(err, &ee); ok && len(ee.Stderr) > 0 {
				hs.Err = firstLine(string(ee.Stderr))
			}
		}
		return hs
	}

	body := string(out)
	switch {
	case strings.Contains(body, "@@NOSMI@@"):
		hs.Status = statusNoGPU
		return hs
	case strings.Contains(body, "@@NODRIVER@@"):
		hs.Status = statusNoDriver
		return hs
	}

	gpus, procs := parseProbe(body)
	for i := range gpus {
		if u, ok := procs[gpus[i].Index]; ok {
			gpus[i].Users = sortedKeys(u)
			gpus[i].ProcCount = len(u)
		}
	}
	hs.GPUs = gpus
	if len(gpus) == 0 {
		hs.Status = statusNoGPU
	}
	return hs
}

func parseProbe(body string) ([]GPU, map[int]map[string]int) {
	var gpus []GPU
	procs := map[int]map[string]int{}

	section := ""
	sc := bufio.NewScanner(strings.NewReader(body))
	for sc.Scan() {
		line := strings.TrimSpace(sc.Text())
		if line == "" {
			continue
		}
		if strings.HasPrefix(line, "@@") {
			section = line
			continue
		}
		switch section {
		case "@@GPUS@@":
			f := splitCSV(line)
			if len(f) < 5 {
				continue
			}
			gpus = append(gpus, GPU{
				Index:    atoi(f[0]),
				Model:    tidyModel(f[1]),
				FreeMiB:  int64(atoi(f[2])),
				TotalMiB: int64(atoi(f[3])),
				UtilPct:  atoi(f[4]),
			})
		case "@@PROCS@@":
			f := splitCSV(line)
			if len(f) < 2 {
				continue
			}
			idx := atoi(f[0])
			if procs[idx] == nil {
				procs[idx] = map[string]int{}
			}
			procs[idx][f[1]]++
		}
	}
	return gpus, procs
}

// tidyModel drops the vendor noise nvidia-smi prints, so the column stays
// narrow: "NVIDIA A100-SXM4-80GB" -> "A100-SXM4-80GB".
func tidyModel(s string) string {
	s = strings.TrimSpace(s)
	s = strings.TrimPrefix(s, "NVIDIA ")
	s = strings.TrimPrefix(s, "Tesla ")
	return s
}

// ------------------------------------------------------------------- sorting

func sortHosts(hosts []HostStatus, key string) {
	// Unreachable / GPU-less hosts always sort last: they are reported, never
	// silently dropped, but they should not push real capacity down the page.
	rank := func(h HostStatus) int {
		if h.Status == statusOK && len(h.GPUs) > 0 {
			return 0
		}
		return 1
	}
	sort.SliceStable(hosts, func(i, j int) bool {
		a, b := hosts[i], hosts[j]
		if ra, rb := rank(a), rank(b); ra != rb {
			return ra < rb
		}
		switch key {
		case "host":
			return a.Host < b.Host
		case "util":
			if a.MaxUtil() != b.MaxUtil() {
				return a.MaxUtil() < b.MaxUtil()
			}
		case "count":
			if a.IdleCount() != b.IdleCount() {
				return a.IdleCount() > b.IdleCount()
			}
			if a.MaxFree() != b.MaxFree() {
				return a.MaxFree() > b.MaxFree()
			}
		default: // "free": largest single free block wins
			if a.MaxFree() != b.MaxFree() {
				return a.MaxFree() > b.MaxFree()
			}
			if a.IdleCount() != b.IdleCount() {
				return a.IdleCount() > b.IdleCount()
			}
		}
		return a.Host < b.Host
	})
	for i := range hosts {
		g := hosts[i].GPUs
		sort.SliceStable(g, func(x, y int) bool { return g[x].Index < g[y].Index })
	}
}

// -------------------------------------------------------------------- output

func renderTable(hosts []HostStatus, w *os.File, color bool) {
	tw := tabwriter.NewWriter(w, 0, 0, 2, ' ', 0)
	fmt.Fprintln(tw, "HOST\tGPU\tMODEL\tFREE\tTOTAL\tUTIL\tUSERS")

	totalIdle, hostsWithIdle := 0, 0
	for _, h := range hosts {
		if h.Status != statusOK {
			continue
		}
		if n := h.IdleCount(); n > 0 {
			totalIdle += n
			hostsWithIdle++
		}
		for _, g := range h.GPUs {
			users := "-"
			if len(g.Users) > 0 {
				users = strings.Join(g.Users, ",")
			}
			free := fmtGiB(g.FreeMiB)
			if color && g.Idle() {
				free = "\x1b[32m" + free + "\x1b[0m" // green: fully idle
			}
			fmt.Fprintf(tw, "%s\t%d\t%s\t%s\t%s\t%d%%\t%s\n",
				h.Host, g.Index, g.Model, free, fmtGiB(g.TotalMiB), g.UtilPct, users)
		}
	}
	tw.Flush()

	fmt.Fprintf(w, "\n%d fully-free GPUs across %d hosts.\n", totalIdle, hostsWithIdle)
	for _, h := range hosts {
		if h.Status == statusOK {
			continue
		}
		detail := h.Status
		if h.Err != "" {
			detail += ": " + h.Err
		}
		fmt.Fprintf(w, "%s: %s\n", h.Host, detail)
	}
}

func fmtGiB(mib int64) string {
	return strconv.FormatFloat(float64(mib)/1024.0, 'f', 1, 64) + "G"
}

// parseSize accepts 54G / 54GiB / 40000M / plain MiB, so --min-free reads the
// way people say it.
func parseSize(s string) (int64, error) {
	s = strings.TrimSpace(strings.ToUpper(s))
	if s == "" {
		return 0, nil
	}
	mult := 1.0
	switch {
	case strings.HasSuffix(s, "GIB"):
		s, mult = strings.TrimSuffix(s, "GIB"), 1024
	case strings.HasSuffix(s, "MIB"):
		s, mult = strings.TrimSuffix(s, "MIB"), 1
	case strings.HasSuffix(s, "G"):
		s, mult = strings.TrimSuffix(s, "G"), 1024
	case strings.HasSuffix(s, "M"):
		s, mult = strings.TrimSuffix(s, "M"), 1
	}
	v, err := strconv.ParseFloat(strings.TrimSpace(s), 64)
	if err != nil {
		return 0, fmt.Errorf("cannot parse size %q", s)
	}
	return int64(v * mult), nil
}

// ---------------------------------------------------------------------- main

func main() {
	var (
		format      = pflag.String("format", "table", "output format: table|yaml|json")
		sortKey     = pflag.String("sort", "free", "sort hosts by: free|count|host|util")
		minFree     = pflag.String("min-free", "", "only show GPUs with at least this much free (e.g. 54G)")
		hostsFlag   = pflag.StringSlice("hosts", nil, "comma-separated hosts (repeatable)")
		timeout     = pflag.Duration("timeout", 8*time.Second, "per-host timeout")
		concurrency = pflag.Int("concurrency", 16, "max concurrent connections")
	)
	pflag.Parse()

	hosts := append([]string{}, *hostsFlag...)
	hosts = append(hosts, pflag.Args()...)
	if len(hosts) == 0 {
		hosts = readStdinHosts()
	}

	minMiB, err := parseSize(*minFree)
	if err != nil {
		fmt.Fprintln(os.Stderr, "gpu-status:", err)
		os.Exit(2)
	}

	local := len(hosts) == 0
	if local {
		hosts = []string{""} // sentinel: query this machine
	}

	results := make([]HostStatus, len(hosts))
	sem := make(chan struct{}, max(1, *concurrency))
	done := make(chan int, len(hosts))
	ctx := context.Background()
	for i, h := range hosts {
		go func(i int, h string) {
			sem <- struct{}{}
			defer func() { <-sem }()
			results[i] = queryHost(ctx, h, *timeout)
			done <- i
		}(i, h)
	}
	for range hosts {
		<-done
	}

	if minMiB > 0 {
		for i := range results {
			kept := results[i].GPUs[:0]
			for _, g := range results[i].GPUs {
				if g.FreeMiB >= minMiB {
					kept = append(kept, g)
				}
			}
			results[i].GPUs = kept
		}
	}

	sortHosts(results, *sortKey)

	// A machine with no NVIDIA GPU is a normal answer, not an error -- say so
	// plainly rather than printing an empty table.
	if local && len(results) == 1 && results[0].Status == statusNoGPU {
		fmt.Println("no NVIDIA GPU on this machine (nvidia-smi not found)")
		return
	}

	switch *format {
	case "json":
		enc := json.NewEncoder(os.Stdout)
		enc.SetIndent("", "  ")
		enc.Encode(results)
	case "yaml":
		enc := yaml.NewEncoder(os.Stdout)
		enc.SetIndent(2)
		enc.Encode(results)
		enc.Close()
	case "table":
		renderTable(results, os.Stdout, isTTY(os.Stdout))
	default:
		fmt.Fprintf(os.Stderr, "gpu-status: unknown --format %q\n", *format)
		os.Exit(2)
	}
}

// ----------------------------------------------------------------- utilities

func readStdinHosts() []string {
	if isTTY(os.Stdin) {
		return nil // interactive: no stdin to read, fall through to local mode
	}
	var out []string
	sc := bufio.NewScanner(os.Stdin)
	for sc.Scan() {
		for _, f := range strings.Fields(sc.Text()) {
			if f != "" {
				out = append(out, f)
			}
		}
	}
	return out
}

func isTTY(f *os.File) bool {
	fi, err := f.Stat()
	if err != nil {
		return false
	}
	return fi.Mode()&os.ModeCharDevice != 0
}

func splitCSV(s string) []string {
	parts := strings.Split(s, ",")
	for i := range parts {
		parts[i] = strings.TrimSpace(parts[i])
	}
	return parts
}

func atoi(s string) int {
	v, _ := strconv.Atoi(strings.TrimSpace(s))
	return v
}

func sortedKeys(m map[string]int) []string {
	out := make([]string, 0, len(m))
	for k := range m {
		out = append(out, k)
	}
	sort.Strings(out)
	return out
}

func firstLine(s string) string {
	if i := strings.IndexByte(s, '\n'); i >= 0 {
		return strings.TrimSpace(s[:i])
	}
	return strings.TrimSpace(s)
}

func asExitError(err error, target **exec.ExitError) bool {
	ee, ok := err.(*exec.ExitError)
	if ok {
		*target = ee
	}
	return ok
}

func max(a, b int) int {
	if a > b {
		return a
	}
	return b
}
