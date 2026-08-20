// gcp-gpu-advice answers "which GCP region can actually give me this GPU
// right now", for every machine shape at once.
//
// It wraps `gcloud beta compute advice capacity`, which is the only source
// that reports capacity rather than catalogue membership. Everything else
// lies by omission:
//
//	Billing Catalog SKU     -> priced in Finland at EUR 0.86/GPU-hr
//	accelerator-types list  -> nvidia-h100-80gb present in europe-west2-b
//	machine-types list      -> a3-highgpu-1g present in europe-north1-c
//	advice capacity         -> not supported in ANY europe-north1 zone
//
// The first three describe what the API knows the name of. Only this one asks
// whether capacity exists. Confirmed by creating: europe-north1-c returns
// `reason: stockout`.
//
// This was a zsh function first. It moved to Go for the fan-out: a full sweep
// is |shapes| x |regions| round trips (~90), which is minutes serially, and
// zsh's job control was not reliable enough to parallelise it.
//
// Nothing here creates or mutates anything, so it is always safe to run.
package main

import (
	"context"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"sort"
	"strings"
	"sync"
	"text/tabwriter"
	"time"

	"github.com/spf13/pflag"
	"golang.org/x/term"
)

// ---------------------------------------------------------------- registry

// Shape is one machine shape: the single place a human GPU name maps to a GCP
// machine type. Adding a shape is one line in `shapes` below.
type Shape struct {
	Name    string // human handle, e.g. "h100-8x"
	Machine string // GCP machine type, e.g. "a3-highgpu-8g"
	GPUs    int
	Model   string // GPU model, for display
	VRAMEa  int    // GB per GPU
}

// VRAM is the total for the node -- what decides "will my model fit".
func (s Shape) VRAM() int { return s.GPUs * s.VRAMEa }

func (s Shape) Label() string {
	return fmt.Sprintf("%dx %s (%dGB)", s.GPUs, s.Model, s.VRAM())
}

// SpotOnly reports whether the shape has no on-demand form at all. A3 and A4
// are spot-or-nothing, so recommending --on-demand for them sends you to a
// guaranteed failure rather than a fallback.
func (s Shape) SpotOnly() bool {
	return strings.HasPrefix(s.Machine, "a3-") || strings.HasPrefix(s.Machine, "a4-")
}

var shapes = []Shape{
	{"l4-1x", "g2-standard-8", 1, "L4", 24},
	{"a100-1x", "a2-highgpu-1g", 1, "A100", 40},
	{"a100-80-1x", "a2-ultragpu-1g", 1, "A100", 80},
	{"a100-80-4x", "a2-ultragpu-4g", 4, "A100", 80},
	{"h100-1x", "a3-highgpu-1g", 1, "H100", 80},
	{"h100-2x", "a3-highgpu-2g", 2, "H100", 80},
	{"h100-4x", "a3-highgpu-4g", 4, "H100", 80},
	{"h100-8x", "a3-highgpu-8g", 8, "H100", 80},
	{"h100mega-8x", "a3-megagpu-8g", 8, "H100-Mega", 80},
	{"h200-8x", "a3-ultragpu-8g", 8, "H200", 141},
	{"b200-8x", "a4-highgpu-8g", 8, "B200", 180},
}

// defaultRegions mirrors gcp_gpu_zone_candidates, collapsed to regions --
// `advice capacity` is a region-level API that names the best zone itself.
var defaultRegions = []string{
	"europe-west9", "europe-west1", "europe-west3", "europe-west4",
	"us-east4", "us-east5",
}

// vramTiers are the "how much do I need" rows of the executive summary.
var vramTiers = []int{24, 40, 80, 160, 320, 640, 1128, 1440}

func shapesSorted() []Shape {
	out := append([]Shape(nil), shapes...)
	sort.SliceStable(out, func(i, j int) bool { return out[i].VRAM() < out[j].VRAM() })
	return out
}

func shapeByName(name string) (Shape, bool) {
	for _, s := range shapes {
		if s.Name == name {
			return s, true
		}
	}
	return Shape{}, false
}

// ------------------------------------------------------------------ probing

// Result is one (shape, region) answer. Status separates the three outcomes
// that must not be conflated: a real score, "this machine does not exist
// here", and "the call failed".
type Result struct {
	Shape  string  `json:"shape"`
	Region string  `json:"region"`
	Zone   string  `json:"zone,omitempty"`
	Score  float64 `json:"obtainability"`
	Uptime string  `json:"est_uptime,omitempty"`
	Status string  `json:"status"` // ok | unsupported | error
	Note   string  `json:"note,omitempty"`
}

// Usable means the row is a real recommendation, not a catalogue gap.
// 0.1 means "forget it"; recommending it would be worse than saying nothing.
func (r Result) Usable(min float64) bool { return r.Status == "ok" && r.Score >= min }

// adviceJSON is the slice of the gcloud response we actually read.
type adviceJSON struct {
	Recommendations []struct {
		Shards []struct {
			Zone string `json:"zone"`
		} `json:"shards"`
		Scores struct {
			Obtainability   float64 `json:"obtainability"`
			EstimatedUptime string  `json:"estimatedUptime"`
		} `json:"scores"`
	} `json:"recommendations"`
}

func probe(ctx context.Context, project, machine, model, region string) Result {
	res := Result{Region: region}
	args := []string{
		"beta", "compute", "advice", "capacity",
		"--provisioning-model=" + model,
		"--instance-selection-machine-types=" + machine,
		"--target-distribution-shape=ANY_SINGLE_ZONE",
		"--size=1", "--region=" + region, "--format=json",
	}
	if project != "" {
		args = append(args, "--project="+project)
	}
	out, err := exec.CommandContext(ctx, "gcloud", args...).CombinedOutput()
	text := string(out)

	if strings.Contains(text, "not supported in location") {
		res.Status, res.Note = "unsupported", "machine spec unsupported here"
		return res
	}

	var parsed adviceJSON
	if jsonErr := json.Unmarshal(out, &parsed); jsonErr != nil {
		res.Status = "error"
		res.Note = firstLine(strings.TrimPrefix(lastAfter(text, "ERROR: "), "ERROR: "))
		if res.Note == "" && err != nil {
			res.Note = err.Error()
		}
		return res
	}
	if len(parsed.Recommendations) == 0 {
		res.Status, res.Note = "unsupported", "no recommendation returned"
		return res
	}

	rec := parsed.Recommendations[0]
	res.Status = "ok"
	res.Score = rec.Scores.Obtainability
	res.Uptime = rec.Scores.EstimatedUptime
	if len(rec.Shards) > 0 {
		res.Zone = lastAfter(rec.Shards[0].Zone, "/")
	}
	return res
}

func firstLine(s string) string {
	if i := strings.IndexByte(s, '\n'); i >= 0 {
		return strings.TrimSpace(s[:i])
	}
	return strings.TrimSpace(s)
}

func lastAfter(s, sep string) string {
	if i := strings.LastIndex(s, sep); i >= 0 {
		return s[i+len(sep):]
	}
	return s
}

// ------------------------------------------------------------------ progress

// progress is a single-line bar on stderr. It is only constructed when the
// output is a terminal, so piping to a file or a pipe stays clean; when
// disabled every method is a no-op.
type progress struct {
	mu      sync.Mutex
	total   int
	done    int
	label   string
	enabled bool
	width   int
	started time.Time
}

func newProgress(total int, enabled bool) *progress {
	p := &progress{total: total, enabled: enabled, width: 32, started: time.Now()}
	p.mu.Lock()
	p.draw()
	p.mu.Unlock()
	return p
}

// step advances the bar. Callers are the probe goroutines, so the whole
// update-and-redraw happens under one lock.
func (p *progress) step(label string) {
	if p == nil || !p.enabled {
		return
	}
	p.mu.Lock()
	defer p.mu.Unlock()
	p.done++
	p.label = label
	p.draw()
}

// draw writes the bar. Caller must hold p.mu.
func (p *progress) draw() {
	if !p.enabled {
		return
	}
	filled := 0
	if p.total > 0 {
		filled = p.done * p.width / p.total
	}
	// \x1b[K clears to end of line, so a shorter label cannot leave
	// fragments of a longer previous one behind.
	fmt.Fprintf(os.Stderr, "\rprobing [%s%s] %d/%d  %s  %s\x1b[K",
		strings.Repeat("█", filled), strings.Repeat("░", p.width-filled),
		p.done, p.total,
		time.Since(p.started).Round(time.Second),
		truncate(p.label, 28))
}

func (p *progress) finish() {
	if p == nil || !p.enabled {
		return
	}
	p.mu.Lock()
	defer p.mu.Unlock()
	fmt.Fprint(os.Stderr, "\r\x1b[K")
}

func truncate(s string, n int) string {
	if len(s) <= n {
		return s
	}
	if n <= 1 {
		return s[:n]
	}
	return s[:n-1] + "…"
}

// ------------------------------------------------------------------- output

// newTable returns a fresh tabwriter. Each table gets its own, because
// tabwriter aligns every line it is given -- sharing one across sections lets
// a long section header stretch the data columns to match it.
func newTable(out *os.File) *tabwriter.Writer {
	return tabwriter.NewWriter(out, 0, 0, 2, ' ', 0)
}

func renderTables(out *os.File, sel []Shape, model string, byShape map[string][]Result) {
	for _, s := range sel {
		spot := ""
		if s.SpotOnly() && model != "SPOT" {
			spot = "  [no on-demand form exists for this machine]"
		}
		fmt.Fprintf(out, "== %-13s %-18s %-16s (%s)%s\n",
			s.Name, s.Label(), s.Machine, model, spot)

		w := newTable(out)
		fmt.Fprintf(w, "  REGION\tBEST-ZONE\tOBTAIN\tEST-UPTIME\t\n")
		for _, r := range byShape[s.Name] {
			switch r.Status {
			case "ok":
				fmt.Fprintf(w, "  %s\t%s\t%.2f\t%s\t\n", r.Region, r.Zone, r.Score, r.Uptime)
			case "unsupported":
				fmt.Fprintf(w, "  %s\t--\tNO\t%s\t\n", r.Region, r.Note)
			default:
				fmt.Fprintf(w, "  %s\t--\t?\t%s\t\n", r.Region, truncate(r.Note, 48))
			}
		}
		w.Flush()
		fmt.Fprintln(out)
	}
}

// best returns the highest-scoring usable region for a shape.
func best(rs []Result, min float64) (Result, bool) {
	var out Result
	found := false
	for _, r := range rs {
		if !r.Usable(min) {
			continue
		}
		if !found || r.Score > out.Score {
			out, found = r, true
		}
	}
	return out, found
}

func renderSummary(out *os.File, sel []Shape, model string, byShape map[string][]Result, min float64) {
	fmt.Fprintf(out, "== executive summary (%s): smallest obtainable node per VRAM need\n", model)

	w := newTable(out)
	fmt.Fprintf(w, "  NEED\tSHAPE\tGPUS\tREGION / ZONE\tOBTAIN\t\n")

	// sel is ascending by VRAM, so the first shape that fits and is usable is
	// the smallest one -- no need to look further.
	for _, tier := range vramTiers {
		hit := false
		for _, s := range sel {
			if s.VRAM() < tier {
				continue
			}
			b, ok := best(byShape[s.Name], min)
			if !ok {
				continue
			}
			fmt.Fprintf(w, "  %dGB\t%s\t%s\t%s / %s\t%.2f\t\n",
				tier, s.Name, s.Label(), b.Region, b.Zone, b.Score)
			hit = true
			break
		}
		if !hit {
			fmt.Fprintf(w, "  %dGB\t--\t--\tnothing obtainable here\t--\t\n", tier)
		}
	}
	w.Flush()
}

// ---------------------------------------------------------------------- main

func main() {
	var (
		model        = pflag.String("provisioning-model", "SPOT", "SPOT | STANDARD | FLEX_START")
		regionsFlag  = pflag.StringSlice("regions", nil, "regions to probe (default: the candidate set)")
		project      = pflag.String("project", os.Getenv("gcp_gpu_project"), "GCP project")
		minScore     = pflag.Float64("min-obtainability", 0.3, "below this a shape is not recommended in the summary")
		concurrency  = pflag.Int("concurrency", 16, "max in-flight gcloud calls")
		asJSON       = pflag.Bool("json", false, "emit raw results as JSON")
		listShapes   = pflag.Bool("list-shapes", false, "print the shape registry and exit")
		summaryOnly  = pflag.Bool("summary", false, "print only the executive summary")
		minVRAM      = pflag.Int("vram", 0, "only consider shapes with at least this much total VRAM (GB)")
		timeout      = pflag.Duration("timeout", 3*time.Minute, "overall deadline")
		progressWhen = pflag.String("progress", "auto", "progress bar: auto | always | never")
	)
	pflag.Usage = func() {
		fmt.Fprintf(os.Stderr,
			"usage: gcp-gpu-advice [flags] [shape...]\n\n"+
				"With no shapes, probes every shape in the registry.\n"+
				"Shapes: %s\n\nflags:\n",
			strings.Join(shapeNames(), ", "))
		pflag.PrintDefaults()
	}
	pflag.Parse()

	if *listShapes {
		w := newTable(os.Stdout)
		fmt.Fprintf(w, "NAME\tMACHINE\tGPUS\tVRAM-TOTAL\t\n")
		for _, s := range shapesSorted() {
			fmt.Fprintf(w, "%s\t%s\t%dx %s\t%dGB\t\n", s.Name, s.Machine, s.GPUs, s.Model, s.VRAM())
		}
		w.Flush()
		return
	}

	sel := shapesSorted()
	if args := pflag.Args(); len(args) > 0 {
		sel = nil
		for _, a := range args {
			s, ok := shapeByName(a)
			if !ok {
				fmt.Fprintf(os.Stderr, "unknown shape %q; known: %s\n", a, strings.Join(shapeNames(), ", "))
				os.Exit(2)
			}
			sel = append(sel, s)
		}
		sort.SliceStable(sel, func(i, j int) bool { return sel[i].VRAM() < sel[j].VRAM() })
	}
	if *minVRAM > 0 {
		var keep []Shape
		for _, s := range sel {
			if s.VRAM() >= *minVRAM {
				keep = append(keep, s)
			}
		}
		sel = keep
		if len(sel) == 0 {
			fmt.Fprintf(os.Stderr, "no shape has >= %dGB of VRAM\n", *minVRAM)
			os.Exit(1)
		}
	}

	regions := *regionsFlag
	if len(regions) == 0 {
		regions = defaultRegions
	}

	ctx, cancel := context.WithTimeout(context.Background(), *timeout)
	defer cancel()

	// The bar is drawn on stderr but gated on whether the OUTPUT is a
	// terminal, so `gcp-gpu-advice > file` and `| jq` stay quiet even though
	// stderr is still a tty.
	var showBar bool
	switch *progressWhen {
	case "always":
		showBar = true
	case "never":
		showBar = false
	case "auto":
		showBar = term.IsTerminal(int(os.Stdout.Fd())) && !*asJSON
	default:
		fmt.Fprintf(os.Stderr, "--progress must be auto, always or never (got %q)\n", *progressWhen)
		os.Exit(2)
	}
	bar := newProgress(len(sel)*len(regions), showBar)

	type key struct{ shape, region string }
	results := make(map[key]Result, len(sel)*len(regions))
	var mu sync.Mutex
	var wg sync.WaitGroup
	sem := make(chan struct{}, *concurrency)

	for _, s := range sel {
		for _, r := range regions {
			wg.Add(1)
			go func(s Shape, region string) {
				defer wg.Done()
				sem <- struct{}{}
				defer func() { <-sem }()
				res := probe(ctx, *project, s.Machine, *model, region)
				res.Shape = s.Name
				mu.Lock()
				results[key{s.Name, region}] = res
				mu.Unlock()
				bar.step(s.Name + " " + region)
			}(s, r)
		}
	}
	wg.Wait()
	bar.finish()

	byShape := make(map[string][]Result, len(sel))
	for _, s := range sel {
		for _, r := range regions {
			byShape[s.Name] = append(byShape[s.Name], results[key{s.Name, r}])
		}
	}

	if *asJSON {
		flat := make([]Result, 0, len(results))
		for _, s := range sel {
			flat = append(flat, byShape[s.Name]...)
		}
		enc := json.NewEncoder(os.Stdout)
		enc.SetIndent("", "  ")
		if err := enc.Encode(flat); err != nil {
			fmt.Fprintln(os.Stderr, err)
			os.Exit(1)
		}
		return
	}

	if !*summaryOnly {
		renderTables(os.Stdout, sel, *model, byShape)
	}
	renderSummary(os.Stdout, sel, *model, byShape, *minScore)

	fmt.Fprintf(os.Stderr,
		"obtainability 0.9 good / 0.5 marginal / 0.1 forget it; below %.1f not recommended.\n"+
			"est-uptime tops out at 3600s -- for multi-day runs use 'gcp-gpu-up --flex-start'.\n"+
			"a recommendation, not proof: 'instances create' is the only definitive test.\n",
		*minScore)
}

func shapeNames() []string {
	out := make([]string, 0, len(shapes))
	for _, s := range shapesSorted() {
		out = append(out, s.Name)
	}
	return out
}
