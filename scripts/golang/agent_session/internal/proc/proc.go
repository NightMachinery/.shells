// Package proc reads the process table, the working directories of processes
// and which tmux session a process sits in. It is how the Codex and Antigravity
// adapters decide which of their sessions are live: neither agent publishes a
// listing the way `claude agents --json` does, so liveness comes from the
// processes themselves.
//
// Each source is read once per call (one `ps`, one `lsof`, one `tmux`), which
// is what keeps `live` at tens of milliseconds however many sessions there are.
package proc

import (
	"bytes"
	"os/exec"
	"strconv"
	"strings"
)

// Process is one row of the process table.
type Process struct {
	PID  int
	PPID int
	// The full command line, as `ps` shows it.
	Cmd string
}

// Run is how the package invokes a command; tests replace it.
var Run = func(name string, args ...string) ([]byte, error) {
	return exec.Command(name, args...).Output()
}

// List is every process, from one `ps`.
func List() ([]Process, error) {
	out, err := Run("ps", "-axo", "pid=,ppid=,args=")
	if err != nil {
		return nil, err
	}
	return parsePS(out), nil
}

func parsePS(out []byte) []Process {
	var procs []Process
	for _, ln := range bytes.Split(out, []byte("\n")) {
		f := strings.Fields(string(ln))
		if len(f) < 3 {
			continue
		}
		pid, err1 := strconv.Atoi(f[0])
		ppid, err2 := strconv.Atoi(f[1])
		if err1 != nil || err2 != nil {
			continue
		}
		procs = append(procs, Process{PID: pid, PPID: ppid, Cmd: strings.Join(f[2:], " ")})
	}
	return procs
}

// ByPID indexes processes by pid.
func ByPID(procs []Process) map[int]Process {
	m := make(map[int]Process, len(procs))
	for _, p := range procs {
		m[p.PID] = p
	}
	return m
}

// Cwds is the working directory of each pid, from one `lsof`. A pid that
// cannot be read (gone, or not ours) is simply absent.
func Cwds(pids []int) map[int]string {
	out := map[int]string{}
	if len(pids) == 0 {
		return out
	}

	strs := make([]string, len(pids))
	for i, p := range pids {
		strs[i] = strconv.Itoa(p)
	}
	// -F: one field per line, `p<pid>` then `n<name>`. lsof exits non-zero
	// when any pid is unreadable but still prints the rest, so the output is
	// used whatever the status.
	raw, _ := Run("lsof", "-a", "-d", "cwd", "-Fpn", "-p", strings.Join(strs, ","))
	return parseLsof(raw)
}

func parseLsof(raw []byte) map[int]string {
	out := map[int]string{}
	pid := 0
	for _, ln := range bytes.Split(raw, []byte("\n")) {
		if len(ln) == 0 {
			continue
		}
		switch ln[0] {
		case 'p':
			pid, _ = strconv.Atoi(string(ln[1:]))
		case 'n':
			if pid != 0 {
				out[pid] = string(ln[1:])
			}
		}
	}
	return out
}

// TmuxPanes maps each tmux pane's shell pid to the session it belongs to,
// from one `tmux list-panes -a`. Empty when tmux is not running.
func TmuxPanes() map[int]string {
	out := map[int]string{}
	raw, err := Run("tmux", "list-panes", "-a", "-F", "#{pane_pid}\t#{session_name}")
	if err != nil {
		return out
	}
	for _, ln := range bytes.Split(raw, []byte("\n")) {
		pidS, name, ok := strings.Cut(string(ln), "\t")
		if !ok {
			continue
		}
		if pid, err := strconv.Atoi(pidS); err == nil {
			out[pid] = name
		}
	}
	return out
}

// TmuxOf is the tmux session a process runs in, found by walking its parents
// up to a pane's shell; "" when it is not under tmux.
func TmuxOf(pid int, procs map[int]Process, panes map[int]string) string {
	for depth := 0; depth < 64 && pid > 1; depth++ {
		if name, ok := panes[pid]; ok {
			return name
		}
		p, ok := procs[pid]
		if !ok {
			return ""
		}
		pid = p.PPID
	}
	return ""
}

// Descendants is the set of pids under any of the roots, roots included.
func Descendants(roots []int, procs []Process) map[int]bool {
	children := map[int][]int{}
	for _, p := range procs {
		children[p.PPID] = append(children[p.PPID], p.PID)
	}

	out := map[int]bool{}
	queue := append([]int{}, roots...)
	for len(queue) > 0 {
		pid := queue[0]
		queue = queue[1:]
		if out[pid] {
			continue
		}
		out[pid] = true
		queue = append(queue, children[pid]...)
	}
	return out
}

// Holders maps each of the files to the pids that have it open, from one
// `lsof`. How a lock file is traced to the process holding it.
func Holders(files []string) map[string][]int {
	out := map[string][]int{}
	if len(files) == 0 {
		return out
	}
	args := append([]string{"-Fpn", "--"}, files...)
	raw, _ := Run("lsof", args...)
	return parseHolders(raw)
}

func parseHolders(raw []byte) map[string][]int {
	out := map[string][]int{}
	pid := 0
	for _, ln := range bytes.Split(raw, []byte("\n")) {
		if len(ln) == 0 {
			continue
		}
		switch ln[0] {
		case 'p':
			pid, _ = strconv.Atoi(string(ln[1:]))
		case 'n':
			if pid != 0 {
				out[string(ln[1:])] = append(out[string(ln[1:])], pid)
			}
		}
	}
	return out
}
