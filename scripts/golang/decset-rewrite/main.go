// Command decset-rewrite runs a command on a pseudo-terminal and rewrites
// DECSET/DECRST private mode numbers in the child's output stream, so that an
// application can keep asking for a mode the terminal at the far end of the
// chain does not implement.
//
// The motivating case is mouse tracking: an app that ends with ESC[?1003h
// loses the mouse on terminals that ignore any-event tracking, and rewriting
// 1003 to 1002 on the way out restores it without patching the app.
package main

import (
	"errors"
	"flag"
	"fmt"
	"io"
	"os"
	"os/exec"
	"os/signal"
	"strconv"
	"strings"
	"sync"
	"syscall"
	"time"

	"github.com/creack/pty"
	"golang.org/x/term"
)

const usageText = `usage: decset-rewrite [-map FROM=TO]... [-trace FILE] [-no-pty] [--] <command> [args...]

Runs <command> on a pty and rewrites DECSET/DECRST private mode numbers in its
output. With no -map it is a pass-through proxy.

  -map FROM=TO   rewrite private mode FROM to TO (repeatable, decimal)
  -trace FILE    append one line per private-mode sequence seen (mode numbers only)
  -no-pty        filter stdin to stdout instead of running a command
  -h, --help     this message
`

// drainTimeout caps how long we wait for the child's last output after it has
// exited. Short enough not to hang on a stuck reader, long enough for the
// final screen repaint to land.
const drainTimeout = 200 * time.Millisecond

func main() {
	os.Exit(realMain(os.Args[1:]))
}

// modeMap collects repeated -map flags.
type modeMap struct {
	m map[int]int
}

func (f *modeMap) String() string {
	if len(f.m) == 0 {
		return ""
	}
	parts := make([]string, 0, len(f.m))
	for k, v := range f.m {
		parts = append(parts, strconv.Itoa(k)+"="+strconv.Itoa(v))
	}
	return strings.Join(parts, ",")
}

func (f *modeMap) Set(s string) error {
	from, to, ok := strings.Cut(s, "=")
	if !ok {
		return fmt.Errorf("want FROM=TO, got %q", s)
	}
	fn, err := parseModeArg(from)
	if err != nil {
		return err
	}
	tn, err := parseModeArg(to)
	if err != nil {
		return err
	}
	if f.m == nil {
		f.m = map[int]int{}
	}
	f.m[fn] = tn
	return nil
}

func parseModeArg(s string) (int, error) {
	n, err := strconv.Atoi(s)
	if err != nil || n < 0 {
		return 0, fmt.Errorf("not a decimal mode number: %q", s)
	}
	return n, nil
}

func usageErr(err error) int {
	if err != nil && !errors.Is(err, flag.ErrHelp) {
		fmt.Fprintf(os.Stderr, "decset-rewrite: %v\n", err)
	}
	fmt.Fprint(os.Stderr, usageText)
	return 2
}

func realMain(args []string) int {
	fs := flag.NewFlagSet("decset-rewrite", flag.ContinueOnError)
	fs.SetOutput(io.Discard) // we print our own usage, once
	var maps modeMap
	fs.Var(&maps, "map", "rewrite private mode FROM=TO (repeatable)")
	tracePath := fs.String("trace", "", "append a line per private-mode sequence to FILE")
	noPTY := fs.Bool("no-pty", false, "filter stdin to stdout, run no command")

	if err := fs.Parse(args); err != nil {
		if errors.Is(err, flag.ErrHelp) {
			fmt.Fprint(os.Stdout, usageText)
			return 0
		}
		return usageErr(err)
	}
	argv := fs.Args()

	if *noPTY {
		if len(argv) > 0 {
			return usageErr(fmt.Errorf("-no-pty takes no command, got %q", argv[0]))
		}
	} else if len(argv) == 0 {
		return usageErr(errors.New("no command given"))
	}

	// A non-terminal stdin means nobody is going to look at the escape codes
	// anyway (`claude -p ... | jq`), so get out of the way entirely rather
	// than inserting a pty the caller never asked for.
	if !*noPTY && !term.IsTerminal(int(os.Stdin.Fd())) {
		path, err := exec.LookPath(argv[0])
		if err != nil {
			fmt.Fprintf(os.Stderr, "decset-rewrite: %v\n", err)
			return 127
		}
		if err := syscall.Exec(path, argv, os.Environ()); err != nil {
			fmt.Fprintf(os.Stderr, "decset-rewrite: exec %s: %v\n", path, err)
			return 127
		}
	}

	trace := openTrace(*tracePath)

	if *noPTY {
		rw := NewRewriter(os.Stdout, maps.m, trace)
		_, err := io.Copy(rw, os.Stdin)
		if ferr := rw.Flush(); err == nil {
			err = ferr
		}
		if err != nil {
			fmt.Fprintf(os.Stderr, "decset-rewrite: %v\n", err)
			return 1
		}
		return 0
	}

	code, err := run(argv, os.Stdin, os.Stdin, os.Stdout, runOpts{maps: maps.m, trace: trace})
	if err != nil {
		fmt.Fprintf(os.Stderr, "decset-rewrite: %v\n", err)
	}
	return code
}

// openTrace returns a TraceFunc appending to path, or nil if path is empty.
// A tracing failure is never allowed to take the child down with it: we warn
// once and carry on untraced.
func openTrace(path string) TraceFunc {
	if path == "" {
		return nil
	}
	f, err := os.OpenFile(path, os.O_WRONLY|os.O_CREATE|os.O_APPEND, 0o600)
	if err != nil {
		fmt.Fprintf(os.Stderr, "decset-rewrite: trace disabled: %v\n", err)
		return nil
	}
	var (
		mu       sync.Mutex
		broken   bool
		line     []byte
		modeList = func(dst []byte, v []int) []byte {
			for i, n := range v {
				if i > 0 {
					dst = append(dst, ';')
				}
				dst = strconv.AppendInt(dst, int64(n), 10)
			}
			return dst
		}
	)
	return func(set bool, orig, rewritten []int) {
		mu.Lock()
		defer mu.Unlock()
		if broken {
			return
		}
		line = time.Now().AppendFormat(line[:0], time.RFC3339)
		line = append(line, ' ')
		if set {
			line = append(line, "set"...)
		} else {
			line = append(line, "reset"...)
		}
		line = append(line, ' ')
		line = modeList(line, orig)
		if !sameModes(orig, rewritten) {
			line = append(line, " -> "...)
			line = modeList(line, rewritten)
		}
		line = append(line, '\n')
		if _, err := f.Write(line); err != nil {
			fmt.Fprintf(os.Stderr, "decset-rewrite: trace disabled: %v\n", err)
			broken = true
			f.Close()
		}
	}
}

func sameModes(a, b []int) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}

type runOpts struct {
	maps  map[int]int
	trace TraceFunc
	// winsize overrides the size taken from tty; used by the tests, which have
	// no terminal to ask.
	winsize *pty.Winsize
}

// run starts argv on a pty and proxies it. tty is the outer terminal, used for
// raw mode and for size tracking; it may be nil, in which case neither
// happens. in is copied to the child verbatim and may be nil. out receives the
// child's output after rewriting.
func run(argv []string, tty *os.File, in io.Reader, out io.Writer, opts runOpts) (int, error) {
	path, err := exec.LookPath(argv[0])
	if err != nil {
		return 127, err
	}

	ptmx, slave, err := pty.Open()
	if err != nil {
		return 1, err
	}
	var closeMaster sync.Once
	defer closeMaster.Do(func() { ptmx.Close() })

	if tty != nil {
		// Best effort: a child on a pty with default termios is still usable,
		// just not identical to the outer terminal.
		_ = copyTermios(tty.Fd(), slave.Fd())
	}

	// Size the pty before the child starts, so it never observes a 0x0
	// terminal and lays itself out for one.
	ws := opts.winsize
	if ws == nil && tty != nil {
		if got, err := pty.GetsizeFull(tty); err == nil {
			ws = got
		}
	}
	if ws != nil {
		_ = pty.Setsize(slave, ws)
	}

	cmd := exec.Command(path, argv[1:]...)
	cmd.Stdin, cmd.Stdout, cmd.Stderr = slave, slave, slave
	// Ctty is the index of the child's own fd, and stdin above is the slave.
	cmd.SysProcAttr = &syscall.SysProcAttr{Setsid: true, Setctty: true, Ctty: 0}
	if err := cmd.Start(); err != nil {
		slave.Close()
		return 127, err
	}
	// The parent must drop the slave, or the master never sees EOF.
	slave.Close()

	var restoreOnce sync.Once
	restore := func() {}
	if tty != nil {
		state, err := term.MakeRaw(int(tty.Fd()))
		if err == nil {
			restore = func() {
				restoreOnce.Do(func() { term.Restore(int(tty.Fd()), state) })
			}
			defer restore()
		}
	}

	sigs := make(chan os.Signal, 8)
	notify := []os.Signal{syscall.SIGINT, syscall.SIGTERM, syscall.SIGHUP, syscall.SIGQUIT}
	if tty != nil {
		notify = append(notify, syscall.SIGWINCH)
	}
	signal.Notify(sigs, notify...)
	defer signal.Stop(sigs)
	sigDone := make(chan struct{})
	go func() {
		for {
			select {
			case <-sigDone:
				return
			case s := <-sigs:
				if s == syscall.SIGWINCH {
					if tty != nil {
						_ = pty.InheritSize(tty, ptmx)
					}
					continue
				}
				// Forward and keep going: the child's exit is what ends us,
				// so it gets to run its own cleanup first.
				if cmd.Process != nil {
					_ = cmd.Process.Signal(s)
				}
			}
		}
	}()
	defer close(sigDone)

	if in != nil {
		go func() {
			buf := make([]byte, 4*1024)
			for {
				n, err := in.Read(buf)
				if n > 0 {
					if _, werr := ptmx.Write(buf[:n]); werr != nil {
						return
					}
				}
				if err != nil {
					return
				}
			}
		}()
	}

	rw := NewRewriter(out, opts.maps, opts.trace)
	outDone := make(chan struct{})
	go func() {
		defer close(outDone)
		buf := make([]byte, 64*1024)
		for {
			n, err := ptmx.Read(buf)
			if n > 0 {
				if _, werr := rw.Write(buf[:n]); werr != nil {
					return
				}
			}
			if err != nil {
				// EOF on darwin, EIO on linux: both mean the last slave fd is
				// gone and there is nothing more to read.
				return
			}
		}
	}()

	waitErr := cmd.Wait()

	select {
	case <-outDone:
	case <-time.After(drainTimeout):
		// Stop the reader before touching the Rewriter from this goroutine.
		closeMaster.Do(func() { ptmx.Close() })
		<-outDone
	}
	_ = rw.Flush()
	restore()

	return exitCode(waitErr), nil
}

// exitCode turns cmd.Wait's error into a shell-style status.
func exitCode(err error) int {
	if err == nil {
		return 0
	}
	var ee *exec.ExitError
	if errors.As(err, &ee) {
		if ws, ok := ee.Sys().(syscall.WaitStatus); ok && ws.Signaled() {
			return 128 + int(ws.Signal())
		}
		return ee.ExitCode()
	}
	return 1
}
