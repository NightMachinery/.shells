// Package progress reports how far a render has got, for a caller that shows a
// progress line or band while a large transcript converts. It is off unless
// Enable is called, and every other function is then a no-op, so the render
// path calls it unconditionally.
//
// A render is a fixed sequence of phases (read the transcript, render the
// turns, convert them with pandoc), each with a unit of its own: bytes read,
// turns rendered, bytes converted. Each phase owns a slice of the overall
// percentage, sized by how long it measured on a large transcript, so one
// number moves forward across all of them.
//
// The report is a file holding one line, `<percent> <phase> <done> <total>`,
// replaced atomically (written beside it, then renamed) so a reader polling it
// never sees half a line. It is rewritten at most every Interval while
// something changes, and once more by Finish. A file rather than stderr,
// because stderr is where the errors go, and the reader is a shell loop that
// wants the latest state, not a stream to parse.
package progress

import (
	"fmt"
	"io"
	"os"
	"path/filepath"
	"sync"
	"sync/atomic"
	"time"
)

// A Phase is one step of the render and its share of the whole, in any unit:
// the shares are normalised.
type Phase struct {
	Name   string
	Weight float64
}

// How often the file is rewritten while a phase moves.
var Interval = 200 * time.Millisecond

type span struct{ lo, hi float64 }

var (
	mu    sync.Mutex
	path  string
	spans map[string]span
	phase string
	total int64
	done  atomic.Int64
	last  string
	stop  chan struct{}
	ended chan struct{}
)

// Enable starts reporting to file, over the given phases in order.
func Enable(file string, phases []Phase) {
	var sum float64
	for _, p := range phases {
		sum += p.Weight
	}
	if sum <= 0 {
		return
	}

	mu.Lock()
	path = file
	spans = map[string]span{}
	lo := 0.0
	for _, p := range phases {
		hi := lo + 100*p.Weight/sum
		spans[p.Name] = span{lo, hi}
		lo = hi
	}
	stop, ended = make(chan struct{}), make(chan struct{})
	mu.Unlock()

	go tick(stop, ended)
}

func tick(stop, ended chan struct{}) {
	defer close(ended)
	t := time.NewTicker(Interval)
	defer t.Stop()
	for {
		select {
		case <-stop:
			return
		case <-t.C:
			write()
		}
	}
}

// Begin enters a phase whose work totals n units. A phase not given to Enable
// reports 0%.
func Begin(name string, n int64) {
	mu.Lock()
	if path == "" {
		mu.Unlock()
		return
	}
	phase, total = name, n
	done.Store(0)
	mu.Unlock()
	write()
}

// Add records n more units of the current phase as done. Safe from any
// goroutine.
func Add(n int64) {
	done.Add(n)
}

// Finish writes the final, complete report and stops the ticker.
func Finish() {
	mu.Lock()
	if path == "" {
		mu.Unlock()
		return
	}
	s, e := stop, ended
	phase, total = "done", 1
	done.Store(1)
	mu.Unlock()

	close(s)
	<-e
	write()
}

// Reader counts what is read through it as done units of the current phase.
// Without reporting it returns r itself, so the read path costs nothing.
func Reader(r io.Reader) io.Reader {
	mu.Lock()
	on := path != ""
	mu.Unlock()
	if !on {
		return r
	}
	return counting{r}
}

type counting struct{ r io.Reader }

func (c counting) Read(p []byte) (int, error) {
	n, err := c.r.Read(p)
	done.Add(int64(n))
	return n, err
}

// percent is the overall percentage for done of total in the named phase.
func percent(name string, d, n int64) int {
	if name == "done" {
		return 100
	}
	s, ok := spans[name]
	if !ok {
		return 0
	}
	f := 1.0
	if n > 0 {
		f = float64(d) / float64(n)
	}
	if f > 1 {
		f = 1
	}
	if f < 0 {
		f = 0
	}
	return int(s.lo + (s.hi-s.lo)*f)
}

func write() {
	mu.Lock()
	defer mu.Unlock()
	if path == "" || phase == "" {
		return
	}
	d := done.Load()
	if d > total && total > 0 {
		d = total
	}
	line := fmt.Sprintf("%d %s %d %d\n", percent(phase, d, total), phase, d, total)
	if line == last {
		return
	}

	tmp, err := os.CreateTemp(filepath.Dir(path), ".progress-*")
	if err != nil {
		return
	}
	_, werr := tmp.WriteString(line)
	cerr := tmp.Close()
	if werr != nil || cerr != nil || os.Rename(tmp.Name(), path) != nil {
		os.Remove(tmp.Name())
		return
	}
	last = line
}
