package main

import (
	"bytes"
	"io"
	"math/rand"
	"strings"
	"testing"
)

const e = "\x1b"

// mouseMap is the motivating map: any-event tracking down to button-event.
var mouseMap = map[int]int{1003: 1002}

type rwCase struct {
	name string
	maps map[int]int
	in   string
	want string
}

var cases = []rwCase{
	{
		name: "plain text is untouched",
		maps: mouseMap,
		in:   "hello world\n\tno escapes here",
		want: "hello world\n\tno escapes here",
	},
	{
		name: "decset is rewritten",
		maps: mouseMap,
		in:   e + "[?1003h",
		want: e + "[?1002h",
	},
	{
		name: "decrst is rewritten",
		maps: mouseMap,
		in:   e + "[?1003l",
		want: e + "[?1002l",
	},
	{
		name: "adjacent sequences",
		maps: mouseMap,
		in:   e + "[?1003h" + e + "[?1006h" + e + "[?1003l",
		want: e + "[?1002h" + e + "[?1006h" + e + "[?1002l",
	},
	{
		name: "embedded in sgr noise",
		maps: mouseMap,
		in:   "hi " + e + "[1;31mred" + e + "[0m" + e + "[?1003h bye" + e + "[2J",
		want: "hi " + e + "[1;31mred" + e + "[0m" + e + "[?1002h bye" + e + "[2J",
	},
	{
		name: "multi parameter",
		maps: mouseMap,
		in:   e + "[?1000;1003h",
		want: e + "[?1000;1002h",
	},
	{
		name: "multi parameter, every position mapped",
		maps: mouseMap,
		in:   e + "[?1003;1003l",
		want: e + "[?1002;1002l",
	},
	{
		name: "empty parameter stays empty",
		maps: mouseMap,
		in:   e + "[?;1003h",
		want: e + "[?;1002h",
	},
	{
		name: "decrqm request is untouched",
		maps: mouseMap,
		in:   e + "[?1003$p",
		want: e + "[?1003$p",
	},
	{
		name: "decrpm reply is untouched",
		maps: mouseMap,
		in:   e + "[?1003;2$y",
		want: e + "[?1003;2$y",
	},
	{
		name: "longer mode number is not a prefix match",
		maps: mouseMap,
		in:   e + "[?10031h",
		want: e + "[?10031h",
	},
	{
		name: "unmapped mode is untouched",
		maps: mouseMap,
		in:   e + "[?1002h",
		want: e + "[?1002h",
	},
	{
		name: "no maps is pure passthrough",
		maps: nil,
		in:   e + "[?1003h" + e + "[?1003l",
		want: e + "[?1003h" + e + "[?1003l",
	},
	{
		// The first ESC cannot be a CSI introducer, so it goes out verbatim
		// and the machine restarts on the second one.
		name: "esc esc restarts the machine",
		maps: mouseMap,
		in:   e + e + "[?1003h",
		want: e + e + "[?1002h",
	},
	{
		name: "abandoned sequence then a real one",
		maps: mouseMap,
		in:   e + "[?1003" + e + "[?1003h",
		want: e + "[?1003" + e + "[?1002h",
	},
	{
		name: "parameter run past the cap is flushed verbatim",
		maps: mouseMap,
		in:   e + "[?" + strings.Repeat("1", paramCap+8) + "h",
		want: e + "[?" + strings.Repeat("1", paramCap+8) + "h",
	},
	{
		name: "csi without the private marker is untouched",
		maps: mouseMap,
		in:   e + "[1003h",
		want: e + "[1003h",
	},
	{
		name: "csi with a non-private intermediate",
		maps: mouseMap,
		in:   e + "[>4;2m" + e + "[?1003h",
		want: e + "[>4;2m" + e + "[?1002h",
	},
}

// writeChunks feeds in as the given chunks and returns everything released,
// including the Flush at the end.
func writeChunks(t *testing.T, maps map[int]int, chunks []string) string {
	t.Helper()
	var buf bytes.Buffer
	r := NewRewriter(&buf, maps, nil)
	for _, c := range chunks {
		n, err := r.Write([]byte(c))
		if err != nil {
			t.Fatalf("Write: %v", err)
		}
		if n != len(c) {
			t.Fatalf("Write returned %d, want %d", n, len(c))
		}
	}
	if err := r.Flush(); err != nil {
		t.Fatalf("Flush: %v", err)
	}
	return buf.String()
}

func TestRewriteTable(t *testing.T) {
	for _, tc := range cases {
		tc := tc
		t.Run(tc.name, func(t *testing.T) {
			got := writeChunks(t, tc.maps, []string{tc.in})
			if got != tc.want {
				t.Fatalf("whole:\n got %q\nwant %q", got, tc.want)
			}
		})
	}
}

func TestRewriteIsSplitInvariant(t *testing.T) {
	for _, tc := range cases {
		tc := tc
		t.Run(tc.name, func(t *testing.T) {
			for i := 0; i <= len(tc.in); i++ {
				got := writeChunks(t, tc.maps, []string{tc.in[:i], tc.in[i:]})
				if got != tc.want {
					t.Fatalf("split at %d:\n got %q\nwant %q", i, got, tc.want)
				}
			}
			bytewise := make([]string, 0, len(tc.in))
			for i := 0; i < len(tc.in); i++ {
				bytewise = append(bytewise, tc.in[i:i+1])
			}
			if got := writeChunks(t, tc.maps, bytewise); got != tc.want {
				t.Fatalf("byte at a time:\n got %q\nwant %q", got, tc.want)
			}
		})
	}
}

func TestPartialSequenceIsHeldUntilFlush(t *testing.T) {
	var buf bytes.Buffer
	r := NewRewriter(&buf, mouseMap, nil)
	if _, err := r.Write([]byte("abc" + e + "[?100")); err != nil {
		t.Fatal(err)
	}
	if got := buf.String(); got != "abc" {
		t.Fatalf("before Flush got %q, want %q", got, "abc")
	}
	if err := r.Flush(); err != nil {
		t.Fatal(err)
	}
	if got, want := buf.String(), "abc"+e+"[?100"; got != want {
		t.Fatalf("after Flush got %q, want %q", got, want)
	}
	// A second Flush must not re-emit anything.
	if err := r.Flush(); err != nil {
		t.Fatal(err)
	}
	if got, want := buf.String(), "abc"+e+"[?100"; got != want {
		t.Fatalf("after second Flush got %q, want %q", got, want)
	}
}

// countingWriter records how many times Write was called.
type countingWriter struct {
	n   int
	buf bytes.Buffer
}

func (c *countingWriter) Write(p []byte) (int, error) {
	c.n++
	return c.buf.Write(p)
}

func TestOneUnderlyingWritePerWrite(t *testing.T) {
	var cw countingWriter
	r := NewRewriter(&cw, mouseMap, nil)
	for i := 0; i < 5; i++ {
		if _, err := r.Write([]byte("x" + e + "[?1003h")); err != nil {
			t.Fatal(err)
		}
	}
	if cw.n != 5 {
		t.Fatalf("underlying writes: got %d, want 5", cw.n)
	}
}

func TestWriteDoesNotAllocateAfterWarmup(t *testing.T) {
	r := NewRewriter(io.Discard, mouseMap, nil)
	p := []byte(strings.Repeat("filler "+e+"[?1000;1003h", 200))
	for i := 0; i < 4; i++ { // warm up the reused buffers
		if _, err := r.Write(p); err != nil {
			t.Fatal(err)
		}
	}
	got := testing.AllocsPerRun(50, func() {
		if _, err := r.Write(p); err != nil {
			t.Fatal(err)
		}
	})
	if got > 0 {
		t.Fatalf("allocations per Write: got %v, want 0", got)
	}
}

func TestTraceCallback(t *testing.T) {
	type rec struct {
		set       bool
		orig, new string
	}
	var got []rec
	join := func(v []int) string {
		var b strings.Builder
		for i, n := range v {
			if i > 0 {
				b.WriteByte(';')
			}
			b.WriteString(itoa(n))
		}
		return b.String()
	}
	r := NewRewriter(io.Discard, mouseMap, func(set bool, orig, rewritten []int) {
		got = append(got, rec{set, join(orig), join(rewritten)})
	})
	in := e + "[?1003h" + e + "[?1000;1003l" + e + "[?1002h" + e + "[?1003$p"
	if _, err := r.Write([]byte(in)); err != nil {
		t.Fatal(err)
	}
	if err := r.Flush(); err != nil {
		t.Fatal(err)
	}
	want := []rec{
		{true, "1003", "1002"},
		{false, "1000;1003", "1000;1002"},
		{true, "1002", "1002"},
	}
	if len(got) != len(want) {
		t.Fatalf("traced %d sequences, want %d: %+v", len(got), len(want), got)
	}
	for i := range want {
		if got[i] != want[i] {
			t.Fatalf("trace[%d]: got %+v, want %+v", i, got[i], want[i])
		}
	}
}

func itoa(n int) string {
	if n == 0 {
		return "0"
	}
	var b [12]byte
	i := len(b)
	for n > 0 {
		i--
		b[i] = byte('0' + n%10)
		n /= 10
	}
	return string(b[i:])
}

// TestRewriteRandomSplits builds streams out of complete, single-parameter
// sequences and non-ESC filler, which is exactly the shape a whole-buffer
// ReplaceAll gets right, and checks the incremental machine against it under
// random chunking.
func TestRewriteRandomSplits(t *testing.T) {
	pool := []string{
		e + "[?1003h", e + "[?1003l",
		e + "[?1000h", e + "[?1006l",
		e + "[?1003$p", e + "[?25l",
		e + "[0m", e + "[H",
	}
	rnd := rand.New(rand.NewSource(1))
	filler := func(n int) string {
		b := make([]byte, n)
		for i := range b {
			b[i] = byte(0x20 + rnd.Intn(0x5f)) // printable, never ESC
		}
		return string(b)
	}

	for iter := 0; iter < 200; iter++ {
		var sb strings.Builder
		for part := 0; part < 1+rnd.Intn(20); part++ {
			if rnd.Intn(2) == 0 {
				sb.WriteString(filler(rnd.Intn(16)))
			} else {
				sb.WriteString(pool[rnd.Intn(len(pool))])
			}
		}
		in := sb.String()

		oracle := bytes.ReplaceAll([]byte(in), []byte(e+"[?1003h"), []byte(e+"[?1002h"))
		oracle = bytes.ReplaceAll(oracle, []byte(e+"[?1003l"), []byte(e+"[?1002l"))

		var chunks []string
		for rest := in; rest != ""; {
			n := 1 + rnd.Intn(len(rest))
			chunks = append(chunks, rest[:n])
			rest = rest[n:]
		}
		got := writeChunks(t, mouseMap, chunks)
		if got != string(oracle) {
			t.Fatalf("iter %d:\n  in %q\n got %q\nwant %q", iter, in, got, oracle)
		}
		if len(got) != len(in) {
			t.Fatalf("iter %d: length changed, %d -> %d", iter, len(in), len(got))
		}
	}
}
