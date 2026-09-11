package main

import (
	"io"
	"strconv"
)

// escByte is the byte that opens every sequence we care about.
const escByte = 0x1b

// paramCap bounds how many parameter bytes we will hold before giving up and
// letting them through verbatim. A real DECSET parameter list is a handful of
// bytes; anything longer is either not a DECSET at all or is not worth the
// unbounded buffer, and holding it would stall the child's output.
const paramCap = 32

// Rewriter states. The machine is deliberately tiny: only the exact shape
// ESC [ ? <digits and semicolons> (h|l) is ever held back.
const (
	stGround = iota // not inside a candidate sequence
	stEsc           // saw ESC
	stCsi           // saw ESC [
	stPriv          // saw ESC [ ? , collecting parameter bytes
)

// TraceFunc is called once per complete DECSET/DECRST private-mode sequence.
// set is true for the DECSET final byte 'h' and false for DECRST 'l'. orig and
// rewritten are the parameter lists before and after mapping; an empty
// parameter is reported as 0, which is what a terminal would default it to.
//
// Both slices are reused between calls, so a callback that needs to keep them
// must copy them.
type TraceFunc func(set bool, orig, rewritten []int)

// Rewriter is an io.Writer that passes bytes through unchanged except for
// DECSET/DECRST private-mode sequences, whose mode numbers it remaps.
type Rewriter struct {
	w     io.Writer
	maps  map[int]int
	trace TraceFunc

	state int
	held  []byte // raw bytes of the candidate sequence, at most 3+paramCap
	out   []byte // released bytes; reused so steady-state Writes do not allocate

	origBuf []int // reused trace scratch
	newBuf  []int
}

// NewRewriter returns a Rewriter writing to w. maps may be nil or empty, which
// makes the Rewriter a pass-through. trace may be nil.
func NewRewriter(w io.Writer, maps map[int]int, trace TraceFunc) *Rewriter {
	return &Rewriter{
		w:     w,
		maps:  maps,
		trace: trace,
		held:  make([]byte, 0, 3+paramCap),
		out:   make([]byte, 0, 64*1024),
	}
}

// Write implements io.Writer. It performs at most one Write on the underlying
// writer per call, and always reports len(p) consumed: bytes held back inside
// an unfinished sequence are the Rewriter's, not the caller's, problem.
func (r *Rewriter) Write(p []byte) (int, error) {
	b := r.feed(p)
	if len(b) > 0 {
		if _, err := r.w.Write(b); err != nil {
			return 0, err
		}
	}
	return len(p), nil
}

// Flush releases any bytes held inside an unfinished sequence. Call it at EOF,
// otherwise a stream ending mid-sequence would silently lose its tail.
func (r *Rewriter) Flush() error {
	if len(r.held) == 0 {
		return nil
	}
	b := r.held
	r.held = r.held[:0]
	r.state = stGround
	_, err := r.w.Write(b)
	return err
}

// feed is the pure transformer: it returns the bytes released by p. The
// returned slice aliases the Rewriter's buffer and is only valid until the
// next call.
func (r *Rewriter) feed(p []byte) []byte {
	r.out = r.out[:0]
	for i := 0; i < len(p); i++ {
		b := p[i]
		switch r.state {
		case stGround:
			if b == escByte {
				r.state = stEsc
				r.held = append(r.held[:0], b)
			} else {
				r.out = append(r.out, b)
			}

		case stEsc:
			if b == '[' {
				r.state = stCsi
				r.held = append(r.held, b)
			} else {
				// Not a CSI after all. Release what we held and look at this
				// byte again from Ground; it may itself be an ESC starting a
				// fresh sequence.
				r.abandon()
				i--
			}

		case stCsi:
			if b == '?' {
				r.state = stPriv
				r.held = append(r.held, b)
			} else {
				r.abandon()
				i--
			}

		case stPriv:
			switch {
			case (b >= '0' && b <= '9') || b == ';':
				if len(r.held)-3 >= paramCap {
					// Over the cap: this is not a sequence we are willing to
					// buffer. Let everything through untouched.
					r.abandon()
					i--
					continue
				}
				r.held = append(r.held, b)
			case b == 'h' || b == 'l':
				r.emit(b)
			default:
				// Intermediate bytes such as '$' (DECRQM) land here, so
				// requests and replies pass through byte for byte.
				r.abandon()
				i--
			}
		}
	}
	return r.out
}

// abandon releases the held bytes verbatim and returns to Ground.
func (r *Rewriter) abandon() {
	r.out = append(r.out, r.held...)
	r.held = r.held[:0]
	r.state = stGround
}

// emit writes the (possibly rewritten) sequence held so far, terminated by
// final, and reports it to the trace callback.
func (r *Rewriter) emit(final byte) {
	params := r.held[3:] // everything after "ESC [ ?"
	r.out = append(r.out, escByte, '[', '?')
	r.origBuf = r.origBuf[:0]
	r.newBuf = r.newBuf[:0]

	start := 0
	for i := 0; i <= len(params); i++ {
		if i != len(params) && params[i] != ';' {
			continue
		}
		part := params[start:i]
		if start != 0 {
			r.out = append(r.out, ';')
		}
		n, ok := parseMode(part)
		r.origBuf = append(r.origBuf, n)
		if to, hit := r.maps[n]; ok && hit {
			r.out = strconv.AppendInt(r.out, int64(to), 10)
			r.newBuf = append(r.newBuf, to)
		} else {
			// Empty or unmapped: keep the caller's own spelling, so a leading
			// zero or an empty slot survives untouched.
			r.out = append(r.out, part...)
			r.newBuf = append(r.newBuf, n)
		}
		start = i + 1
	}
	r.out = append(r.out, final)

	if r.trace != nil {
		r.trace(final == 'h', r.origBuf, r.newBuf)
	}
	r.held = r.held[:0]
	r.state = stGround
}

// parseMode parses a decimal parameter. An empty parameter is 0 (the terminal
// default) but reports ok=false so it is never treated as a mappable mode.
func parseMode(b []byte) (int, bool) {
	if len(b) == 0 {
		return 0, false
	}
	n := 0
	for _, c := range b {
		if c < '0' || c > '9' {
			return 0, false
		}
		n = n*10 + int(c-'0')
		if n > 1<<20 {
			// Absurd for a private mode; do not map, do not overflow.
			return 0, false
		}
	}
	return n, true
}
