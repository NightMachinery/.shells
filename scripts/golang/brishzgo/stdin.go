package main

import (
	"bytes"
	"errors"
	"io"
	"sync"
)

// replayLimit bounds how much streamed stdin is kept for a fallback.
//
// The raw request sends `Expect: 100-continue` and `Connection: close` when
// it streams our stdin, so a garden without the raw API answers 404 before
// any of stdin is sent, and Go's transport then never sends it. Some may
// still be sent: past the transport's wait for a 100 Continue, or to a
// server that asks for the body before answering 404, such as a proxy that
// buffers requests. The copy is for those. Past this limit the copy is
// dropped, and such a fallback fails.
var replayLimit = 16 << 20

var errStdinGone = errors.New("stdin was already sent to a garden without the raw API")

// stdinSource is the command's stdin: brishz_in itself, or with
// brishz_in=MAGIC_READ_STDIN our own stdin, streamed.
//
// The raw request reads it on the HTTP transport's goroutine, which can
// still be reading after the reply arrived, while a fallback reads the rest
// on ours. So every read of r holds mu and keeps what it read, in order;
// once a fallback has taken over (stopped), the transport gets io.EOF and
// never reads r again.
type stdinSource struct {
	magic   bool
	literal []byte
	r       io.Reader

	mu      sync.Mutex
	stopped bool // a fallback has taken over
	eof     bool // r is exhausted, so it is not read again
	// What the raw request read of r, for a fallback.
	seen     bytes.Buffer
	overflow bool
}

func newStdinSource(cfg config, r io.Reader) *stdinSource {
	return &stdinSource{magic: cfg.stdinMagic, literal: cfg.stdinLiteral, r: r}
}

// Read streams our stdin to the raw request, keeping a copy of up to
// replayLimit bytes.
func (s *stdinSource) Read(p []byte) (int, error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	if s.stopped || s.eof {
		return 0, io.EOF
	}
	n, err := s.r.Read(p)
	if n > 0 && !s.overflow {
		if s.seen.Len()+n > replayLimit {
			s.overflow = true
			s.seen = bytes.Buffer{}
		} else {
			s.seen.Write(p[:n])
		}
	}
	if err == io.EOF {
		s.eof = true
	}
	return n, err
}

// all returns the whole of stdin, for the JSON API: what the raw request
// read, then the rest. It ends the raw request's reading.
func (s *stdinSource) all() ([]byte, error) {
	if !s.magic {
		return s.literal, nil
	}
	s.mu.Lock()
	defer s.mu.Unlock()
	s.stopped = true
	if s.overflow {
		return nil, errStdinGone
	}
	var rest []byte
	if !s.eof {
		var err error
		if rest, err = io.ReadAll(s.r); err != nil {
			return nil, err
		}
		s.eof = true
	}
	data := make([]byte, 0, s.seen.Len()+len(rest))
	return append(append(data, s.seen.Bytes()...), rest...), nil
}

// toFile writes the whole of stdin to a new temp file, for the JSON API's
// `< file { ... }`: what the raw request read, then the rest, streamed. It
// ends the raw request's reading, and returns the file's path and size.
func (s *stdinSource) toFile() (string, int64, error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.stopped = true
	if s.overflow {
		return "", 0, errStdinGone
	}
	f, err := temps.create("brishzgo-stdin.")
	if err != nil {
		return "", 0, err
	}
	n, err := f.Write(s.seen.Bytes())
	size := int64(n)
	if err == nil && !s.eof {
		var m int64
		m, err = io.Copy(f, s.r)
		size += m
		s.eof = err == nil
	}
	if cerr := f.Close(); err == nil {
		err = cerr
	}
	return f.Name(), size, err
}

// alreadyRead is how much of stdin the raw request read, for debug output.
func (s *stdinSource) alreadyRead() int {
	s.mu.Lock()
	defer s.mu.Unlock()
	return s.seen.Len()
}
