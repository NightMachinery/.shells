package main

import (
	"bytes"
	"errors"
	"io"
)

// replayLimit bounds how much streamed stdin is kept for a fallback.
//
// The raw request sends `Expect: 100-continue` when it streams our stdin, so
// a garden without the raw API answers 404 before any of stdin is sent, and
// the fallback gets all of it. The copy is for a server that reads the body
// anyway before answering 404, such as a proxy that buffers requests. Past
// this limit the copy is dropped, and such a fallback fails.
var replayLimit = 16 << 20

var errStdinGone = errors.New("stdin was already sent to a garden without the raw API")

// stdinSource is the command's stdin: brishz_in itself, or with
// brishz_in=MAGIC_READ_STDIN our own stdin, streamed.
type stdinSource struct {
	magic   bool
	literal []byte
	r       io.Reader

	// What the raw request read of r, for a fallback.
	seen     bytes.Buffer
	overflow bool
}

func newStdinSource(cfg config, r io.Reader) *stdinSource {
	return &stdinSource{magic: cfg.stdinMagic, literal: cfg.stdinLiteral, r: r}
}

// Read streams our stdin, keeping a copy of up to replayLimit bytes.
func (s *stdinSource) Read(p []byte) (int, error) {
	n, err := s.r.Read(p)
	if n > 0 && !s.overflow {
		if s.seen.Len()+n > replayLimit {
			s.overflow = true
			s.seen = bytes.Buffer{}
		} else {
			s.seen.Write(p[:n])
		}
	}
	return n, err
}

// all returns the whole of stdin, for the JSON API: what the raw request
// read, then the rest.
func (s *stdinSource) all() ([]byte, error) {
	if !s.magic {
		return s.literal, nil
	}
	if s.overflow {
		return nil, errStdinGone
	}
	rest, err := io.ReadAll(s.r)
	if err != nil {
		return nil, err
	}
	return append(s.seen.Bytes(), rest...), nil
}
