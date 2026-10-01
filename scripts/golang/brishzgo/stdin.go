package main

import (
	"bytes"
	"fmt"
	"io"
	"os"
	"sync"
)

// replayLimit is how much of the stdin a raw request read is kept in
// memory for a fallback; past it, the copy moves to a temp file.
//
// Two fallbacks resend it. A garden that refused the raw request
// (X-Brish-Refused: 1) read all of it first. A garden without the raw API
// normally reads none: the raw request sends `Expect: 100-continue` and
// `Connection: close` when it streams our stdin, so the 404 comes before
// any of stdin is sent, and Go's transport then never sends it. Some may
// still go out past the transport's wait for a 100 Continue, or to a server
// that asks for the body before answering 404, such as a proxy that
// buffers requests.
var replayLimit = 16 << 20

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
	// What the raw request read of r, for a fallback: in seen, or past
	// replayLimit all of it in spill.
	seen    bytes.Buffer
	spill   *os.File
	kept    int64
	keepErr error // the copy failed, so a fallback cannot resend stdin
}

func newStdinSource(cfg config, r io.Reader) *stdinSource {
	return &stdinSource{magic: cfg.stdinMagic, literal: cfg.stdinLiteral, r: r}
}

// Read streams our stdin to the raw request, keeping a copy.
func (s *stdinSource) Read(p []byte) (int, error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	if s.stopped || s.eof {
		return 0, io.EOF
	}
	n, err := s.r.Read(p)
	if n > 0 {
		s.keep(p[:n])
	}
	if err == io.EOF {
		s.eof = true
	}
	return n, err
}

func (s *stdinSource) keep(b []byte) {
	if s.keepErr != nil {
		return
	}
	s.kept += int64(len(b))
	if s.spill == nil && s.seen.Len()+len(b) <= replayLimit {
		s.seen.Write(b)
		return
	}
	if s.spill == nil {
		f, err := temps.create("brishzgo-stdin.")
		if err != nil {
			s.keepErr = err
			s.seen = bytes.Buffer{}
			return
		}
		s.spill = f
		_, s.keepErr = f.Write(s.seen.Bytes())
		s.seen = bytes.Buffer{}
	}
	if s.keepErr == nil {
		_, s.keepErr = s.spill.Write(b)
	}
}

// takeOver ends the raw request's reading, for a fallback.
func (s *stdinSource) takeOver() error {
	s.stopped = true
	if s.keepErr != nil {
		return fmt.Errorf("could not keep the stdin the raw request read: %w", s.keepErr)
	}
	return nil
}

// all returns the whole of stdin, for the JSON API: what the raw request
// read, then the rest. It ends the raw request's reading.
func (s *stdinSource) all() ([]byte, error) {
	if !s.magic {
		return s.literal, nil
	}
	s.mu.Lock()
	defer s.mu.Unlock()
	if err := s.takeOver(); err != nil {
		return nil, err
	}
	var data bytes.Buffer
	if s.spill != nil {
		if _, err := s.spill.Seek(0, io.SeekStart); err != nil {
			return nil, err
		}
		if _, err := data.ReadFrom(s.spill); err != nil {
			return nil, err
		}
	} else {
		data.Write(s.seen.Bytes())
	}
	if !s.eof {
		if _, err := data.ReadFrom(s.r); err != nil {
			return nil, err
		}
		s.eof = true
	}
	return data.Bytes(), nil
}

// toFile writes the whole of stdin to a temp file, for the JSON API's
// `< file { ... }`: what the raw request read, then the rest, streamed. It
// ends the raw request's reading, and returns the file's path and size.
func (s *stdinSource) toFile() (string, int64, error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	if err := s.takeOver(); err != nil {
		return "", 0, err
	}
	f := s.spill
	size := s.kept
	if f == nil {
		var err error
		if f, err = temps.create("brishzgo-stdin."); err != nil {
			return "", 0, err
		}
		if _, err := f.Write(s.seen.Bytes()); err != nil {
			f.Close()
			return "", 0, err
		}
	}
	var err error
	if !s.eof {
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

// alreadyRead is how much of stdin the raw request read, and whether its
// copy is in a temp file, for debug output.
func (s *stdinSource) alreadyRead() (int64, bool) {
	s.mu.Lock()
	defer s.mu.Unlock()
	return s.kept, s.spill != nil
}
