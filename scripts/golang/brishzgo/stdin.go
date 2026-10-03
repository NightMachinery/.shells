package main

import (
	"bytes"
	"fmt"
	"io"
	"os"
	"sync"
)

// replayLimit is how much of the stdin a request read is kept in memory
// for a fallback; past it, the copy moves to a temp file.
//
// Two fallbacks resend it. A garden that refused the request
// (X-Brish-Refused: 1) read all of it first. A garden without the API asked
// for (the streaming or the raw API) normally reads none: such a request
// sends `Expect: 100-continue` and `Connection: close` when it streams our
// stdin, so the 404 comes before any of stdin is sent, and Go's transport
// then never sends it. Some may still go out past the transport's wait for
// a 100 Continue, or to a server that asks for the body before answering
// 404, such as a proxy that buffers requests.
var replayLimit = 16 << 20

// stdinSource is the command's stdin: brishz_in itself, or with
// brishz_in=MAGIC_READ_STDIN our own stdin, streamed.
//
// Each request that streams it (the streaming API's, then the raw API's
// after a 404) reads it through its own reader, which replays what earlier
// requests read and then reads on, keeping a copy. The transport reads a
// request's body on its own goroutine, which can still be reading after the
// reply arrived, while the next request reads on ours. So every read holds
// mu; a reader whose request is no longer the latest gets io.EOF, and so
// does every reader once the JSON API has taken over (stopped).
type stdinSource struct {
	magic   bool
	literal []byte
	r       io.Reader

	mu      sync.Mutex
	pass    int  // the latest request's reader
	stopped bool // the JSON API has taken over
	eof     bool // r is exhausted, so it is not read again
	// What the requests read of r, for the next one: in seen, or past
	// replayLimit all of it in spill.
	seen    bytes.Buffer
	spill   *os.File
	kept    int64
	keepErr error // the copy failed, so stdin cannot be sent again
}

func newStdinSource(cfg config, r io.Reader) *stdinSource {
	return &stdinSource{magic: cfg.stdinMagic, literal: cfg.stdinLiteral, r: r}
}

// reader returns our stdin for one more request: what earlier requests read
// (from the copy), then the rest, copied as it is read. The readers of
// earlier requests get io.EOF from now on.
func (s *stdinSource) reader() (io.Reader, error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	if s.keepErr != nil {
		return nil, fmt.Errorf("could not keep the stdin an earlier request read: %w", s.keepErr)
	}
	s.pass++
	return &stdinReader{s: s, pass: s.pass}, nil
}

// stdinReader is one request's view of stdinSource.
type stdinReader struct {
	s    *stdinSource
	pass int
	off  int64 // how much of stdin this request has read
}

func (r *stdinReader) Read(p []byte) (int, error) {
	s := r.s
	s.mu.Lock()
	defer s.mu.Unlock()
	if s.stopped || r.pass != s.pass {
		return 0, io.EOF
	}
	if r.off < s.kept {
		n, err := s.copyAt(p, r.off)
		r.off += int64(n)
		return n, err
	}
	if s.eof {
		return 0, io.EOF
	}
	n, err := s.r.Read(p)
	if n > 0 {
		s.keep(p[:n])
		r.off += int64(n)
	}
	if err == io.EOF {
		s.eof = true
	}
	return n, err
}

// copyAt copies the kept stdin from offset off into p.
func (s *stdinSource) copyAt(p []byte, off int64) (int, error) {
	if s.spill == nil {
		return copy(p, s.seen.Bytes()[off:]), nil
	}
	if left := s.kept - off; int64(len(p)) > left {
		p = p[:left]
	}
	n, err := s.spill.ReadAt(p, off)
	if err == io.EOF && n == len(p) {
		err = nil
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

// takeOver ends the requests' reading, for the JSON API.
func (s *stdinSource) takeOver() error {
	s.stopped = true
	if s.keepErr != nil {
		return fmt.Errorf("could not keep the stdin an earlier request read: %w", s.keepErr)
	}
	return nil
}

// all returns the whole of stdin, for the JSON API: what earlier requests
// read, then the rest. It ends their reading.
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
// `< file { ... }`: what earlier requests read, then the rest, streamed. It
// ends their reading, and returns the file's path and size.
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

// alreadyRead is how much of stdin earlier requests read, and whether its
// copy is in a temp file, for debug output.
func (s *stdinSource) alreadyRead() (int64, bool) {
	s.mu.Lock()
	defer s.mu.Unlock()
	return s.kept, s.spill != nil
}
