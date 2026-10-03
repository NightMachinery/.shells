package main

import (
	"bufio"
	"bytes"
	"context"
	"encoding/binary"
	"errors"
	"fmt"
	"io"
	"net/http"
	"strconv"
	"strings"
	"sync/atomic"
)

// The streaming API's reply body is a sequence of frames: a type byte, the
// payload's length as 4 bytes big-endian, then the payload. It ends with
// exactly one exit frame, whose payload is the retcode in ASCII decimal.
const (
	frameStdout = 1
	frameStderr = 2
	frameExit   = 3

	frameHeaderLen = 5
	// An exit frame's payload is a number; anything longer is not one.
	maxExitPayload = 32
)

var (
	// errCutShort: the body ended before its exit frame, as when the
	// garden died mid-reply. What arrived has been written.
	errCutShort = errors.New("the reply ended before its exit frame")
	// errBadExit: an exit frame whose payload is not a number.
	errBadExit = errors.New("an exit frame without a retcode")
)

// writeError is a failure to write our own output, as opposed to one
// reading the reply. (A closed pipe on stdout or stderr never gets here:
// Go's runtime kills us with SIGPIPE, which closes the connection.)
type writeError struct{ err error }

func (e writeError) Error() string { return "write: " + e.err.Error() }
func (e writeError) Unwrap() error { return e.err }

type tagWriter struct{ w io.Writer }

func (t tagWriter) Write(p []byte) (int, error) {
	n, err := t.w.Write(p)
	if err != nil {
		err = writeError{err}
	}
	return n, err
}

// readFrames copies a streaming reply's frames to out and errOut as they
// arrive, and returns the exit frame's retcode. Each piece of a payload is
// written as soon as it is read, with no buffer of ours in between, so
// output appears as the command makes it. A frame of an unknown type is
// skipped, for a later garden's additions.
func readFrames(r io.Reader, out, errOut io.Writer) (int, error) {
	br := bufio.NewReaderSize(r, 64<<10)
	out, errOut = tagWriter{out}, tagWriter{errOut}
	var hdr [frameHeaderLen]byte
	for {
		if _, err := io.ReadFull(br, hdr[:]); err != nil {
			return 0, cutShort(err)
		}
		n := int64(binary.BigEndian.Uint32(hdr[1:]))
		var dst io.Writer
		switch hdr[0] {
		case frameExit:
			if n > maxExitPayload {
				return 0, errBadExit
			}
			p := make([]byte, n)
			if _, err := io.ReadFull(br, p); err != nil {
				return 0, cutShort(err)
			}
			rc, err := strconv.Atoi(string(p))
			if err != nil {
				return 0, errBadExit
			}
			return rc, nil
		case frameStdout:
			dst = out
		case frameStderr:
			dst = errOut
		default:
			dst = io.Discard
		}
		if _, err := io.CopyN(dst, br, n); err != nil {
			return 0, cutShort(err)
		}
	}
}

func cutShort(err error) error {
	if err == io.EOF || err == io.ErrUnexpectedEOF {
		return errCutShort
	}
	return err
}

// fallback is where a request goes when its API could not run it.
type fallback int

const (
	noFallback   fallback = iota
	fallbackRaw           // no streaming API: the raw API next
	fallbackJSON          // refused: the JSON API next, as for the raw API
)

// stream runs the command through POST /zsh/stream/, writing its stdout and
// stderr as they arrive. The fallback is set when nothing ran: the garden
// has no streaming API (HTTP 404 or 405) or refused the request
// (X-Brish-Refused: 1).
//
// SIGHUP, SIGINT or SIGTERM while it runs closes the connection, which
// makes the garden kill the command, and then kills us with the same signal
// (see sigHandler). This is the one transport where interrupting brishzgo
// stops the command: the other APIs run it to its end whatever we do.
func (c *client) stream(in *stdinSource) (int, fallback) {
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	var interrupted atomic.Bool
	remove := sigs.add(func() {
		interrupted.Store(true)
		c.debugf("signal: closing the connection, so the garden kills the command")
		cancel()
	})
	defer remove()
	// The signal handler kills us right after it closed the connection;
	// exiting with a status of our own would race with it.
	dying := func() {
		if interrupted.Load() {
			awaitSignalDeath()
		}
	}

	req, code := c.rawRequest("stream/", in)
	if req == nil {
		return code, noFallback
	}
	resp, code := c.do(req.WithContext(ctx))
	if resp == nil {
		dying()
		return code, noFallback
	}
	defer resp.Body.Close()

	switch {
	case resp.StatusCode == http.StatusNotFound || resp.StatusCode == http.StatusMethodNotAllowed:
		c.fallbackWhy = fmt.Sprintf("no streaming API (HTTP %d)", resp.StatusCode)
		return 0, fallbackRaw
	case resp.StatusCode >= 400:
		return exitHTTPError, noFallback
	}
	h := resp.Header
	if strings.TrimSpace(h.Get("X-Brish-Refused")) == "1" {
		// Nothing ran; see raw for why the JSON API can still run it. The
		// raw API would refuse it the same way.
		var msg bytes.Buffer
		readFrames(io.LimitReader(resp.Body, 4096), io.Discard, &msg)
		c.fallbackWhy = fmt.Sprintf("the streaming request was refused (%q)", msg.Bytes())
		return 0, fallbackJSON
	}
	if strings.TrimSpace(h.Get("X-Brish-Stream")) != "1" {
		// Not a streaming reply at all.
		data, err := io.ReadAll(resp.Body)
		if err != nil {
			dying()
			return curlExitCode(err, true), noFallback
		}
		return c.printNotice(data), noFallback
	}
	if h.Get("X-Brish-Notice") == "1" {
		// A magic command's log, in a stdout frame: printed as the other
		// APIs' notices are.
		var notice bytes.Buffer
		_, err := readFrames(resp.Body, &notice, c.stderr)
		if err != nil {
			dying()
			return c.streamExit(err), noFallback
		}
		return c.printNotice(notice.Bytes()), noFallback
	}

	// X-Brish-Binary: 0 is a legacy-mode garden; its output is printed all
	// the same, as for the raw API.
	rc, err := readFrames(resp.Body, c.stdout, c.stderr)
	if err != nil {
		dying()
		return c.streamExit(err), noFallback
	}
	return rc, noFallback
}

// streamExit is the exit status for a reply that failed midway, as curl
// would give it.
func (c *client) streamExit(err error) int {
	c.debugf("stream: %v", err)
	var we writeError
	switch {
	case errors.Is(err, errCutShort):
		return curlPartial
	case errors.Is(err, errBadExit):
		return curlWeirdServerReply
	case errors.As(err, &we):
		return curlWrite
	}
	return curlExitCode(err, true)
}
