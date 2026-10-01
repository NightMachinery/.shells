package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"strings"
	"testing"
	"time"
)

// slowStdin is a pipe that a goroutine fills with numbered lines, pausing
// between them, so a reader sees them arrive over time. It returns the
// reader and everything that will be written.
func slowStdin(lines int, pause time.Duration) (io.Reader, []byte) {
	var want bytes.Buffer
	for i := 1; i <= lines; i++ {
		fmt.Fprintf(&want, "%06d\n", i)
	}
	pr, pw := io.Pipe()
	data := want.Bytes()
	go func() {
		for i := 0; i < len(data); i += 7 {
			pw.Write(data[i : i+7])
			time.Sleep(pause)
		}
		pw.Close()
	}()
	return pr, data
}

// TestFallbackKeepsStdinOrder: a garden without the raw API answers 404
// while the transport is already streaming stdin (here because its wait for
// a 100 Continue is cut to nothing). The fallback must still send all of
// stdin, in order, while the transport may go on reading. Run it with
// -race too: the transport's reads and the fallback's share stdinSource.
func TestFallbackKeepsStdinOrder(t *testing.T) {
	old := expectContinueTimeout
	expectContinueTimeout = time.Millisecond
	defer func() { expectContinueTimeout = old }()

	g := newFakeGardenLazy(t, func(w http.ResponseWriter, r *http.Request, body func() []byte) {
		if strings.HasPrefix(r.URL.Path, "/zsh/raw/") {
			// Answer without reading the body, as uvicorn does for an
			// unknown route, after some of stdin went out.
			time.Sleep(20 * time.Millisecond)
			http.NotFound(w, r)
			return
		}
		var req map[string]any
		json.Unmarshal(body(), &req)
		d, _ := json.Marshal(map[string]any{"retcode": 0, "out": requestStdin(req), "err": ""})
		w.Write(d)
	})
	for i := 0; i < 5; i++ {
		stdin, want := slowStdin(200, 200*time.Microsecond)
		var out, errb bytes.Buffer
		env := envOf("bshEndpoint", g.URL, "brishz_in", "MAGIC_READ_STDIN", "brishz_debug", "y")
		code := run([]string{"cat"}, env, "/x", t.TempDir(), stdin, &out, &errb)
		if code != 0 || !bytes.Equal(out.Bytes(), want) {
			t.Fatalf("run %d: exit %d, got %d bytes, starting %q\n%s", i, code, out.Len(), trunc(out.String()), errb.String())
		}
	}
}

// TestFallbackSendsNoStdinBefore404: with the garden's 404 arriving before
// the wait for a 100 Continue is over, the raw request sends none of stdin.
func TestFallbackSendsNoStdinBefore404(t *testing.T) {
	g := jsonGarden(t, echoReply)
	stdin, want := slowStdin(50, 100*time.Microsecond)
	var out, errb bytes.Buffer
	env := envOf("bshEndpoint", g.URL, "brishz_in", "MAGIC_READ_STDIN", "brishz_debug", "y")
	code := run([]string{"cat"}, env, "/x", t.TempDir(), stdin, &out, &errb)
	if code != 4 || !bytes.Equal(out.Bytes(), want) {
		t.Fatalf("exit %d, got %q", code, trunc(out.String()))
	}
	if !strings.Contains(errb.String(), fmt.Sprintf("stdin: %d bytes, 0 of them already read", len(want))) {
		t.Errorf("the raw request read stdin before the 404:\n%s", errb.String())
	}
}
