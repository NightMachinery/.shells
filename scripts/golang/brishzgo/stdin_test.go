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
		env := envOf("brishz_stream", "n", "bshEndpoint", g.URL, "brishz_in", "MAGIC_READ_STDIN", "brishz_debug", "y")
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
	env := envOf("brishz_stream", "n", "bshEndpoint", g.URL, "brishz_in", "MAGIC_READ_STDIN", "brishz_debug", "y")
	code := run([]string{"cat"}, env, "/x", t.TempDir(), stdin, &out, &errb)
	if code != 4 || !bytes.Equal(out.Bytes(), want) {
		t.Fatalf("exit %d, got %q", code, trunc(out.String()))
	}
	if !strings.Contains(errb.String(), fmt.Sprintf("stdin: %d bytes, 0 of them already read", len(want))) {
		t.Errorf("the raw request read stdin before the 404:\n%s", errb.String())
	}
}

// TestStdinPasses: each request reads all of stdin through its own reader,
// the later ones replaying what the earlier ones read (from memory, or past
// replayLimit from the temp file) and then reading on. An earlier
// request's reader gets io.EOF once a later one exists, and every reader
// does once the JSON API took over.
func TestStdinPasses(t *testing.T) {
	stdin := strings.Repeat("0123456789", 1000)
	for _, limit := range []int{1 << 20, 1000} {
		old := replayLimit
		replayLimit = limit
		src := newStdinSource(config{stdinMagic: true}, strings.NewReader(stdin))
		r1, _ := src.reader()
		buf := make([]byte, 3000)
		if n, err := io.ReadFull(r1, buf); n != 3000 || err != nil {
			t.Fatalf("limit %d: pass 1 read %d, %v", limit, n, err)
		}
		r2, _ := src.reader()
		if n, err := r1.Read(buf); n != 0 || err != io.EOF {
			t.Errorf("limit %d: an old pass read %d, %v", limit, n, err)
		}
		// Small reads, so a read of the copy ends exactly at its end.
		var got bytes.Buffer
		small := make([]byte, 7)
		for got.Len() < 5000 {
			n, err := r2.Read(small)
			got.Write(small[:n])
			if err != nil {
				t.Fatalf("limit %d: pass 2: %v", limit, err)
			}
		}
		r3, _ := src.reader()
		all, err := io.ReadAll(r3)
		if err != nil || string(all) != stdin {
			t.Errorf("limit %d: pass 3 got %d bytes, %v", limit, len(all), err)
		}
		if read, spilled := src.alreadyRead(); read != int64(len(stdin)) || spilled != (limit == 1000) {
			t.Errorf("limit %d: read %d, spilled %v", limit, read, spilled)
		}
		r4, _ := src.reader()
		if data, err := src.all(); err != nil || string(data) != stdin {
			t.Errorf("limit %d: all: %v, %d bytes", limit, err, len(data))
		}
		if n, err := r4.Read(buf); n != 0 || err != io.EOF {
			t.Errorf("limit %d: a pass after the JSON API took over read %d, %v", limit, n, err)
		}
		replayLimit = old
		temps.removeAll()
	}
}
