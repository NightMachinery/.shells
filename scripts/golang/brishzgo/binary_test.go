package main

import (
	"bytes"
	"encoding/base64"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"os"
	"os/exec"
	"strconv"
	"strings"
	"sync/atomic"
	"syscall"
	"testing"
	"time"
)

// gardenGen is a garden generation, as brishz_binary=y meets it.
type gardenGen string

const (
	// Binary mode, with binary=1 (8571467 on): it changes nothing.
	genBinary gardenGen = "binary"
	// Binary mode, older than binary=1, which it ignores: exact anyway.
	genOldBinary gardenGen = "old binary"
	// Legacy mode (BRISH_BINARY=0) from 8571467 on: binary=1 and the JSON
	// API's binary: 1 are refused, X-Brish-Refused: 1, nothing runs.
	genLegacy gardenGen = "legacy"
	// Legacy mode, older than binary=1: it ignores the option and runs the
	// command in text mode (X-Brish-Binary: 0, no refusal). Its JSON API
	// refuses binary: 1.
	genOldLegacy gardenGen = "old legacy"
	// No raw or streaming API (404 or 405), the JSON API's binary
	// transport, binary mode (such as 42ddc9d).
	gen404 gardenGen = "404"
	gen405 gardenGen = "405"
	// No raw API and no _b64 fields (such as ec61c63): the JSON request's
	// command, only in cmd_b64, arrives empty.
	genPreBinary gardenGen = "pre-binary"
)

var allGens = []gardenGen{genBinary, genOldBinary, genLegacy, genOldLegacy, gen404, gen405, genPreBinary}

// genGarden is a fake garden of one generation, with its raw, streaming
// and JSON APIs. A command runs by writing "out:" and its stdin to stdout
// and "err\xff" to stderr, and returning 300; an empty command, or one
// that starts with %GARDEN_, is a notice instead. runs counts the commands
// that ran.
type genGarden struct {
	*fakeGarden
	gen  gardenGen
	runs atomic.Int32
}

func newGenGarden(t *testing.T, gen gardenGen) *genGarden {
	g := &genGarden{gen: gen}
	g.fakeGarden = newFakeGarden(t, func(w http.ResponseWriter, r *http.Request, body []byte) {
		if strings.HasPrefix(r.URL.Path, "/zsh/raw/") || strings.HasPrefix(r.URL.Path, "/zsh/stream/") {
			g.serveRaw(w, r, body)
			return
		}
		g.serveJSON(w, body)
	})
	return g
}

const genNotice = "Empty command received."

func (g *genGarden) exec(cmd, stdin []byte) (out, errOut []byte, rc int, notice bool) {
	if len(cmd) == 0 || bytes.HasPrefix(cmd, []byte("%GARDEN_")) {
		return []byte(genNotice), nil, 0, true
	}
	g.runs.Add(1)
	return append([]byte("out:"), stdin...), []byte("err\xff"), 300, false
}

func (g *genGarden) exactP() bool {
	return g.gen == genBinary || g.gen == genOldBinary || g.gen == gen404 || g.gen == gen405
}

func (g *genGarden) serveRaw(w http.ResponseWriter, r *http.Request, body []byte) {
	switch g.gen {
	case gen404, genPreBinary:
		http.NotFound(w, r)
		return
	case gen405:
		w.WriteHeader(http.StatusMethodNotAllowed)
		return
	}
	n, _ := strconv.Atoi(r.Header.Get("X-Brish-Cmd-Length"))
	binaryHeader := "0"
	if g.exactP() {
		binaryHeader = "1"
	}
	h := w.Header()
	h.Set("X-Brish-Binary", binaryHeader)
	var out, errOut []byte
	rc, notice := 9000, false
	if g.gen == genLegacy && r.URL.Query().Get("binary") == "1" {
		h.Set("X-Brish-Refused", "1")
		errOut = []byte("brishgarden: refused\n")
	} else {
		out, errOut, rc, notice = g.exec(body[:n], body[n:])
	}
	if notice {
		h.Set("X-Brish-Notice", "1")
	}
	if !strings.HasPrefix(r.URL.Path, "/zsh/stream/") {
		rawReply(w, string(out), string(errOut), rc, binaryHeader)
		return
	}
	h.Set("X-Brish-Stream", "1")
	if len(out) > 0 {
		w.Write(frameOf(frameStdout, out))
	}
	if len(errOut) > 0 {
		w.Write(frameOf(frameStderr, errOut))
	}
	w.Write(exitFrameOf(rc))
}

func (g *genGarden) serveJSON(w http.ResponseWriter, body []byte) {
	var req map[string]any
	json.Unmarshal(body, &req)
	b64 := base64.StdEncoding
	binaryReq := req["binary"] != nil
	if binaryReq && (g.gen == genLegacy || g.gen == genOldLegacy) {
		// Refused, without X-Brish-Binary.
		io.WriteString(w, `{"retcode":9000,"out":"","err":"brishgarden: binary refused"}`)
		return
	}
	var cmd, stdin []byte
	if s, ok := req["cmd_b64"].(string); ok && g.gen != genPreBinary {
		cmd, _ = b64.DecodeString(s)
		s, _ = req["stdin_b64"].(string)
		stdin, _ = b64.DecodeString(s)
	} else {
		s, _ := req["cmd"].(string)
		cmd = []byte(s)
		stdin = []byte(requestStdin(req))
	}
	out, errOut, rc, notice := g.exec(cmd, stdin)
	exact := binaryReq && g.gen != genPreBinary
	if exact {
		w.Header().Set("X-Brish-Binary", "1")
	}
	if notice {
		io.WriteString(w, string(out)+"\n")
		return
	}
	reply := map[string]any{"retcode": rc}
	if exact {
		reply["out_b64"] = b64.EncodeToString(out)
		reply["err_b64"] = b64.EncodeToString(errOut)
	} else {
		reply["out"], reply["err"] = string(out), string(errOut)
	}
	d, _ := json.Marshal(reply)
	w.Write(d)
}

func (g *genGarden) paths() string {
	var p []string
	for _, r := range g.reqs {
		p = append(p, r.path)
	}
	return strings.Join(p, " ")
}

// TestBinaryOptInGenerations: brishz_binary=y over the raw and streaming
// APIs, against every garden generation, with stdin streamed or literal:
//
//   - exact bytes where the garden has them, from the API asked, or after a
//     404 or 405 from the JSON API's binary transport (never the raw API
//     after the streaming API's 404);
//   - a refusal goes to that transport too, with all of stdin, which a
//     legacy-mode garden refuses again: 201, and nothing ran;
//   - a legacy-mode garden older than binary=1 ran the command in text mode:
//     201, the retcode in the message, and none of its output;
//   - the empty command of a garden without the _b64 fields: 201.
func TestBinaryOptInGenerations(t *testing.T) {
	stdin := "a\x00b\xff\r\n\n"
	for _, api := range []string{"raw", "stream"} {
		for _, gen := range allGens {
			for _, magic := range []bool{true, false} {
				name := fmt.Sprintf("%s, %s, magic %v", api, gen, magic)
				g := newGenGarden(t, gen)
				kv := []string{"brishz_binary", "y", "brishz_noquote", "y", "brishz_stream", map[string]string{"raw": "n", "stream": "y"}[api]}
				pipe := ""
				if magic {
					kv = append(kv, "brishz_in", magicReadStdin)
					pipe = stdin
				} else {
					kv = append(kv, "brishz_in", stdin)
				}
				got := runBufferedWith(t, g.fakeGarden, pipe, []string{"cat"}, kv...)

				first := "/zsh/" + api + "/"
				var want result
				wantPaths, wantRuns := first, int32(1)
				switch gen {
				case genBinary, genOldBinary:
					want = result{300, "out:" + stdin, "err\xff"}
				case gen404, gen405:
					want = result{300, "out:" + stdin, "err\xff"}
					wantPaths += " /zsh/"
				case genLegacy, genPreBinary:
					want = result{201, "", noBinaryMessage + "\n"}
					wantPaths += " /zsh/"
					wantRuns = 0
				case genOldLegacy:
					want = result{201, "", fmt.Sprintf(textModeMessage, textModeRanPhrase, "300")}
				}
				if got != want || g.paths() != wantPaths || g.runs.Load() != wantRuns {
					t.Errorf("%s: got %+v, want %+v; requests %s, ran %d", name, got, want, g.paths(), g.runs.Load())
					continue
				}
				// The first request carries binary=1, the command and stdin
				// as bytes; the JSON fallback all of stdin, as base64, and
				// binary: 1.
				r := g.reqs[0]
				if r.query != "binary=1" || string(r.body) != "cat"+stdin || r.header.Get("X-Brish-Cmd-Length") != "3" {
					t.Errorf("%s: request %s?%s %q", name, r.path, r.query, r.body)
				}
				if len(g.reqs) == 2 {
					var req map[string]any
					json.Unmarshal(g.reqs[1].body, &req)
					if req["binary"] != float64(1) || string(b64Field(t, g.reqs[1].body, "stdin_b64")) != stdin ||
						string(b64Field(t, g.reqs[1].body, "cmd_b64")) != "cat" {
						t.Errorf("%s: JSON fallback %s", name, g.reqs[1].body)
					}
				}
			}
		}
	}
}

// TestBinaryOptInNotices: a notice in an exact reply is printed, with exit
// 200, as without the opt-in; one from a garden that ignored binary=1 is
// withheld, with 201, like any of its replies.
func TestBinaryOptInNotices(t *testing.T) {
	for _, stream := range []string{"n", "y"} {
		for _, gen := range allGens {
			g := newGenGarden(t, gen)
			got := runBufferedWith(t, g.fakeGarden, "", nil, "brishz_binary", "y", "brishz_noquote", "y", "brishz_stream", stream)
			want := result{200, genNotice + "\n", ""}
			switch gen {
			case genOldLegacy:
				want = result{201, "", fmt.Sprintf(textModeMessage, textModeNoticePhrase, "0")}
			case genLegacy, genPreBinary:
				// Refused, then refused by the JSON API; or a JSON API that
				// gets an empty command, the notice without the header.
				want = result{201, "", noBinaryMessage + "\n"}
			}
			if got != want {
				t.Errorf("stream %s, %s: got %+v, want %+v", stream, gen, got, want)
			}
		}
	}
}

// TestBinaryOptInRoutes: without the opt-in nothing carries binary=1;
// brishz_raw=n keeps the opt-in on the JSON API, unless brishz_stream=y,
// whose fallback is that API anyway.
func TestBinaryOptInRoutes(t *testing.T) {
	for _, c := range []struct {
		kv    []string
		paths string
		query string
	}{
		{nil, "/zsh/raw/", ""},
		{[]string{"brishz_stream", "y"}, "/zsh/stream/", ""},
		{[]string{"brishz_binary", "y", "brishz_raw", "n"}, "/zsh/", ""},
		{[]string{"brishz_binary", "y", "brishz_raw", "n", "brishz_stream", "y"}, "/zsh/stream/", "binary=1"},
		{[]string{"brishz_binary", "y", "brishz_session", "s", "brishz_nolog", "y", "brishz_failure_expected", "y"}, "/zsh/raw/nolog/", "binary=1&failure_expected=1&nolog=1&session=s"},
	} {
		g := newGenGarden(t, genBinary)
		got := runBufferedWith(t, g.fakeGarden, "", []string{"true"}, c.kv...)
		if got.code != 300 || g.paths() != c.paths || g.reqs[0].query != c.query {
			t.Errorf("%q: got %+v, requests %s?%s", c.kv, got, g.paths(), g.reqs[0].query)
		}
	}
	// A refusal of the streaming request with brishz_raw=n: the JSON API.
	g := newGenGarden(t, genLegacy)
	got := runBufferedWith(t, g.fakeGarden, "", []string{"true"}, "brishz_binary", "y", "brishz_raw", "n", "brishz_stream", "y")
	if got.code != 201 || g.paths() != "/zsh/stream/ /zsh/" || g.runs.Load() != 0 {
		t.Errorf("refused stream, brishz_raw=n: got %+v, requests %s", got, g.paths())
	}
}

// TestBinaryStreamTextModeDrains: a streaming reply without
// X-Brish-Binary: 1 is read to its exit frame, so the garden writes all of
// it (the command is not killed half way), and none of it is printed. One
// that ends before its exit frame still exits 201, with the retcode
// unknown.
func TestBinaryStreamTextModeDrains(t *testing.T) {
	big := bytes.Repeat([]byte("x"), 16<<20)
	wrote := make(chan error, 1)
	g := newFakeGarden(t, func(w http.ResponseWriter, r *http.Request, body []byte) {
		w.Header().Set("X-Brish-Stream", "1")
		w.Header().Set("X-Brish-Binary", "0")
		w.(http.Flusher).Flush()
		var err error
		if string(body) == "cut" {
			w.Write(frameOf(frameStdout, []byte("half")))
			wrote <- nil
			return
		}
		for _, f := range [][]byte{frameOf(frameStdout, big), frameOf(frameStderr, []byte("e")), exitFrameOf(3)} {
			if _, err = w.Write(f); err != nil {
				break
			}
		}
		wrote <- err
	})
	got := runBufferedWith(t, g, "", []string{"cat"}, "brishz_stream", "y", "brishz_binary", "y", "brishz_noquote", "y")
	if want := (result{201, "", fmt.Sprintf(textModeMessage, textModeRanPhrase, "3")}); got != want {
		t.Errorf("got %+v, want %+v", got, want)
	}
	if err := <-wrote; err != nil {
		t.Errorf("the client went away before the exit frame: %v", err)
	}

	got = runBufferedWith(t, g, "", []string{"cut"}, "brishz_stream", "y", "brishz_binary", "y", "brishz_noquote", "y")
	<-wrote
	if want := (result{201, "", fmt.Sprintf(textModeMessage, textModeRanPhrase, "unknown")}); got != want {
		t.Errorf("cut short: got %+v, want %+v", got, want)
	}
}

// TestBinaryStreamTextModeSignal: SIGINT while the client reads a text-mode
// reply to its end closes the connection and kills it with SIGINT, as for
// any streamed command, with nothing printed.
func TestBinaryStreamTextModeSignal(t *testing.T) {
	bin := builtBinary(t)
	arrived := make(chan struct{}, 1)
	gone := make(chan struct{}, 1)
	g := newFakeGarden(t, func(w http.ResponseWriter, r *http.Request, body []byte) {
		w.Header().Set("X-Brish-Stream", "1")
		w.Header().Set("X-Brish-Binary", "0")
		w.Write(frameOf(frameStdout, []byte("started\n")))
		w.(http.Flusher).Flush()
		arrived <- struct{}{}
		select {
		case <-r.Context().Done():
			gone <- struct{}{}
		case <-time.After(3 * time.Second):
			w.Write(exitFrameOf(0))
		}
	})
	dfl := []string{"perl", "-e", `$SIG{$_} = "DEFAULT" for qw(INT TERM HUP); exec @ARGV or die`, bin, "true"}
	cmd := exec.Command(dfl[0], dfl[1:]...)
	cmd.Env = []string{"PATH=" + os.Getenv("PATH"), "HOME=" + t.TempDir(), "bshEndpoint=" + g.URL, "brishz_stream=y", "brishz_binary=y"}
	var out, errb bytes.Buffer
	cmd.Stdout, cmd.Stderr = &out, &errb
	if err := cmd.Start(); err != nil {
		t.Fatal(err)
	}
	select {
	case <-arrived:
	case <-time.After(5 * time.Second):
		cmd.Process.Kill()
		t.Fatal("no request")
	}
	time.Sleep(100 * time.Millisecond)
	cmd.Process.Signal(syscall.SIGINT)
	cmd.Wait()
	ws := cmd.ProcessState.Sys().(syscall.WaitStatus)
	if !ws.Signaled() || ws.Signal() != syscall.SIGINT || out.Len() != 0 || errb.Len() != 0 {
		t.Errorf("%v, out %q, err %q", cmd.ProcessState, out.String(), errb.String())
	}
	select {
	case <-gone:
	case <-time.After(2 * time.Second):
		t.Error("the garden did not see the connection close")
	}
}
