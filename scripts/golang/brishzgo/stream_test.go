package main

import (
	"bufio"
	"bytes"
	"encoding/binary"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"math/rand"
	"net/http"
	"os"
	"os/exec"
	"strconv"
	"strings"
	"syscall"
	"testing"
	"testing/iotest"
	"time"
)

func frameOf(kind byte, payload []byte) []byte {
	b := make([]byte, frameHeaderLen, frameHeaderLen+len(payload))
	b[0] = kind
	binary.BigEndian.PutUint32(b[1:], uint32(len(payload)))
	return append(b, payload...)
}

func exitFrameOf(rc int) []byte { return frameOf(frameExit, []byte(strconv.Itoa(rc))) }

// framesOf joins frames into a reply body.
func framesOf(frames ...[]byte) []byte { return bytes.Join(frames, nil) }

// refFrames is a reference decoder for readFrames: it parses the whole
// body at once, and returns what readFrames should have written, the
// retcode and the error.
func refFrames(body []byte) (out, errOut []byte, rc int, err error) {
	for {
		if len(body) < frameHeaderLen {
			return out, errOut, 0, errCutShort
		}
		kind := body[0]
		n := int(binary.BigEndian.Uint32(body[1:frameHeaderLen]))
		body = body[frameHeaderLen:]
		if kind == frameExit {
			if n > maxExitPayload {
				return out, errOut, 0, errBadExit
			}
			if len(body) < n {
				return out, errOut, 0, errCutShort
			}
			rc, err := strconv.Atoi(string(body[:n]))
			if err != nil {
				return out, errOut, 0, errBadExit
			}
			return out, errOut, rc, nil
		}
		m := min(n, len(body))
		switch kind {
		case frameStdout:
			out = append(out, body[:m]...)
		case frameStderr:
			errOut = append(errOut, body[:m]...)
		}
		if m < n {
			return out, errOut, 0, errCutShort
		}
		body = body[n:]
	}
}

// splitReader returns body in reads that end at the given offsets.
type splitReader struct {
	body  []byte
	cuts  []int
	start int
}

func (s *splitReader) Read(p []byte) (int, error) {
	if s.start >= len(s.body) {
		return 0, io.EOF
	}
	end := len(s.body)
	for len(s.cuts) > 0 {
		if c := s.cuts[0]; c > s.start {
			end = min(end, c)
			break
		}
		s.cuts = s.cuts[1:]
	}
	n := copy(p, s.body[s.start:end])
	s.start += n
	return n, nil
}

func checkFrames(t *testing.T, name string, r io.Reader, body []byte) {
	t.Helper()
	var out, errOut bytes.Buffer
	rc, err := readFrames(r, &out, &errOut)
	wOut, wErr, wRC, wErrV := refFrames(body)
	if !bytes.Equal(out.Bytes(), wOut) || !bytes.Equal(errOut.Bytes(), wErr) || rc != wRC || !errors.Is(err, wErrV) && err != wErrV {
		t.Fatalf("%s: got (%q, %q, %d, %v), want (%q, %q, %d, %v)", name,
			trunc(out.String()), trunc(errOut.String()), rc, err, trunc(string(wOut)), trunc(string(wErr)), wRC, wErrV)
	}
}

func sampleBody() []byte {
	big := make([]byte, 100<<10)
	rand.New(rand.NewSource(1)).Read(big)
	return framesOf(
		frameOf(frameStdout, []byte("out\n")),
		frameOf(frameStderr, []byte("err\x00\r\n")),
		frameOf(frameStdout, nil),
		frameOf(9, []byte("a later garden's frame")),
		frameOf(frameStdout, big),
		frameOf(frameStderr, []byte{0xff, 0xfe}),
		exitFrameOf(300),
	)
}

// TestReadFramesSplits: a reply split into reads at every offset, or one
// byte at a time, decodes the same; one cut short at every offset writes
// what arrived and fails with errCutShort.
func TestReadFramesSplits(t *testing.T) {
	body := sampleBody()
	var out, errOut bytes.Buffer
	rc, err := readFrames(bytes.NewReader(body), &out, &errOut)
	if rc != 300 || err != nil || out.Len() != 4+100<<10 || errOut.String() != "err\x00\r\n\xff\xfe" {
		t.Fatalf("whole: %d %v %d %q", rc, err, out.Len(), errOut.String())
	}
	checkFrames(t, "one byte at a time", iotest.OneByteReader(bytes.NewReader(body)), body)
	checkFrames(t, "half-empty reads", iotest.HalfReader(bytes.NewReader(body)), body)
	small := framesOf(
		frameOf(frameStdout, []byte("out\n")),
		frameOf(frameStderr, []byte("err")),
		frameOf(7, []byte("x")),
		frameOf(frameStdout, nil),
		exitFrameOf(3),
	)
	for i := 0; i <= len(small); i++ {
		checkFrames(t, fmt.Sprintf("split at %d", i), &splitReader{body: small, cuts: []int{i}}, small)
		checkFrames(t, fmt.Sprintf("cut short at %d", i), bytes.NewReader(small[:i]), small[:i])
	}
	for i := 0; i < len(body); i += 997 {
		checkFrames(t, fmt.Sprintf("big, split at %d", i), &splitReader{body: body, cuts: []int{i, i + 1, i + 5}}, body)
		checkFrames(t, fmt.Sprintf("big, cut short at %d", i), bytes.NewReader(body[:i]), body[:i])
	}
}

func TestReadFramesBadExit(t *testing.T) {
	for name, body := range map[string][]byte{
		"not a number": frameOf(frameExit, []byte("abc")),
		"empty":        frameOf(frameExit, nil),
		"too long":     frameOf(frameExit, bytes.Repeat([]byte("1"), maxExitPayload+1)),
	} {
		if _, err := readFrames(bytes.NewReader(body), io.Discard, io.Discard); err != errBadExit {
			t.Errorf("%s: %v", name, err)
		}
	}
	// Whatever follows the exit frame is not read.
	body := framesOf(exitFrameOf(-1), frameOf(frameStdout, []byte("after")))
	var out bytes.Buffer
	if rc, err := readFrames(bytes.NewReader(body), &out, io.Discard); rc != -1 || err != nil || out.Len() != 0 {
		t.Errorf("after exit: %d %v %q", rc, err, out.String())
	}
}

// TestReadFramesProgressive: a frame is written before the next one
// arrives, and a payload's first piece before its last.
func TestReadFramesProgressive(t *testing.T) {
	pr, pw := io.Pipe()
	got := make(chan string, 10)
	done := make(chan error, 1)
	go func() {
		_, err := readFrames(pr, writerFunc(func(p []byte) (int, error) {
			got <- string(p)
			return len(p), nil
		}), io.Discard)
		done <- err
	}()
	expect := func(want string) {
		t.Helper()
		select {
		case s := <-got:
			if s != want {
				t.Fatalf("got %q, want %q", s, want)
			}
		case <-time.After(5 * time.Second):
			t.Fatalf("%q was not written while the reply went on", want)
		}
	}
	pw.Write(frameOf(frameStdout, []byte("first")))
	expect("first")
	whole := frameOf(frameStdout, []byte("second half"))
	pw.Write(whole[:frameHeaderLen+7])
	expect("second ")
	pw.Write(whole[frameHeaderLen+7:])
	expect("half")
	pw.Write(exitFrameOf(0))
	if err := <-done; err != nil {
		t.Fatal(err)
	}
}

type writerFunc func(p []byte) (int, error)

func (f writerFunc) Write(p []byte) (int, error) { return f(p) }

// FuzzReadFrames compares readFrames with refFrames on any body, read in
// any pieces.
func FuzzReadFrames(f *testing.F) {
	f.Add(sampleBody()[:200], int64(1))
	f.Add(framesOf(frameOf(frameStdout, []byte("a")), exitFrameOf(0)), int64(2))
	f.Add(frameOf(frameExit, []byte("+7")), int64(3))
	f.Add([]byte{1, 0, 0, 0, 9, 'x'}, int64(4))
	f.Fuzz(func(t *testing.T, body []byte, seed int64) {
		rng := rand.New(rand.NewSource(seed))
		var cuts []int
		for i := 0; i < len(body); i += 1 + rng.Intn(8) {
			cuts = append(cuts, i)
		}
		checkFrames(t, "fuzz", &splitReader{body: body, cuts: cuts}, body)
	})
}

// streamGarden is a fake garden with the streaming API: reply gets the
// request's command and stdin and writes the frames.
func streamGarden(t *testing.T, reply func(w http.ResponseWriter, cmd, stdin []byte)) *fakeGarden {
	return newFakeGarden(t, func(w http.ResponseWriter, r *http.Request, body []byte) {
		if !strings.HasPrefix(r.URL.Path, "/zsh/stream/") {
			t.Errorf("request to %s", r.URL.Path)
			http.NotFound(w, r)
			return
		}
		n, _ := strconv.Atoi(r.Header.Get("X-Brish-Cmd-Length"))
		w.Header().Set("Content-Type", "application/octet-stream")
		w.Header().Set("X-Brish-Binary", "1")
		w.Header().Set("X-Brish-Stream", "1")
		reply(w, body[:n], body[n:])
	})
}

func TestStreamRoundTrip(t *testing.T) {
	g := streamGarden(t, func(w http.ResponseWriter, cmd, stdin []byte) {
		w.Write(frameOf(frameStdout, stdin))
		w.Write(frameOf(frameStderr, cmd))
		w.Write(exitFrameOf(300))
	})
	stdin := "a\x00b\xff\r\n"
	got := runWith(t, g, stdin, []string{"cat"}, "brishz_in", "MAGIC_READ_STDIN", "brishz_stream", "y",
		"brishz_session", "s1", "brishz_nolog", "y", "brishz_failure_expected", "y")
	if got.code != 300 || got.out != stdin || !strings.Contains(got.errOut, "cat") {
		t.Errorf("got %+v", got)
	}
	r := g.reqs[0]
	if len(g.reqs) != 1 || r.path != "/zsh/stream/nolog/" || r.query != "failure_expected=1&nolog=1&session=s1" ||
		r.header.Get("Expect") != "100-continue" {
		t.Errorf("request: %+v", g.reqs)
	}
	// A literal brishz_in has a length, and needs no 100 Continue.
	g.reqs = nil
	got = runWith(t, g, "", []string{"cat"}, "brishz_in", "lit", "brishz_stream", "1")
	if got.code != 300 || got.out != "lit" || g.reqs[0].path != "/zsh/stream/" || g.reqs[0].header.Get("Expect") != "" {
		t.Errorf("literal: got %+v, %+v", got, g.reqs)
	}
	// brishz_stream is off by default, and with a false value.
	for _, v := range []string{"", "n", "0", "no"} {
		gr := newFakeGarden(t, func(w http.ResponseWriter, r *http.Request, body []byte) {
			rawReply(w, "", "", 0, "1")
		})
		runWith(t, gr, "", []string{"true"}, "brishz_stream", v)
		if len(gr.reqs) != 1 || gr.reqs[0].path != "/zsh/raw/" {
			t.Errorf("brishz_stream=%q: %+v", v, gr.reqs)
		}
	}
}

// TestStreamProgressive: output reaches our stdout while the command still
// runs.
func TestStreamProgressive(t *testing.T) {
	release := make(chan struct{})
	g := streamGarden(t, func(w http.ResponseWriter, cmd, stdin []byte) {
		w.Write(frameOf(frameStdout, []byte("early\n")))
		w.(http.Flusher).Flush()
		select {
		case <-release:
		case <-time.After(10 * time.Second):
		}
		w.Write(frameOf(frameStdout, []byte("late\n")))
		w.Write(exitFrameOf(0))
	})
	first := make(chan string, 1)
	var all bytes.Buffer
	out := writerFunc(func(p []byte) (int, error) {
		if all.Len() == 0 {
			first <- string(p)
		}
		return all.Write(p)
	})
	done := make(chan int, 1)
	go func() {
		var errb bytes.Buffer
		done <- run([]string{"true"}, envOf("bshEndpoint", g.URL, "brishz_stream", "y"), "/x", t.TempDir(), strings.NewReader(""), out, &errb)
	}()
	select {
	case s := <-first:
		if s != "early\n" {
			t.Errorf("first write %q", s)
		}
	case <-time.After(5 * time.Second):
		t.Fatal("no output while the command ran")
	}
	close(release)
	if code := <-done; code != 0 || all.String() != "early\nlate\n" {
		t.Errorf("exit %d, out %q", code, all.String())
	}
}

func TestStreamNotices(t *testing.T) {
	g := newFakeGarden(t, func(w http.ResponseWriter, r *http.Request, body []byte) {
		w.Header().Set("X-Brish-Stream", "1")
		w.Header().Set("X-Brish-Notice", "1")
		w.Write(frameOf(frameStdout, []byte("Empty command received.")))
		w.Write(exitFrameOf(0))
	})
	if got := runWith(t, g, "", []string{"true"}, "brishz_stream", "y"); got.code != 200 || got.out != "Empty command received.\n" {
		t.Errorf("notice: %+v", got)
	}
	// A 200 that is not a streaming reply is printed as a notice too.
	g = newFakeGarden(t, func(w http.ResponseWriter, r *http.Request, body []byte) {
		io.WriteString(w, "something else\n\n")
	})
	if got := runWith(t, g, "", []string{"true"}, "brishz_stream", "y"); got.code != 200 || got.out != "something else\n" || len(g.reqs) != 1 {
		t.Errorf("not a stream: %+v", got)
	}
}

// TestStreamFallbacks: a garden without the streaming API (404 or 405)
// gets the raw request, or with brishz_raw=n the JSON one; a refusal goes
// to the JSON API at once. Each resends all of stdin, even when the
// request before read it all.
func TestStreamFallbacks(t *testing.T) {
	for _, c := range []struct {
		name      string
		stream    int // the stream route's answer: 404, 405, or 200 refused
		raw       int // the raw route's: 404, or 200
		rawOpt    string
		readFirst bool // the 404s read the body first
		paths     string
	}{
		{"404, raw", 404, 200, "", false, "/zsh/stream/ /zsh/raw/"},
		{"405, raw", 405, 200, "", true, "/zsh/stream/ /zsh/raw/"},
		{"404, no raw, json", 404, 404, "", true, "/zsh/stream/ /zsh/raw/ /zsh/"},
		{"404, brishz_raw=n, json", 404, 200, "n", false, "/zsh/stream/ /zsh/"},
		{"refused, json", 200, 200, "", true, "/zsh/stream/ /zsh/"},
	} {
		g := newFakeGardenLazy(t, func(w http.ResponseWriter, r *http.Request, body func() []byte) {
			switch {
			case strings.HasPrefix(r.URL.Path, "/zsh/stream/"):
				if c.readFirst || c.stream == 200 {
					body()
				}
				if c.stream != 200 {
					w.WriteHeader(c.stream)
					return
				}
				w.Header().Set("X-Brish-Stream", "1")
				w.Header().Set("X-Brish-Refused", "1")
				w.Write(frameOf(frameStderr, []byte("brishgarden: refused\n")))
				w.Write(exitFrameOf(9000))
			case strings.HasPrefix(r.URL.Path, "/zsh/raw/"):
				b := body()
				if c.raw != 200 {
					w.WriteHeader(c.raw)
					return
				}
				n, _ := strconv.Atoi(r.Header.Get("X-Brish-Cmd-Length"))
				rawReply(w, string(b[n:]), "", 5, "1")
			default:
				var req map[string]any
				json.Unmarshal(body(), &req)
				_, d := echoReply(req)
				io.WriteString(w, d)
			}
		})
		stdin := strings.Repeat("stdin ", 1000)
		got := runWith(t, g, stdin, []string{"cat"}, "brishz_in", "MAGIC_READ_STDIN", "brishz_stream", "y",
			"brishz_raw", c.rawOpt, "brishz_debug", "y")
		var paths []string
		for _, r := range g.reqs {
			paths = append(paths, r.path)
		}
		wantCode := 5
		if strings.HasSuffix(c.paths, "/zsh/") {
			wantCode = 4
		}
		if got.code != wantCode || got.out != stdin || strings.Join(paths, " ") != c.paths {
			t.Errorf("%s: exit %d, out %q, requests %v\n%s", c.name, got.code, trunc(got.out), paths, got.errOut)
		}
	}
}

// TestStreamFailures: a reply cut short exits 18 after writing what
// arrived, a bad exit frame 8, an HTTP error 22, and a failed write of our
// own output 23.
func TestStreamFailures(t *testing.T) {
	g := streamGarden(t, func(w http.ResponseWriter, cmd, stdin []byte) {
		switch string(cmd) {
		case "cut":
			w.Write(frameOf(frameStdout, []byte("so far")))
			w.Write(frameOf(frameStdout, []byte("half"))[:7])
		case "noexit":
			w.Write(frameOf(frameStdout, []byte("all")))
		case "bad":
			w.Write(frameOf(frameExit, []byte("x")))
		case "http":
			w.WriteHeader(500)
		default:
			w.Write(frameOf(frameStdout, []byte("out")))
			w.Write(exitFrameOf(0))
		}
	})
	for cmd, want := range map[string]result{
		"cut":    {18, "so farha", ""},
		"noexit": {18, "all", ""},
		"bad":    {8, "", ""},
		"http":   {22, "", ""},
	} {
		got := runWith(t, g, "", []string{cmd}, "brishz_stream", "y", "brishz_noquote", "y")
		if got != want {
			t.Errorf("%s: got %+v, want %+v", cmd, got, want)
		}
	}
	var errb bytes.Buffer
	failing := writerFunc(func(p []byte) (int, error) { return 0, errors.New("disk full") })
	if code := run([]string{"ok"}, envOf("bshEndpoint", g.URL, "brishz_stream", "y", "brishz_noquote", "y"),
		"/x", t.TempDir(), strings.NewReader(""), failing, &errb); code != 23 {
		t.Errorf("write error: exit %d", code)
	}
}

// TestStreamBinaryOptIn: brishz_binary=y takes the JSON API even with
// brishz_stream=y, since only that API runs nothing on a garden without
// binary mode.
func TestStreamBinaryOptIn(t *testing.T) {
	g := jsonGarden(t, echoReply)
	got := runWith(t, g, "", []string{"true"}, "brishz_stream", "y", "brishz_binary", "y", "brishz_debug", "y")
	if len(g.reqs) != 1 || g.reqs[0].path != "/zsh/" || !strings.Contains(got.errOut, "brishz_stream=y does nothing") {
		t.Errorf("got %+v, requests %+v", got, g.reqs)
	}
}

func TestStreamKeyRedacted(t *testing.T) {
	home := t.TempDir()
	os.MkdirAll(home+"/.keys", 0o700)
	os.WriteFile(home+"/.keys/brishgarden", []byte("X-Synthetic-Key: not-a-real-key\n"), 0o600)
	g := streamGarden(t, func(w http.ResponseWriter, cmd, stdin []byte) {
		w.Write(exitFrameOf(0))
	})
	ep := strings.Replace(g.URL, "127.0.0.1", "localhost", 1)
	var out, errb bytes.Buffer
	code := run([]string{"true"}, envOf("bshEndpoint", ep, "brishz_stream", "y", "brishz_debug", "y"), "/x", home, strings.NewReader(""), &out, &errb)
	if code != 0 || g.reqs[0].header.Get("X-Synthetic-Key") != "not-a-real-key" {
		t.Fatalf("exit %d, %+v", code, g.reqs)
	}
	if strings.Contains(out.String()+errb.String(), "not-a-real-key") || !strings.Contains(errb.String(), "<redacted>") {
		t.Errorf("the key leaked, or was not redacted:\n%s", errb.String())
	}
}

// TestStreamSignals runs the built binary against a fake garden whose
// command runs until the client goes away. SIGINT, SIGTERM or SIGHUP mid
// stream closes the connection (so the garden kills the command) and kills
// us with the same signal; a SIGINT ignored at start stays ignored.
func TestStreamSignals(t *testing.T) {
	bin := builtBinary(t)
	gone := make(chan struct{}, 10)
	g := newFakeGarden(t, func(w http.ResponseWriter, r *http.Request, body []byte) {
		w.Header().Set("X-Brish-Stream", "1")
		w.Write(frameOf(frameStdout, []byte("started\n")))
		w.(http.Flusher).Flush()
		select {
		case <-r.Context().Done():
			gone <- struct{}{}
		case <-time.After(3 * time.Second):
			w.Write(frameOf(frameStdout, []byte("ran to its end\n")))
			w.Write(exitFrameOf(0))
		}
	})
	start := func(wrap ...string) (*exec.Cmd, *bufio.Reader) {
		t.Helper()
		args := append(wrap, bin, "true")
		cmd := exec.Command(args[0], args[1:]...)
		cmd.Env = []string{"PATH=" + os.Getenv("PATH"), "HOME=" + t.TempDir(), "bshEndpoint=" + g.URL, "brishz_stream=y"}
		stdout, err := cmd.StdoutPipe()
		if err != nil {
			t.Fatal(err)
		}
		if err := cmd.Start(); err != nil {
			t.Fatal(err)
		}
		br := bufio.NewReader(stdout)
		if line, err := br.ReadString('\n'); line != "started\n" {
			t.Fatalf("first line %q, %v", line, err)
		}
		return cmd, br
	}
	// The signals' default handling, whatever this test inherited (a
	// background job may have SIGINT ignored, which exec keeps).
	dfl := []string{"perl", "-e", `$SIG{$_} = "DEFAULT" for qw(INT TERM HUP); exec @ARGV or die`}
	for _, sig := range []syscall.Signal{syscall.SIGINT, syscall.SIGTERM, syscall.SIGHUP} {
		cmd, br := start(dfl...)
		cmd.Process.Signal(sig)
		rest, _ := io.ReadAll(br)
		cmd.Wait()
		ws := cmd.ProcessState.Sys().(syscall.WaitStatus)
		if !ws.Signaled() || ws.Signal() != sig || len(rest) != 0 {
			t.Errorf("%v: %v, rest %q", sig, cmd.ProcessState, rest)
		}
		select {
		case <-gone:
		case <-time.After(2 * time.Second):
			t.Errorf("%v: the garden did not see the connection close", sig)
		}
	}
	// `trap '' INT` as a non-interactive shell's background job has it.
	cmd, br := start(append(dfl, "sh", "-c", `trap '' INT; exec "$@"`, "sh")...)
	time.Sleep(100 * time.Millisecond)
	cmd.Process.Signal(syscall.SIGINT)
	rest, _ := io.ReadAll(br)
	if err := cmd.Wait(); err != nil || string(rest) != "ran to its end\n" {
		t.Errorf("ignored SIGINT: %v, rest %q", err, rest)
	}
}

// TestStreamSignalStalledStderr: with brishz_debug=y and our stderr on a
// full pipe that nobody reads, a SIGINT still closes the connection at once
// and kills us with SIGINT. The signal's cleanup closes the connection
// before it writes its debug line, and does not wait long for that write.
func TestStreamSignalStalledStderr(t *testing.T) {
	bin := builtBinary(t)
	gone := make(chan time.Time, 1)
	g := newFakeGarden(t, func(w http.ResponseWriter, r *http.Request, body []byte) {
		w.Header().Set("X-Brish-Stream", "1")
		w.Write(frameOf(frameStdout, []byte("started\n")))
		w.(http.Flusher).Flush()
		// More stderr than a pipe holds: the client blocks writing it,
		// and this write blocks once the socket buffers are full.
		w.Write(frameOf(frameStderr, bytes.Repeat([]byte("e"), 4<<20)))
		select {
		case <-r.Context().Done():
			gone <- time.Now()
		case <-time.After(20 * time.Second):
		}
	})
	stderrR, stderrW, err := os.Pipe()
	if err != nil {
		t.Fatal(err)
	}
	defer stderrR.Close()
	dfl := []string{"perl", "-e", `$SIG{$_} = "DEFAULT" for qw(INT TERM HUP); exec @ARGV or die`, bin, "true"}
	cmd := exec.Command(dfl[0], dfl[1:]...)
	cmd.Env = []string{"PATH=" + os.Getenv("PATH"), "HOME=" + t.TempDir(), "bshEndpoint=" + g.URL, "brishz_stream=y", "brishz_debug=y"}
	cmd.Stderr = stderrW
	stdout, err := cmd.StdoutPipe()
	if err != nil {
		t.Fatal(err)
	}
	if err := cmd.Start(); err != nil {
		t.Fatal(err)
	}
	stderrW.Close()
	br := bufio.NewReader(stdout)
	if line, err := br.ReadString('\n'); line != "started\n" {
		t.Fatalf("first line %q, %v", line, err)
	}
	// By now the client is blocked on its stderr.
	time.Sleep(300 * time.Millisecond)
	t0 := time.Now()
	cmd.Process.Signal(syscall.SIGINT)
	exited := make(chan struct{})
	go func() {
		io.Copy(io.Discard, br)
		cmd.Wait()
		close(exited)
	}()
	select {
	case <-exited:
	case <-time.After(5 * time.Second):
		cmd.Process.Kill()
		<-exited
		t.Fatal("still alive 5 s after SIGINT")
	}
	ws := cmd.ProcessState.Sys().(syscall.WaitStatus)
	if !ws.Signaled() || ws.Signal() != syscall.SIGINT {
		t.Errorf("not killed by SIGINT: %v", cmd.ProcessState)
	}
	select {
	case at := <-gone:
		if d := at.Sub(t0); d > time.Second {
			t.Errorf("the garden saw the connection close %v after the SIGINT", d)
		}
	case <-time.After(2 * time.Second):
		t.Error("the garden did not see the connection close")
	}
}
