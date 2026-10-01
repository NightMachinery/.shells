package main

import (
	"bytes"
	"encoding/base64"
	"encoding/json"
	"fmt"
	"net/http"
	"os"
	"os/exec"
	"reflect"
	"strconv"
	"strings"
	"sync"
	"testing"
)

// TestParityWithBrishzq sends a corpus of argv lists through brishzq.zsh and
// through brishzgo to a recording fake garden, and compares what each sent:
//
//   - the command's exact bytes: brishzq.zsh with brishz_binary=y (its
//     cmd_b64), against brishzgo's raw request and its brishz_binary=y one;
//   - brishzq.zsh's JSON text request, against brishzgo's with brishz_raw=n:
//     every field and their order, so invalid UTF-8 must become the same
//     U+FFFD; with MAGIC_READ_STDIN, the temp file's path aside, and the
//     file must hold stdin exactly.
//
// Point BRISHZGO_TEST_BRISHZQ at brishzq.zsh to run it:
//
//	BRISHZGO_TEST_BRISHZQ=$NIGHTDIR/zshlang/wrappers/brishz/brishzq.zsh go test -run Parity
func TestParityWithBrishzq(t *testing.T) {
	brishzq := os.Getenv("BRISHZGO_TEST_BRISHZQ")
	if brishzq == "" {
		t.Skip("BRISHZGO_TEST_BRISHZQ is not set")
	}
	zsh := needZsh(t)

	type parityReq struct {
		path   string
		header http.Header
		body   []byte
		stdin  []byte // the temp file the command reads, if any
	}
	var mu sync.Mutex
	var reqs []parityReq
	g := newFakeGarden(t, func(w http.ResponseWriter, r *http.Request, body []byte) {
		pr := parityReq{r.URL.Path, r.Header.Clone(), body, nil}
		var req map[string]any
		json.Unmarshal(body, &req)
		if cmd, ok := req["cmd"].(string); ok {
			if m := stdinFileRe.FindStringSubmatch(cmd); m != nil {
				pr.stdin, _ = os.ReadFile(m[1])
			}
		}
		mu.Lock()
		reqs = append(reqs, pr)
		mu.Unlock()
		w.Header().Set("X-Brish-Binary", "1")
		if strings.HasPrefix(r.URL.Path, "/zsh/raw/") {
			rawReply(w, "", "", 0, "1")
			return
		}
		w.Write([]byte(`{"retcode":0,"out":"","err":"","out_b64":"","err_b64":""}`))
	})
	last := func() parityReq {
		mu.Lock()
		defer mu.Unlock()
		if len(reqs) == 0 {
			t.Fatalf("no request recorded")
		}
		return reqs[len(reqs)-1]
	}
	port := g.URL[strings.LastIndex(g.URL, ":")+1:]

	corpus := [][]string{
		{"print", "-r", "--", "ok"},
		{"echo", "a b", "it's", "", "$HOME", "`x`", "\\", "\"q\""},
		{"cat"},
		{"ec", "a\nb\n", "\t", "~", "=x", "*"},
		{"a.b/c,d:e@f%g+h-i_j", "x"},
		{"a b", "c"},
		{"~x"},
		{"=print"},
		{"x!y"},
		{"!", "false"},
		{"é"},
		{"line\nbreak"},
		{"\xff\xfe", "\x01\x7f"},
		{"print", "--", "a\xe2\x82b", "\xed\xa0\x80", "\xc0\xaf"},
		{},
		{""},
		{"print", "--", strings.Repeat("long ", 2000)},
	}
	envs := [][]string{
		{},
		{"NIGHT_EMACS_P", "y", "emacs_night_server_name", "srv name's"},
		{"NIGHT_EMACS_P", "1", "EMACS_SOCKET_NAME", "/tmp/s"},
		{"brishz_noquote", "y"},
		{"brishz_session", "s 1", "brishz_nolog", "y", "brishz_failure_expected", "n", "brishz_in", "lit\xffin"},
	}
	endpoints := map[string]string{
		"local":     "http://127.0.0.1:" + port,
		"localhost": "http://localhost:" + port,
	}
	magicStdin := []byte("a\x00b\xff\r\n\n")

	dir := t.TempDir()
	home := t.TempDir()
	runZq := func(args []string, stdin []byte, kv ...string) parityReq {
		cmd := exec.Command(zsh, append([]string{"-f", brishzq}, args...)...)
		cmd.Dir = dir
		cmd.Env = []string{"PATH=" + os.Getenv("PATH"), "HOME=" + home, "PWD=" + dir, "LC_ALL=en_US.UTF-8"}
		for i := 0; i+1 < len(kv); i += 2 {
			cmd.Env = append(cmd.Env, kv[i]+"="+kv[i+1])
		}
		cmd.Stdin = bytes.NewReader(stdin)
		mu.Lock()
		reqs = nil
		mu.Unlock()
		cmd.Run()
		mu.Lock()
		n := len(reqs)
		mu.Unlock()
		if n != 1 {
			t.Fatalf("brishzq.zsh %q %q sent %d requests", kv, args, n)
		}
		return last()
	}
	runGo := func(args []string, stdin []byte, kv ...string) parityReq {
		var out, errb bytes.Buffer
		run(args, envOf(kv...), dir, home, bytes.NewReader(stdin), &out, &errb)
		return last()
	}

	n, pending := 0, 0
	for epName, ep := range endpoints {
		for _, extra := range envs {
			for _, args := range corpus {
				kv := append([]string{"bshEndpoint", ep}, extra...)

				// The command's exact bytes.
				zq := runZq(args, nil, append(kv, "brishz_binary", "y")...)
				want := b64Field(t, zq.body, "cmd_b64")
				for _, raw := range []string{"n", "y"} {
					n++
					var got []byte
					if raw == "y" {
						r := runGo(args, nil, append(kv, "brishz_raw", "y")...)
						l, _ := strconv.Atoi(r.header.Get("X-Brish-Cmd-Length"))
						got = r.body[:l]
					} else {
						got = b64Field(t, runGo(args, nil, append(kv, "brishz_binary", "y")...).body, "cmd_b64")
					}
					if bytes.Equal(got, want) {
						continue
					}
					if bangPending(args, got, want) {
						pending++
						continue
					}
					t.Errorf("%s %q %q raw=%s:\n got %q\nwant %q", epName, extra, args, raw, got, want)
				}

				// The JSON text request, with and without MAGIC_READ_STDIN.
				for _, magic := range []bool{false, true} {
					if magic && (len(args) == 0 || (args[0] != "cat" && args[0] != "print")) {
						continue
					}
					var stdin []byte
					kv2 := kv
					if magic {
						stdin = magicStdin
						kv2 = append(append([]string{}, kv...), "brishz_in", "MAGIC_READ_STDIN")
					}
					n++
					zq := runZq(args, stdin, kv2...)
					gr := runGo(args, stdin, append(kv2, "brishz_raw", "n")...)
					zf, zk := jsonFields(t, zq.body)
					gf, gk := jsonFields(t, gr.body)
					if magic {
						for _, f := range []map[string]any{zf, gf} {
							if c, ok := f["cmd"].(string); ok {
								f["cmd"] = stdinFileRe.ReplaceAllString(c, "< 'FILE' {\n")
							}
						}
						if !bytes.Equal(zq.stdin, magicStdin) || !bytes.Equal(gr.stdin, magicStdin) {
							t.Errorf("%s %q %q: stdin files: brishzq.zsh %q, brishzgo %q", epName, extra, args, zq.stdin, gr.stdin)
						}
					}
					if reflect.DeepEqual(zf, gf) && reflect.DeepEqual(zk, gk) {
						continue
					}
					if bangPending(args, []byte(fmt.Sprint(gf["cmd"])), []byte(fmt.Sprint(zf["cmd"]))) {
						pending++
						continue
					}
					t.Errorf("%s %q %q magic=%v:\n got %v %q\nwant %v %q", epName, extra, args, magic, gk, gf, zk, zf)
				}
			}
		}
	}
	t.Logf("%d cases compared", n)
	if pending > 0 {
		t.Logf("%d cases differ only in keeping a first word of ! bare, which this brishzq.zsh does not do yet", pending)
	}
}

// bangPending reports whether got and want differ only in that brishzgo
// keeps a first word of `!` bare and brishzq.zsh quotes it.
func bangPending(args []string, got, want []byte) bool {
	return len(args) > 0 && args[0] == "!" && bytes.Equal(got, bytes.ReplaceAll(want, []byte("'!'"), []byte("!")))
}

func b64Field(t *testing.T, body []byte, name string) []byte {
	t.Helper()
	s := requestField(t, body, name)
	b, err := base64.StdEncoding.DecodeString(strings.Join(strings.Fields(s), ""))
	if err != nil {
		t.Fatalf("%s: %v", name, err)
	}
	return b
}

// jsonFields decodes a JSON object, and also returns its keys in order.
func jsonFields(t *testing.T, body []byte) (map[string]any, []string) {
	t.Helper()
	var m map[string]any
	if err := json.Unmarshal(body, &m); err != nil {
		t.Fatalf("request %q: %v", body, err)
	}
	dec := json.NewDecoder(bytes.NewReader(body))
	dec.Token() // {
	var keys []string
	for dec.More() {
		k, _ := dec.Token()
		keys = append(keys, k.(string))
		var v any
		dec.Decode(&v)
	}
	return m, keys
}
