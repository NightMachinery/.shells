package main

import (
	"bytes"
	"encoding/base64"
	"encoding/json"
	"net/http"
	"os"
	"os/exec"
	"strconv"
	"strings"
	"testing"
)

// TestParityWithBrishzq sends a corpus of argv lists through brishzq.zsh and
// through brishzgo to a recording fake garden, and compares the command
// text each one sent. brishzq.zsh runs with brishz_binary=y, so its
// cmd_b64 holds the command's exact bytes. Point BRISHZGO_TEST_BRISHZQ at
// brishzq.zsh to run it:
//
//	BRISHZGO_TEST_BRISHZQ=$NIGHTDIR/zshlang/wrappers/brishz/brishzq.zsh go test -run Parity
func TestParityWithBrishzq(t *testing.T) {
	brishzq := os.Getenv("BRISHZGO_TEST_BRISHZQ")
	if brishzq == "" {
		t.Skip("BRISHZGO_TEST_BRISHZQ is not set")
	}
	zsh := needZsh(t)

	g := newFakeGarden(t, func(w http.ResponseWriter, r *http.Request, body []byte) {
		w.Header().Set("X-Brish-Binary", "1")
		if strings.HasPrefix(r.URL.Path, "/zsh/raw/") {
			rawReply(w, "", "", 0, "1")
			return
		}
		w.Write([]byte(`{"retcode":0,"out_b64":"","err_b64":""}`))
	})
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
		{"é"},
		{"line\nbreak"},
		{"\xff\xfe", "\x01\x7f"},
		{},
		{""},
		{"print", "--", strings.Repeat("long ", 2000)},
	}
	envs := [][]string{
		{},
		{"NIGHT_EMACS_P", "y", "emacs_night_server_name", "srv name's"},
		{"NIGHT_EMACS_P", "1", "EMACS_SOCKET_NAME", "/tmp/s"},
		{"brishz_noquote", "y"},
	}
	endpoints := map[string]string{
		"local":     "http://127.0.0.1:" + port,
		"localhost": "http://localhost:" + port,
	}

	dir := t.TempDir()
	home := t.TempDir()
	n := 0
	firstWords := map[string]bool{}
	for epName, ep := range endpoints {
		for _, extra := range envs {
			for _, args := range corpus {
				n++
				kv := append([]string{"bshEndpoint", ep, "brishz_binary", "y"}, extra...)

				// brishzq.zsh
				g.mu.Lock()
				g.reqs = nil
				g.mu.Unlock()
				cmd := exec.Command(zsh, append([]string{"-f", brishzq}, args...)...)
				cmd.Dir = dir
				cmd.Env = []string{"PATH=" + os.Getenv("PATH"), "HOME=" + home, "PWD=" + dir, "LC_ALL=en_US.UTF-8"}
				for i := 0; i+1 < len(kv); i += 2 {
					cmd.Env = append(cmd.Env, kv[i]+"="+kv[i+1])
				}
				cmd.Stdin = strings.NewReader("")
				cmd.Run()
				if len(g.reqs) != 1 {
					t.Fatalf("brishzq.zsh sent %d requests", len(g.reqs))
				}
				var req struct {
					CmdB64 string `json:"cmd_b64"`
				}
				if err := json.Unmarshal(g.reqs[0].body, &req); err != nil {
					t.Fatalf("brishzq.zsh request: %v", err)
				}
				want, _ := base64.StdEncoding.DecodeString(strings.Join(strings.Fields(req.CmdB64), ""))

				for _, raw := range []string{"n", "y"} {
					g.reqs = nil
					var out, errb bytes.Buffer
					// raw=n: brishz_binary=y, so the JSON request has cmd_b64.
					// raw=y: without it, since brishz_binary=y never uses the
					// raw API.
					goKV := append([]string{"bshEndpoint", ep, "brishz_raw", raw}, extra...)
					if raw == "n" {
						goKV = append(goKV, "brishz_binary", "y")
					}
					env := envOf(goKV...)
					run(args, env, dir, home, strings.NewReader(""), &out, &errb)
					var got []byte
					r := g.reqs[len(g.reqs)-1]
					if raw == "y" {
						l, _ := strconv.Atoi(r.header.Get("X-Brish-Cmd-Length"))
						got = r.body[:l]
					} else {
						json.Unmarshal(r.body, &req)
						got, _ = base64.StdEncoding.DecodeString(req.CmdB64)
					}
					if bytes.Equal(got, want) {
						continue
					}
					if len(args) > 0 && firstWordDiffers(t, zsh, args[0]) {
						firstWords[args[0]] = true
						continue
					}
					t.Errorf("%s %q %q raw=%s:\n got %q\nwant %q", epName, extra, args, raw, got, want)
				}
			}
		}
	}
	t.Logf("%d cases compared", n)
	if len(firstWords) > 0 {
		t.Logf("first words that brishzq.zsh quotes with (q+), so otherwise: %v", firstWords)
	}
}

// firstWordDiffers reports whether zsh's ${(q+)w}, which brishzq.zsh uses
// for the first word, quotes w otherwise than quoteFirstWord.
func firstWordDiffers(t *testing.T, zsh, w string) bool {
	out, err := exec.Command(zsh, "-fc", `print -rn -- "${(q+)1}"`, "zsh", w).Output()
	if err != nil {
		t.Fatalf("zsh: %v", err)
	}
	return string(out) != quoteFirstWord(w)
}
