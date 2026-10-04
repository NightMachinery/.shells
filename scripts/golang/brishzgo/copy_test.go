package main

import (
	"bytes"
	"context"
	"encoding/json"
	"io"
	"net/http"
	"os"
	"os/exec"
	"path/filepath"
	"reflect"
	"strings"
	"testing"
	"time"
)

// TestCopyReplay executes the copied script against a fake garden, never
// the user's clipboard, and compares the full command and input requests.
func TestCopyReplay(t *testing.T) {
	zsh, err := exec.LookPath("zsh")
	if err != nil {
		t.Skip("zsh not installed")
	}
	bin := builtBinary(t)
	for _, mode := range []string{"stream", "raw", "json"} {
		for _, inputMode := range []string{"literal", "stdin"} {
			t.Run(mode+"/"+inputMode, func(t *testing.T) {
				tmp := t.TempDir()
				clip := filepath.Join(tmp, "clipboard")
				mock := "#!/bin/sh\ncommand cat > " + quoteSingle(clip) + "\n"
				if err := os.WriteFile(filepath.Join(tmp, "pbcopy"), []byte(mock), 0o700); err != nil {
					t.Fatal(err)
				}
				g := newFakeGarden(t, func(w http.ResponseWriter, r *http.Request, body []byte) {
					w.Header().Set("X-Brish-Binary", "1")
					if mode == "stream" {
						w.Header().Set("X-Brish-Stream", "1")
						w.Write(frameOf(frameStdout, []byte("out")))
						w.Write(frameOf(frameStderr, []byte("err")))
						w.Write(exitFrameOf(17))
					} else if mode == "raw" {
						rawReply(w, "out", "err", 17, "1")
					} else {
						io.WriteString(w, `{"out_b64":"b3V0","err_b64":"ZXJy","retcode":17}`)
					}
				})
				input := "it's literal\n\n"
				ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
				defer cancel()
				cmd := exec.CommandContext(ctx, bin, "--", "--help", "it's", "$(printf inert)", "", "line\nbreak")
				cmd.Dir = tmp
				path := tmp + string(os.PathListSeparator) + filepath.Dir(bin) + string(os.PathListSeparator) + os.Getenv("PATH")
				cmd.Env = []string{"PATH=" + path, "HOME=" + tmp, "bshEndpoint=" + g.URL,
					"brishz_copy=y", "brishz_binary=y", "brishz_session=s 1", "brishz_nolog=y", "brishz_failure_expected=0",
					"NIGHT_EMACS_P=y", "emacs_night_server_name=srv 1"}
				if inputMode == "stdin" {
					input = strings.Repeat("x", 256<<10) + "\x00\xff\n\n"
					cmd.Stdin = strings.NewReader(input)
					cmd.Env = append(cmd.Env, "brishz_in=MAGIC_READ_STDIN")
				} else {
					cmd.Env = append(cmd.Env, "brishz_in="+input)
				}
				if mode != "stream" {
					cmd.Env = append(cmd.Env, "brishz_stream=n")
				}
				if mode == "json" {
					cmd.Env = append(cmd.Env, "brishz_raw=n")
				}
				var out, errOut bytes.Buffer
				cmd.Stdout, cmd.Stderr = &out, &errOut
				err := cmd.Run()
				if e, ok := err.(*exec.ExitError); !ok || e.ExitCode() != 17 || out.String() != "out" || errOut.String() != "err" {
					t.Fatalf("original: %v, %q, %q", err, out.String(), errOut.String())
				}
				replay, err := os.ReadFile(clip)
				if err != nil {
					t.Fatal(err)
				}
				if err := os.Remove(clip); err != nil {
					t.Fatal(err)
				}
				cmd = exec.CommandContext(ctx, zsh, "-f")
				// Poison ambient settings. The replay must restore the request
				// and suppress both copying and async launch.
				cmd.Env = []string{"PATH=" + path, "HOME=" + tmp, "brishz_copy=y", "brishz_c=y", "brishz_async=y",
					"brishz_noquote=y", "brishz_in=wrong", "brishz_session=wrong", "bshEndpoint=http://example.invalid"}
				cmd.Dir = t.TempDir()
				cmd.Stdin = bytes.NewReader(replay)
				out.Reset()
				errOut.Reset()
				cmd.Stdout, cmd.Stderr = &out, &errOut
				err = cmd.Run()
				if e, ok := err.(*exec.ExitError); !ok || e.ExitCode() != 17 || out.String() != "out" || errOut.String() != "err" {
					t.Fatalf("replay: %v, %q, %q", err, out.String(), errOut.String())
				}
				if _, err := os.Stat(clip); !os.IsNotExist(err) {
					t.Fatalf("replay copied again: %v", err)
				}
				g.mu.Lock()
				defer g.mu.Unlock()
				if len(g.reqs) != 2 {
					t.Fatalf("got %d requests", len(g.reqs))
				}
				a, b := g.reqs[0], g.reqs[1]
				if mode == "json" {
					var left, right any
					json.Unmarshal(a.body, &left)
					json.Unmarshal(b.body, &right)
					if !reflect.DeepEqual(left, right) {
						t.Error("JSON replay changed the request")
					}
				} else if !bytes.Equal(a.body, b.body) {
					t.Error("replay changed command or stdin bytes")
				}
				if a.path != b.path || a.query != b.query {
					t.Errorf("replay changed route: %s?%s to %s?%s", a.path, a.query, b.path, b.query)
				}
			})
		}
	}
}

func TestCopyDoesNotEmbedCredentials(t *testing.T) {
	for _, endpoint := range []string{"http://localhost:1", "https://garden.example.invalid", "https://user:synthetic-secret@example.invalid"} {
		cfg := newConfig([]string{"true"}, envOf("bshEndpoint", endpoint, "GARDEN_PASS0", "synthetic-secret", "https_proxy", "http://user:synthetic-secret@localhost"), "/x", "")
		cfg.apikeyFile = "/synthetic/secret-headers"
		if got := replayCommand(cfg, "/x", nil); strings.Contains(got, "synthetic-secret") || strings.Contains(got, cfg.apikeyFile) {
			t.Fatal("credential embedded in replay")
		}
	}
}

func TestCopyOptionalAndNonfatal(t *testing.T) {
	for _, installed := range []bool{false, true} {
		t.Run(map[bool]string{false: "absent", true: "failing"}[installed], func(t *testing.T) {
			tmp := t.TempDir()
			t.Setenv("PATH", tmp)
			if installed {
				os.WriteFile(filepath.Join(tmp, "pbcopy"), []byte("#!/bin/sh\nexit 1\n"), 0o700)
			}
			g := newFakeGarden(t, func(w http.ResponseWriter, r *http.Request, body []byte) {
				rawReply(w, "out", "", 17, "1")
			})
			got := runBufferedWith(t, g, "input\n\n", []string{"cat"}, "brishz_c", "y", "brishz_in", magicReadStdin)
			if got.code != 17 || got.out != "out" || (installed && !strings.Contains(got.errOut, "pbcopy")) || (!installed && got.errOut != "") {
				t.Fatalf("%+v", got)
			}
			if !bytes.HasSuffix(g.reqs[0].body, []byte("input\n\n")) {
				t.Fatal("copy consumed stdin without sending it")
			}
		})
	}
}
