package main

import (
	"bytes"
	"context"
	"fmt"
	"io"
	"net/http"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"syscall"
	"testing"
	"time"
)

func TestAsyncDetached(t *testing.T) {
	bin := builtBinary(t)
	for _, mode := range []string{"stream", "raw", "fallback-json"} {
		for _, inputMode := range []string{"literal", "stdin"} {
			t.Run(mode+"/"+inputMode, func(t *testing.T) {
				input := "it's input\n\n"
				if inputMode == "stdin" {
					input = strings.Repeat("a", 256<<10) + "\x00\xff\n\n"
				}
				ready := make(chan []byte, 1)
				finished := make(chan error, 1)
				allowReply := make(chan struct{}, 1)
				g := newFakeGarden(t, func(w http.ResponseWriter, r *http.Request, body []byte) {
					if mode == "fallback-json" && r.URL.Path != "/zsh/" {
						w.WriteHeader(404)
						return
					}
					var payload []byte
					if r.URL.Path == "/zsh/" {
						if inputMode == "stdin" {
							cmdText := requestField(t, body, "cmd")
							// Local JSON fallback hands the garden a temp file.
							file := strings.Split(cmdText, "'")[1]
							var err error
							payload, err = os.ReadFile(file)
							if err != nil {
								finished <- err
								return
							}
						} else {
							payload = []byte(requestField(t, body, "stdin"))
						}
					} else {
						n := 0
						fmt.Sscan(r.Header.Get("X-Brish-Cmd-Length"), &n)
						payload = body[n:]
					}
					ready <- payload
					select {
					case <-r.Context().Done():
						finished <- fmt.Errorf("parent exit cancelled detached request")
						return
					case <-allowReply:
					}
					var err error
					if mode == "stream" {
						w.Header().Set("X-Brish-Stream", "1")
						w.Write(frameOf(frameStdout, bytes.Repeat([]byte("o"), 256<<10)))
						w.Write(frameOf(frameStderr, bytes.Repeat([]byte("e"), 256<<10)))
						_, err = w.Write(exitFrameOf(17))
					} else if mode == "raw" {
						rawReply(w, "discarded", "discarded", 17, "1")
					} else {
						_, err = io.WriteString(w, `{"retcode":17,"out":"discarded","err":"discarded"}`)
					}
					finished <- err
				})
				// Cleanup runs before server.Close, including on test failure.
				t.Cleanup(func() { close(allowReply) })
				ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
				defer cancel()
				cmd := exec.CommandContext(ctx, bin, "--", "--help")
				cmd.Dir = t.TempDir()
				// Separate parent group lets us send a caller-group signal below
				// without touching the test runner or unrelated processes.
				cmd.SysProcAttr = &syscall.SysProcAttr{Setpgid: true}
				cmd.Env = []string{"HOME=" + t.TempDir(), "bshEndpoint=" + g.URL, "brishz_async=y", "brishz_noquote=y"}
				if mode == "raw" {
					cmd.Env = append(cmd.Env, "brishz_stream=n")
				}
				if inputMode == "stdin" {
					cmd.Env = append(cmd.Env, "brishz_in=MAGIC_READ_STDIN")
					cmd.Stdin = strings.NewReader(input)
				} else {
					cmd.Env = append(cmd.Env, "brishz_in="+input)
				}
				if out, err := cmd.CombinedOutput(); err != nil || len(out) != 0 {
					t.Fatalf("parent waited for response or failed: %v, %q", err, out)
				}
				// The parent has exited; the server still has not sent a reply.
				select {
				case got := <-ready:
					if string(got) != input {
						t.Errorf("stdin changed: received %d of %d bytes", len(got), len(input))
					}
				case err := <-finished:
					t.Fatalf("request failed: %v", err)
				case <-ctx.Done():
					t.Fatal("detached worker did not deliver request")
				}
				// A detached worker must survive signals sent to the caller's group.
				syscall.Kill(-cmd.Process.Pid, syscall.SIGTERM)
				allowReply <- struct{}{}
				select {
				case err := <-finished:
					if err != nil {
						t.Fatal(err)
					}
				case <-ctx.Done():
					t.Fatal("worker failed to drain response")
				}
			})
		}
	}
}

func TestAsyncInputFailure(t *testing.T) {
	tmp := t.TempDir()
	t.Setenv("TMPDIR", tmp)
	var errOut bytes.Buffer
	code := run([]string{"true"}, envOf("brishz_async", "y", "brishz_in", magicReadStdin), "/x", "", failingReader{}, io.Discard, &errOut)
	if code != 1 || !strings.Contains(errOut.String(), "async stdin") {
		t.Fatalf("exit %d, stderr %q", code, errOut.String())
	}
	files, err := filepath.Glob(filepath.Join(tmp, "brishzgo-async.*"))
	if err != nil || len(files) != 0 {
		t.Fatalf("leaked input files %v, %v", files, err)
	}
}

type failingReader struct{}

func (failingReader) Read([]byte) (int, error) { return 0, fmt.Errorf("synthetic read failure") }
