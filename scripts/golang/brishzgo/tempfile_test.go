package main

import (
	"errors"
	"io"
	"os"
	"os/exec"
	"strings"
	"syscall"
	"testing"
	"time"
)

// TestSignalRemovesTempFile: SIGTERM while the JSON request is in flight
// removes the stdin temp file, and brishzgo dies of that same signal.
func TestSignalRemovesTempFile(t *testing.T) {
	bin := builtBinary(t)
	arrived := make(chan struct{}, 1)
	release := make(chan struct{})
	defer close(release)
	g := jsonGarden(t, func(req map[string]any) (string, string) {
		arrived <- struct{}{}
		<-release
		return echoReply(req)
	})
	tmp := t.TempDir()
	cmd := exec.Command(bin, "cat")
	cmd.Env = []string{"PATH=" + os.Getenv("PATH"), "HOME=" + t.TempDir(), "TMPDIR=" + tmp,
		"bshEndpoint=" + g.URL, "brishz_in=MAGIC_READ_STDIN"}
	cmd.Stdin = strings.NewReader("secret-ish stdin")
	cmd.Stdout, cmd.Stderr = io.Discard, io.Discard
	if err := cmd.Start(); err != nil {
		t.Fatal(err)
	}
	select {
	case <-arrived:
	case <-time.After(10 * time.Second):
		cmd.Process.Kill()
		t.Fatal("the JSON request never arrived")
	}
	if names, _ := os.ReadDir(tmp); len(names) != 1 {
		t.Fatalf("temp files during the request: %v", names)
	}
	cmd.Process.Signal(syscall.SIGTERM)
	err := cmd.Wait()
	var ee *exec.ExitError
	if !errors.As(err, &ee) || ee.Sys().(syscall.WaitStatus).Signal() != syscall.SIGTERM {
		t.Errorf("exit: %v, want death by SIGTERM", err)
	}
	if names, _ := os.ReadDir(tmp); len(names) != 0 {
		t.Errorf("temp files left: %v", names)
	}
}
