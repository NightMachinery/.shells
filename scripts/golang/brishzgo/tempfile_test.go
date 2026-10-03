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

// TestIgnoredSignalStaysIgnored: a SIGINT that was ignored when brishzgo
// started (a background job of a non-interactive shell) still is: the run
// goes on to its end, and its temp file is removed then.
func TestIgnoredSignalStaysIgnored(t *testing.T) {
	bin := builtBinary(t)
	arrived := make(chan struct{}, 1)
	release := make(chan struct{})
	g := jsonGarden(t, func(req map[string]any) (string, string) {
		arrived <- struct{}{}
		<-release
		return echoReply(req)
	})
	tmp := t.TempDir()
	// sh's trap '' INT sets SIGINT to ignored, which exec keeps.
	cmd := exec.Command("/bin/sh", "-c", `trap '' INT; exec "$0" cat`, bin)
	cmd.Env = []string{"PATH=" + os.Getenv("PATH"), "HOME=" + t.TempDir(), "TMPDIR=" + tmp,
		"bshEndpoint=" + g.URL, "brishz_in=MAGIC_READ_STDIN"}
	cmd.Stdin = strings.NewReader("in")
	var out strings.Builder
	cmd.Stdout, cmd.Stderr = &out, io.Discard
	if err := cmd.Start(); err != nil {
		t.Fatal(err)
	}
	select {
	case <-arrived:
	case <-time.After(10 * time.Second):
		cmd.Process.Kill()
		t.Fatal("the JSON request never arrived")
	}
	cmd.Process.Signal(syscall.SIGINT)
	time.Sleep(200 * time.Millisecond)
	close(release)
	err := cmd.Wait()
	var ee *exec.ExitError
	if !errors.As(err, &ee) || ee.ExitCode() != 4 || out.String() != "in" {
		t.Errorf("exit: %v, out %q; want exit 4 and the output", err, out.String())
	}
	if names, _ := os.ReadDir(tmp); len(names) != 0 {
		t.Errorf("temp files left: %v", names)
	}
}
