//go:build unix

package handoff

import (
	"context"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
	"testing"
	"time"
)

func TestCancellationStopsAppServerChildGroup(t *testing.T) {
	o := fixture(t, "timeout-child")
	ctx, cancel := context.WithTimeout(context.Background(), time.Second)
	defer cancel()
	c, err := start(ctx, o.Codex, nil, nil)
	if err != nil {
		t.Fatal(err)
	}
	if err = c.call("initialize", map[string]any{}, nil); err == nil {
		t.Fatal("hung helper succeeded")
	}
	c.close()
	data, err := os.ReadFile(filepath.Join(o.Cwd, "child.pid"))
	if err != nil {
		t.Fatal(err)
	}
	pid, err := strconv.Atoi(string(data))
	if err != nil || pid <= 0 {
		t.Fatalf("invalid fake child pid %q", data)
	}
	// A just-orphaned child can briefly be a zombie pending the system reaper;
	// that is terminated and holds no pipes or resources, unlike a live orphan.
	deadline := time.Now().Add(time.Second)
	for {
		output, err := exec.Command("ps", "-o", "stat=", "-p", strconv.Itoa(pid)).Output()
		state := strings.TrimSpace(string(output))
		if err != nil || state == "" || strings.HasPrefix(state, "Z") {
			break
		}
		if time.Now().After(deadline) {
			t.Fatalf("app-server child %d still running (%s)", pid, state)
		}
		time.Sleep(20 * time.Millisecond)
	}
	if c.cmd.ProcessState == nil {
		t.Fatal("app-server parent was not reaped")
	}
}
