//go:build unix

package handoff

import (
	"errors"
	"os"
	"os/exec"
	"syscall"
)

// App-server may initialize MCP child processes before any model work. Give
// this invocation its own group so cancellation cannot leave those children
// running. The group id is the validated PID returned by exec.Start, never a
// process-name pattern or inherited group shared with the user's terminal.
func isolateProcess(cmd *exec.Cmd) {
	cmd.SysProcAttr = &syscall.SysProcAttr{Setpgid: true}
	cmd.Cancel = func() error {
		if cmd.Process == nil || cmd.Process.Pid <= 0 {
			return os.ErrProcessDone
		}
		err := syscall.Kill(-cmd.Process.Pid, syscall.SIGKILL)
		if errors.Is(err, syscall.ESRCH) {
			return os.ErrProcessDone
		}
		return err
	}
}
