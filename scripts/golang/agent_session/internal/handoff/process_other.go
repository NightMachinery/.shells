//go:build !unix

package handoff

import "os/exec"

// Non-Unix platforms retain exec.CommandContext's direct-child cancellation.
func isolateProcess(cmd *exec.Cmd) {}
