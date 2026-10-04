package main

import (
	"fmt"
	"io"
	"os"
	"os/exec"
	"strings"
	"syscall"
)

// startAsync re-execs the client in its own session, with no caller-owned
// output pipes. It must keep the streaming connection open until completion:
// closing it after submission would cancel the garden command.
func startAsync(cfg config, pwd, home string, stdin io.Reader, stderr io.Writer) int {
	bin, err := os.Executable()
	if err != nil {
		fmt.Fprintf(stderr, "brishzgo: async executable: %v\n", err)
		return 1
	}
	cmd := exec.Command(bin, append([]string{"--"}, cfg.args...)...)
	cmd.Dir = pwd
	cmd.Env = asyncEnvironment(cfg.env, pwd, home)
	cmd.SysProcAttr = &syscall.SysProcAttr{Setsid: true}
	// nil stdout/stderr are /dev/null, rather than pipes inherited from a
	// hook runner which would wait for the detached child to close them.
	if cfg.stdinMagic {
		f, err := temps.create("brishzgo-async.")
		if err != nil {
			fmt.Fprintf(stderr, "brishzgo: async stdin: %v\n", err)
			return 1
		}
		defer f.Close()
		// The open descriptor survives unlink and is inherited as fd 0.
		// No named payload file remains if either process crashes.
		if err := os.Remove(f.Name()); err != nil {
			fmt.Fprintf(stderr, "brishzgo: async stdin: %v\n", err)
			return 1
		}
		if _, err := io.Copy(f, stdin); err != nil {
			fmt.Fprintf(stderr, "brishzgo: async stdin: %v\n", err)
			return 1
		}
		if _, err := f.Seek(0, io.SeekStart); err != nil {
			fmt.Fprintf(stderr, "brishzgo: async stdin: %v\n", err)
			return 1
		}
		cmd.Stdin = f
	}
	if err := cmd.Start(); err != nil {
		fmt.Fprintf(stderr, "brishzgo: async launch: %v\n", err)
		return 1
	}
	// The CLI exits immediately. Release the process handle; the detached
	// worker is reaped by the OS after this parent exits.
	_ = cmd.Process.Release()
	return 0
}

// Include names known to configuration even when absent from os.Environ so
// an injected lookupEnv has the same behavior in tests. Actual CLI workers
// inherit the full environment, including authentication and proxy settings.
var clientEnvNames = []string{
	"bshEndpoint", "GARDEN_PORT", "GARDEN_PASS0", "DISABLE_BRISH",
	"brishz_in", "brishz_session", "brishz_nolog", "brishz_failure_expected",
	"brishz_binary", "brishz_raw", "brishz_stream", "brishz_debug", "brishz_noquote",
	"brishz_async", "brishz_copy", "brishz_c", "NIGHT_EMACS_P", "EMACS_SOCKET_NAME", "emacs_night_server_name",
}

func asyncEnvironment(env lookupEnv, pwd, home string) []string {
	names := make(map[string]bool)
	for _, entry := range os.Environ() {
		name, _, _ := strings.Cut(entry, "=")
		names[name] = true
	}
	for _, name := range clientEnvNames {
		names[name] = true
	}
	var result []string
	for name := range names {
		if value, ok := env(name); ok {
			result = append(result, name+"="+value)
		}
	}
	return append(result, "brishz_async=", "brishz_copy=", "brishz_c=", "PWD="+pwd, "HOME="+home)
}
