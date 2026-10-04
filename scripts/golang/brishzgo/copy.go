package main

import (
	"bytes"
	"encoding/base64"
	"fmt"
	"io"
	"net/url"
	"os/exec"
	"strings"
)

// copyReplay copies a shell command that resubmits the request, like the
// legacy copy option's curl snippet. It uses the client's normal credential
// sources rather than embedding key headers or passwords in the clipboard.
func copyReplay(cfg config, pwd string, stdin io.Reader, stderr io.Writer) (io.Reader, error) {
	bin, err := exec.LookPath("pbcopy")
	if err != nil {
		return stdin, nil // Same optional dependency as brishzq.zsh.
	}
	input := cfg.stdinLiteral
	if cfg.stdinMagic {
		input, err = io.ReadAll(stdin)
		if err != nil {
			return stdin, err
		}
		stdin = bytes.NewReader(input)
	}
	cmd := exec.Command(bin)
	cmd.Stdin = strings.NewReader(replayCommand(cfg, pwd, input))
	if err := cmd.Run(); err != nil {
		// Clipboard failure must not replace the garden command's status.
		fmt.Fprintf(stderr, "brishzgo: pbcopy: %v\n", err)
	}
	return stdin, nil
}

var replayEnvNames = []string{
	"bshEndpoint", "GARDEN_PORT", "brishz_session", "brishz_nolog",
	"brishz_failure_expected", "brishz_binary", "brishz_raw", "brishz_stream",
	"brishz_noquote", "NIGHT_EMACS_P", "EMACS_SOCKET_NAME", "emacs_night_server_name",
}

func replayCommand(cfg config, pwd string, input []byte) string {
	opts := []string{"-u", "brishz_copy", "-u", "brishz_c", "-u", "brishz_async"}
	assignments := []string{"brishz_in=" + string(input)}
	if cfg.stdinMagic {
		assignments[0] = "brishz_in=" + magicReadStdin
	}
	for _, name := range replayEnvNames {
		value, set := cfg.env(name)
		if name == "bshEndpoint" {
			if u, err := url.Parse(value); err == nil && u.User != nil {
				// Reuse an exported endpoint URL containing credentials
				// rather than copying the secret.
				continue
			}
		}
		if set {
			assignments = append(assignments, name+"="+value)
		} else {
			opts = append(opts, "-u", name)
		}
	}
	words := append([]string{"env"}, opts...)
	words = append(words, assignments...)
	words = append(words, "brishzgo", "--")
	words = append(words, cfg.args...)
	// Base64 keeps direct binary stdin, trailing newlines and large input out
	// of argv and avoids payload-file paths that disappear after this run.
	prefix := ""
	if cfg.stdinMagic {
		prefix = "command printf '%s' " + quoteSingle(base64.StdEncoding.EncodeToString(input)) +
			" | command base64 --decode |\n"
	}
	return "(\ncd " + quoteSingle(pwd) + " &&\n" + prefix + "command " + gquote(words) + "\n)\n"
}
