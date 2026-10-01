package main

import (
	"regexp"
	"strings"
)

// The command text, assembled from argv as brishzq.zsh assembles it.

// lookupEnv is os.LookupEnv, injectable for tests.
type lookupEnv func(string) (string, bool)

func (e lookupEnv) get(name string) string {
	v, _ := e(name)
	return v
}

// localEndpointRe is brishzq.zsh's test for a local garden, kept exactly,
// unescaped dots included, since it decides the wrapping below.
var localEndpointRe = regexp.MustCompile(`^https?://127.0.0.1`)

// emacsForwardedVars are forwarded, when set, if NIGHT_EMACS_P is non-empty.
var emacsForwardedVars = []string{"NIGHT_EMACS_P", "EMACS_SOCKET_NAME", "emacs_night_server_name"}

// buildCommand returns the command text brishzq.zsh sends for argv `args`,
// with brishz_noquote unset:
//
//   - the words quoted by gquote;
//   - when NIGHT_EMACS_P is non-empty, one `local -x NAME=VALUE` line per
//     forwarded variable that is set, prepended, so the last name comes
//     first; EMACS_SOCKET_NAME is set to emacs_night_server_name first, as
//     brishzq.zsh exports it;
//   - for a local endpoint, the whole of it inside a subshell that marks
//     itself with mark-me, cds to our working directory, and cds back to
//     /tmp before returning the command's status.
//
// With brishz_noquote set, the words are only joined by spaces, unwrapped.
func buildCommand(args []string, env lookupEnv, endpoint, pwd string) string {
	if env.get("brishz_noquote") != "" {
		return strings.Join(args, " ")
	}
	cmd := gquote(args)

	if env.get("NIGHT_EMACS_P") != "" {
		values := map[string]string{}
		set := map[string]bool{}
		for _, name := range emacsForwardedVars {
			values[name], set[name] = env(name)
		}
		values["EMACS_SOCKET_NAME"] = values["emacs_night_server_name"]
		set["EMACS_SOCKET_NAME"] = true
		for _, name := range emacsForwardedVars {
			if set[name] {
				cmd = "local -x " + name + "=" + quoteTypesetValue(values[name]) + "\n" + cmd
			}
		}
	}

	if localEndpointRe.MatchString(endpoint) {
		cmd = "( mark-me 'BRISHZQ_MARKER' " + gquote(args) + "\n" +
			"cd " + quoteSingle(pwd) + "\n" +
			cmd + "\n" +
			"ret=$? ; cd /tmp ; return-code $ret )"
	}
	return cmd
}
