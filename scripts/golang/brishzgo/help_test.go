package main

import (
	"bytes"
	"io"
	"net/http"
	"strings"
	"testing"
)

type unreadableStdin struct{ t *testing.T }

func (r unreadableStdin) Read([]byte) (int, error) {
	r.t.Fatal("help read stdin")
	return 0, io.EOF
}

func TestLocalHelp(t *testing.T) {
	for _, flag := range []string{"-h", "--help"} {
		var out, errOut bytes.Buffer
		// Help must not consult configuration, credentials or the network.
		env := func(string) (string, bool) {
			t.Fatal("help consulted environment")
			return "", false
		}
		if code := run([]string{flag}, env, "/x", "/nonexistent", unreadableStdin{t}, &out, &errOut); code != 0 || !strings.HasPrefix(out.String(), "Usage:") || errOut.Len() != 0 {
			t.Fatalf("%s: exit %d, stdout %q, stderr %q", flag, code, out.String(), errOut.String())
		}
	}
}

func TestHelpPassthrough(t *testing.T) {
	for _, args := range [][]string{{"--", "--help"}, {"--", "-h"}, {"-c", "--help"}, {"command", "--help"}, {"--help", "argument"}, {"--", "--", "-h"}} {
		g := newFakeGarden(t, func(w http.ResponseWriter, r *http.Request, body []byte) {
			rawReply(w, "garden", "", 0, "1")
		})
		got := runBufferedWith(t, g, "", args, "brishz_noquote", "y")
		if got != (result{0, "garden", ""}) {
			t.Fatalf("%q: %+v", args, got)
		}
		wantArgs := args
		if args[0] == "--" || args[0] == "-c" {
			wantArgs = args[1:]
		}
		if got := string(g.reqs[0].body); got != strings.Join(wantArgs, " ") {
			t.Errorf("%q: sent %q", args, got)
		}
	}
}
