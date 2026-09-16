package hold

import (
	"encoding/json"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

// payload builds a PreToolUse payload the way Claude Code sends one.
func payload(session, tool, cwd, filePath, command string) string {
	p := map[string]any{
		"session_id": session,
		"cwd":        cwd,
		"tool_name":  tool,
		"tool_input": map[string]any{
			"file_path": filePath,
			"command":   command,
		},
	}
	b, _ := json.Marshal(p)
	return string(b)
}

func guardStore(t *testing.T) (Store, string) {
	t.Helper()
	home := t.TempDir()
	t.Setenv("HOME", home)
	s := Store{Root: filepath.Join(home, ".night-holds")}
	held := filepath.Join(home, "tmp-holdtest")

	if _, err := s.Acquire(AcquireOpts{
		Resource: "repo:" + held,
		Holder:   "holder-A",
		TTL:      10 * time.Minute,
		Reason:   "guard test",
		Matches:  []string{"vcsh holdtest.sh"},
	}); err != nil {
		t.Fatal(err)
	}
	return s, held
}

// The fifteen cases the shell guard was held to, so the port cannot regress.
func TestGuardVerdicts(t *testing.T) {
	s, held := guardStore(t)
	home := filepath.Dir(held)

	cases := []struct {
		name string
		in   string
		deny bool
	}{
		{"foreign Edit inside held path", payload("holder-B", "Edit", "/x", held+"/x", ""), true},
		{"holder's own Edit", payload("holder-A", "Edit", "/x", held+"/x", ""), false},
		{"Edit outside held path", payload("holder-B", "Edit", "/x", home+"/elsewhere/init.el", ""), false},
		{"Bash naming the path", payload("holder-B", "Bash", "/x", "", "ls "+held), true},
		{"Bash matching --match literal", payload("holder-B", "Bash", "/x", "", "vcsh holdtest.sh status"), true},
		{"Bash cwd inside held path", payload("holder-B", "Bash", held+"/sub", "", "echo hi"), true},
		{"unrelated Bash", payload("holder-B", "Bash", home, "", "echo hello"), false},
		{"hold-release naming it", payload("holder-B", "Bash", "/x", "", "hold-release repo:"+held), false},
		{"hold-status from inside held cwd", payload("holder-B", "Bash", held, "", "hold-status"), false},
		{"empty file_path does not shift fields", payload("holder-B", "Bash", "/x", "", "echo safe"), false},
		{"multiline command", payload("holder-B", "Bash", "/x", "", "echo one\nls "+held), true},
		{"Read is not a matched tool", payload("holder-B", "Read", "/x", held+"/x", ""), false},
		{"sibling dir in Bash", payload("holder-B", "Bash", "/x", "", "ls "+held+"-backup"), false},
		{"sibling dir in Edit", payload("holder-B", "Edit", "/x", held+"-backup/x", ""), false},
		{"malformed payload fails open", "not json at all", false},
	}

	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			d := s.Guard(strings.NewReader(c.in), time.Now())
			if d.Deny != c.deny {
				t.Errorf("deny = %v, want %v (reason: %s)", d.Deny, c.deny, d.Reason)
			}
		})
	}
}

func TestGuardFailsOpenOnEmptyInput(t *testing.T) {
	s, _ := guardStore(t)
	if d := s.Guard(strings.NewReader(""), time.Now()); d.Deny {
		t.Error("empty input must fail open: a guard that bricks every agent is worse than the accident")
	}
}

// A held path is only named where a boundary sits on each side of it. The bare
// substring test this replaces denied `ls ~/tmpfoo` under a hold on ~/tmp.
func TestPathNamedBoundaries(t *testing.T) {
	const p = "/Users/evar/tmp"

	mustNot := []string{
		"ls /Users/evar/tmpfoo",
		"cat /Users/evar/tmpfile.txt",
		"echo /Users/evar/tmp-unrelated",
		"cd /Users/evar/tmpdir && ls",
		"ls /Users/evar/tm",
		"",
	}
	for _, c := range mustNot {
		if pathNamed(c, p) {
			t.Errorf("pathNamed(%q) fired; the held path is only a prefix of an unrelated one", c)
		}
	}

	must := []string{
		"cd /Users/evar/tmp",
		"cd /Users/evar/tmp && ls",
		"sed -i \"\" s/a/b/ /Users/evar/tmp/x.zsh",
		"rm -rf /Users/evar/tmp/old",
		"ls \"/Users/evar/tmp\"",
		"ls '/Users/evar/tmp'",
		"cd /Users/evar/tmp; ls",
		"find /Users/evar/tmp | wc -l",
		"echo $(ls /Users/evar/tmp)",
		"ls /Users/evar/tmpfoo /Users/evar/tmp",
	}
	for _, c := range must {
		if !pathNamed(c, p) {
			t.Errorf("pathNamed(%q) did not fire; the command genuinely names the held path", c)
		}
	}
}

func TestUnderPathIsNotAPrefixTest(t *testing.T) {
	if underPath("/a/tmp-backup/x", "/a/tmp") {
		t.Error("/a/tmp-backup is not inside /a/tmp")
	}
	if !underPath("/a/tmp", "/a/tmp") {
		t.Error("a path is inside itself")
	}
	if !underPath("/a/tmp/x/y", "/a/tmp") {
		t.Error("a nested path is inside")
	}
}

// gpu:0 has no filesystem extent, so nothing textual is derived from it and the
// guard can only ever block on an explicit --match.
func TestNonPathResourceBlocksNothingByDefault(t *testing.T) {
	home := t.TempDir()
	t.Setenv("HOME", home)
	s := Store{Root: filepath.Join(home, ".night-holds")}
	if _, err := s.Acquire(AcquireOpts{Resource: "gpu:0", Holder: "a", Reason: "training"}); err != nil {
		t.Fatal(err)
	}

	for _, in := range []string{
		payload("b", "Bash", "/x", "", "python train.py --gpu 0"),
		payload("b", "Edit", "/x", "/etc/hosts", ""),
		payload("b", "Bash", "/0", "", "echo 0"),
	} {
		if d := s.Guard(strings.NewReader(in), time.Now()); d.Deny {
			t.Errorf("a bare gpu:0 hold denied %q: %s", in, d.Reason)
		}
	}
}

func TestDenyJSONIsWhatClaudeCodeExpects(t *testing.T) {
	var out struct {
		HookSpecificOutput struct {
			HookEventName      string `json:"hookEventName"`
			PermissionDecision string `json:"permissionDecision"`
			Reason             string `json:"permissionDecisionReason"`
		} `json:"hookSpecificOutput"`
	}
	if err := json.Unmarshal([]byte(DenyJSON("because")), &out); err != nil {
		t.Fatal(err)
	}
	if out.HookSpecificOutput.HookEventName != "PreToolUse" ||
		out.HookSpecificOutput.PermissionDecision != "deny" ||
		out.HookSpecificOutput.Reason != "because" {
		t.Errorf("wrong shape: %+v", out.HookSpecificOutput)
	}
}
