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
	t.Setenv("CLAUDE_PID", "")
	t.Setenv("hold_agent_pid", "")
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

// verdict is the three-way contract: a structured write to a held path is
// denied, a textual `Bash' signal warns and lets the call through, everything
// else is silent.
type verdict int

const (
	allow verdict = iota
	warn
	deny
)

func (v verdict) String() string {
	switch v {
	case deny:
		return "deny"
	case warn:
		return "warn"
	}
	return "allow"
}

func got(d Decision) verdict {
	switch {
	case d.Deny:
		return deny
	case d.Warn:
		return warn
	}
	return allow
}

// The fifteen cases the shell guard was held to, so the port cannot regress --
// with the four `Bash' ones downgraded from deny to warn, which is the point of
// the split: only an edit tool's `file_path' proves a write.
func TestGuardVerdicts(t *testing.T) {
	s, held := guardStore(t)
	home := filepath.Dir(held)

	cases := []struct {
		name string
		in   string
		want verdict
	}{
		{"foreign Edit inside held path", payload("holder-B", "Edit", "/x", held+"/x", ""), deny},
		{"holder's own Edit", payload("holder-A", "Edit", "/x", held+"/x", ""), allow},
		{"Edit outside held path", payload("holder-B", "Edit", "/x", home+"/elsewhere/init.el", ""), allow},
		{"Bash naming the path", payload("holder-B", "Bash", "/x", "", "ls "+held), warn},
		{"Bash matching --match literal", payload("holder-B", "Bash", "/x", "", "vcsh holdtest.sh status"), warn},
		{"Bash cwd inside held path", payload("holder-B", "Bash", held+"/sub", "", "echo hi"), warn},
		{"unrelated Bash", payload("holder-B", "Bash", home, "", "echo hello"), allow},
		{"hold-release naming it", payload("holder-B", "Bash", "/x", "", "hold-release repo:"+held), allow},
		{"hold-status from inside held cwd", payload("holder-B", "Bash", held, "", "hold-status"), allow},
		{"empty file_path does not shift fields", payload("holder-B", "Bash", "/x", "", "echo safe"), allow},
		{"multiline command", payload("holder-B", "Bash", "/x", "", "echo one\nls "+held), warn},
		{"Read is not a matched tool", payload("holder-B", "Read", "/x", held+"/x", ""), allow},
		{"sibling dir in Bash", payload("holder-B", "Bash", "/x", "", "ls "+held+"-backup"), allow},
		{"sibling dir in Edit", payload("holder-B", "Edit", "/x", held+"-backup/x", ""), allow},
		{"malformed payload fails open", "not json at all", allow},
	}

	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			d := s.Guard(strings.NewReader(c.in), time.Now())
			if g := got(d); g != c.want {
				t.Errorf("verdict = %s, want %s (reason: %s)", g, c.want, d.Reason)
			}
		})
	}
}

// The case that started this: an agent could not even read under a hold. A
// read names the path exactly as a write does, so the guard must not deny it.
func TestReadUnderHoldIsNotDenied(t *testing.T) {
	s, held := guardStore(t)

	reads := []string{
		"grep -rn 'foo' " + held + "/zshlang",
		"cat " + held + "/readme.org",
		"vcsh holdtest.sh log --oneline -5",
		"python3 - <<'EOF'\nprint('mentions vcsh holdtest.sh as data')\nEOF",
	}
	for _, cmd := range reads {
		d := s.Guard(strings.NewReader(payload("holder-B", "Bash", "/x", "", cmd)), time.Now())
		if d.Deny {
			t.Errorf("denied a Bash call it cannot prove writes: %q", cmd)
		}
		if !d.Warn {
			t.Errorf("no warning for a command naming the held resource: %q", cmd)
		}
	}
}

// A warning must never carry a permissionDecision. "allow" would not merely
// un-block the call, it would bypass the normal permission prompt for it.
func TestWarnJSONGrantsNothing(t *testing.T) {
	out := WarnJSON("careful")
	if strings.Contains(out, "permissionDecision") {
		t.Fatalf("WarnJSON must not decide permission: %s", out)
	}
	var parsed map[string]any
	if err := json.Unmarshal([]byte(out), &parsed); err != nil {
		t.Fatalf("WarnJSON is not valid JSON: %v", err)
	}
	h, _ := parsed["hookSpecificOutput"].(map[string]any)
	if h["additionalContext"] != "careful" || h["hookEventName"] != "PreToolUse" {
		t.Errorf("warning does not reach the agent: %s", out)
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
	t.Setenv("CLAUDE_PID", "")
	t.Setenv("hold_agent_pid", "")
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
