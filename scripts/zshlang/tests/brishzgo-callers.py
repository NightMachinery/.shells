#!/usr/bin/env python3
"""Exercise migrated launchers with a recording client and synthetic HOME."""
import json
import os
from pathlib import Path
import shlex
import subprocess
import tempfile

ROOT = Path(__file__).resolve().parents[2]

with tempfile.TemporaryDirectory(prefix="brishzgo-callers-") as tmp:
    home = Path(tmp)
    mock = home / "client with spaces"
    log = home / "calls.jsonl"
    mock.write_text('''#!/usr/bin/env python3
import json, os, sys
from pathlib import Path
args=sys.argv[1:]
assert args[0]=='--', args
payload=sys.stdin.buffer.read() if os.environ.get('brishz_in')=='MAGIC_READ_STDIN' else b''
record={'args':args, 'stdin':payload.hex(), 'async':os.environ.get('brishz_async','')}
if args[1]=='h-stt-filter': record['file_input']=Path(args[2]).read_bytes().hex()
with open(os.environ['CALL_LOG'],'a') as f: f.write(json.dumps(record)+'\\n')
outputs={'last-idle-get-min':'42','audio-input-glyph-get':'mic','datej':'date',
 'now':'now','h-agent-session-pick-rows':'1\\t/path with quote\\\'s/transcript\\tclaude\\tlabel',
 'h-agent-session-fz-parts':'preview {3}\\n\\nheader'}
print(outputs.get(args[1], 'ok'))
sys.exit(int(os.environ.get('CLIENT_STATUS','0')))
''')
    mock.chmod(0o700)
    (home / ".bashrc").write_text("")
    env = {"HOME": tmp, "PATH": tmp + ":" + os.environ["PATH"],
           "BRISHZGO_BIN": str(mock), "CALL_LOG": str(log),
           "TMUX_PANE": "%9", "TMUX_SUBAGENT_NODE": "test"}

    def calls():
        rows = [json.loads(x) for x in log.read_text().splitlines()] if log.exists() else []
        log.unlink(missing_ok=True)
        return rows

    def run(argv, payload=b"", extra=None, status=0):
        p = subprocess.run(argv, cwd=ROOT, env=dict(env, **(extra or {})),
                           input=payload, capture_output=True, timeout=5)
        assert p.returncode == status, (argv, p.returncode, p.stderr)
        return p

    def commands(value):
        if isinstance(value, dict):
            if value.get("type") == "command": yield value["command"]
            for v in value.values(): yield from commands(v)
        elif isinstance(value, list):
            for v in value: yield from commands(v)

    for config in ("claude-code/settings.json", "codex/hooks.json", "antigravity/hooks.json"):
        count = 0
        for code in commands(json.loads((ROOT / "configFiles" / config).read_text())):
            if "BRISHZGO_BIN" not in code: continue
            run(["/bin/sh", "-c", code], b'{"event":"synthetic"}\n')
            rows = calls()
            assert len(rows) == 1 and rows[0]["async"] == "y"
            assert bytes.fromhex(rows[0]["stdin"]) == b'{"event":"synthetic"}\n'
            if rows[0]["args"][1] == "bell-claude":
                assert rows[0]["args"][2:] == ["--pane=%9", "--node=1"]
            if "tmux-autoname" in rows[0]["args"][1]:
                assert rows[0]["args"][2:] == ["%9"]
            count += 1
        assert count > 0, config

    strange = "a path with 'quotes';$(printf inert)"
    for shell, script, command, extra in (
        ("bash", "wrappers/emacs.dash", "emc-nowait2", {"emacs_no_wait": "y"}),
        ("bash", "wrappers/zopen.bash", "awaysh-named", {}),
        ("dash", "wrappers/audio-guard-tick.dash", "audio-guard-tick", {}),
        ("dash", "wrappers/rem-today-notify.dash", "rem-today-notify", {}),
        ("zsh", "hooks/lock.zsh", "h-hook-lock", {}),
        ("zsh", "hooks/unlock.zsh", "h-hook-unlock", {}),
        ("zsh", "hooks/audio-output-change.zsh", "h-hook-audio-output-change", {}),
    ):
        run([shell, str(ROOT / "zshlang" / script), strange],
            extra=dict(extra, CLIENT_STATUS="17"), status=17)
        row = calls()[0]
        assert row["args"][1] == command
        if command in ("emc-nowait2", "h-hook-audio-output-change"):
            assert row["args"][2:] == [strange]
        if command == "awaysh-named":
            assert row["async"] == "y" and row["args"][2:] == ["JOKER_MARKER", "zopen", strange]

    run(["dash", str(ROOT / "zshlang/wrappers/stt_filter.sh")],
        b"input\x00\xff\n\n", extra={"CLIENT_STATUS": "17"}, status=17)
    row = calls()[0]
    assert bytes.fromhex(row["file_input"]) == b"input\x00\xff\n\n"
    assert not Path(row["args"][2]).exists(), "STT input file leaked"

    run(["bash", str(ROOT / "zshlang/menubar/date.sh")])
    rows = calls()
    assert [r["args"][1] for r in rows] == ["eval", "last-idle-get-min", "audio-input-glyph-get", "datej", "eval", "now"]
    assert rows[0]["args"][2] == "menu_stopwatch_format=min serr reval-true menu-stopwatch-text-get"

    # Sioyek joins tokens ending in backslash before substituting macros.
    code = next(x for x in (ROOT / "configFiles/sioyek/prefs_user.config").read_text().splitlines()
                if x.startswith("new_command _copy_location_as_org ")).split(" ", 2)[2]
    parts = code.split()
    joined = []
    for part in parts:
        if joined and joined[-1].endswith("\\"):
            joined[-1] = joined[-1][:-1] + " " + part
        else: joined.append(part)
    replacements = {"%{file_path}": strange, "%{page_number}": "4", "%{offset_x_document}": "1.25",
                    "%{offset_y_document}": "2.5", "%{zoom_level}": "3"}
    argv = [replacements.get(x, x) for x in joined]
    run(argv)
    assert calls()[0]["args"] == ["--", "h-sioyek-org-pdf-link-create", strange, "4", "1.25", "2.5", "3"]

    kitty = (ROOT / "configFiles/kitty/kitty.conf").read_text()
    code = next(x for x in kitty.splitlines() if x.startswith("map cmd+shift+o "))
    run(shlex.split(code.split("--type=background ", 1)[1]))
    assert calls()[0]["args"] == ["--", "agent-view-session-focused"]
    code = next(x for x in (ROOT / "configFiles/kitty/open-actions.conf").read_text().splitlines()
                if x.startswith("action launch --type=background "))
    argv = shlex.split(code.split("--type=background ", 1)[1])
    run([strange if x == "${FILE_PATH}" else x for x in argv])
    assert calls()[0]["args"] == ["--", "zopen", strange]

    # Exercise the picker with fzf as an inert row selector.
    fzf = home / "fzf"
    fzf.write_text('#!/usr/bin/env python3\nimport sys\nprint(sys.stdin.read().splitlines()[0])\n')
    fzf.chmod(0o700)
    run(["zsh", "-f", str(ROOT / "zshlang/wrappers/agent-session-pick.zsh")],
        extra={"AGENT_VIEW_TAB_KEY": "tab with spaces"})
    rows = calls()
    assert rows[-1]["args"] == ["--", "agent-view-session-bg", "/path with quote's/transcript", "tab with spaces"]

print("PASS: hooks, wrappers, binary STT input, menubar, picker, kitty and Sioyek launchers")
