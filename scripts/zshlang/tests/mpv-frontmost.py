#!/usr/bin/env python3
"""Check mpv focus and its Luna/Night Shift callers without GUI side effects."""

from pathlib import Path
import re
import subprocess
import unittest


ROOT = Path(__file__).resolve().parents[2]


def function_source(filename, name):
    # Load only the function under test, avoiding unrelated module startup code.
    source = (ROOT / filename).read_text()
    return re.search(
        rf"^function {re.escape(name)} \{{\n.*?^\}}", source, re.M | re.S,
    ).group(0)


FUNCTIONS = "\n".join([
    function_source("zshlang/auto-load/others/mpv.zsh", "mpv-frontmost-p"),
    function_source("zshlang/auto-load/others/luna-bells.zsh", "luna-skip-p"),
    function_source("zshlang/auto-load/others/macOS/nightshift.zsh", "nightshift-auto"),
])


class MpvFrontmostTests(unittest.TestCase):
    def shell(self, body, identity="org.example.Editor"):
        preamble = r'''
unset night_basic_plugin_loaded_p
source ./zshlang/basic/basic.plugin.zsh || exit $?
function frontapp-get {
    print -ru2 -- fixture-query
    print -r -- "${fixture_frontapp}"
    return "${fixture_query_rc:-0}"
}
function meeting-p { [[ "${fixture_meeting:-n}" == y ]]; }
function focus-do-not-disturb-p { [[ "${fixture_dnd:-n}" == y ]]; }
function nightshift_auto_enabled_get { print -r -- "${fixture_enabled:-1}"; }
function nightshift-off { print -r -- nightshift-off; }
function nightshift-on { print -r -- nightshift-on; }
fixture_frontapp="$1"
'''
        return subprocess.run(
            ["zsh", "-f", "-c", preamble + '\neval "$2"\n' + body,
             "fixture", identity, FUNCTIONS],
            cwd=ROOT, text=True, capture_output=True, timeout=15,
        )

    def test_app_and_shell_forms_match_silently(self):
        for identity in ("io.mpv", "mpv", "/example/bin/mpv"):
            with self.subTest(identity=identity):
                result = self.shell("mpv-frontmost-p", identity)
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertEqual(result.stdout, "")
                self.assertEqual(result.stderr, "fixture-query\n")

    def test_other_apps_and_lookalikes_do_not_match(self):
        for identity in ("org.example.Editor", "mpv-helper", "io.mpv.helper", "", "missing value"):
            with self.subTest(identity=identity):
                result = self.shell("mpv-frontmost-p", identity)
                self.assertEqual(result.returncode, 1, result.stderr)
                self.assertEqual(result.stdout, "")
                self.assertEqual(result.stderr, "fixture-query\n")

    def test_query_errors_propagate(self):
        for caller in ("mpv-frontmost-p", "luna-skip-p"):
            with self.subTest(caller=caller):
                result = self.shell(f"fixture_query_rc=23\n{caller}", "mpv")
                self.assertEqual(result.returncode, 23, result.stderr)
                self.assertEqual(result.stdout, "")

    def test_luna_skips_for_frontmost_mpv(self):
        for identity in ("io.mpv", "mpv", "/example/bin/mpv"):
            with self.subTest(identity=identity):
                result = self.shell("luna-skip-p", identity)
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertEqual(result.stdout, "")
                self.assertEqual(result.stderr, "fixture-query\n")
        result = self.shell("luna-skip-p")
        self.assertEqual(result.returncode, 1, result.stderr)

    def test_meeting_and_dnd_skip_without_querying_focus(self):
        for setting in ("fixture_meeting=y", "fixture_dnd=y"):
            with self.subTest(setting=setting):
                result = self.shell(f"{setting}\nluna-skip-p")
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertEqual(result.stdout, "")
                self.assertEqual(result.stderr, "")

    def test_nightshift_uses_same_app_matching(self):
        for identity, expected in (("io.mpv", "off"), ("mpv", "off"), ("org.example.Editor", "on")):
            with self.subTest(identity=identity):
                result = self.shell("nightshift-auto", identity)
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertEqual(result.stdout, f"nightshift-{expected}\n")
                self.assertEqual(result.stderr, "fixture-query\n")

    def test_disabled_nightshift_does_not_query_or_change_state(self):
        result = self.shell("fixture_enabled=0\nnightshift-auto", "mpv")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout, "")
        self.assertEqual(result.stderr, "")


if __name__ == "__main__":
    unittest.main()
