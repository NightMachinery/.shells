#!/usr/bin/env python3
"""Regression checks for front-app identity and query failures, using fixtures."""

from pathlib import Path
import re
import subprocess
import sys
import unittest


ROOT = Path(__file__).resolve().parents[2]
SOURCE = ROOT / "zshlang/auto-load/others/macOS/macOS.zsh"


class FrontAppShellTests(unittest.TestCase):
    def shell(self, body):
        preamble = r'''
unset night_basic_plugin_loaded_p
source ./zshlang/basic/basic.plugin.zsh || exit $?
source ./zshlang/basic/compatibility.zsh || exit $?
function aliasfn { :; }
source ./zshlang/auto-load/others/macOS/macOS.zsh || exit $?
function cat-copy-if-tty { command cat; }
function isDarwin { return 0; }
unset front_app_cached
setopt nopipefail
'''
        return subprocess.run(
            ["zsh", "-f", "-c", preamble + body], cwd=ROOT,
            text=True, capture_output=True, timeout=15,
        )

    def test_backend_result_is_preserved(self):
        for identity in ("io.mpv", "mpv", "org.example.Player"):
            with self.subTest(identity=identity):
                result = self.shell(
                    f"function frontapp-get-osa {{ print -r -- '{identity}'; }}\n"
                    "frontapp-get\n"
                )
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertEqual(result.stdout, identity + "\n")

    def test_cached_value_bypasses_query(self):
        result = self.shell('''
function frontapp-get-osa { print -ru2 -- 'unexpected query'; return 23; }
front_app_cached=mpv frontapp-get
''')
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout, "mpv\n")
        self.assertEqual(result.stderr, "")

    def test_query_failure_survives_pipeline(self):
        result = self.shell('''
function frontapp-get-osa { print -ru2 -- 'fixture query failed'; return 23; }
frontapp-get
''')
        self.assertEqual(result.returncode, 23, result.stderr)
        self.assertEqual(result.stdout, "")
        self.assertIn("fixture query failed", result.stderr)

    def test_pipefail_change_is_local(self):
        result = self.shell('''
function frontapp-get-osa { return 23; }
frontapp-get || true
[[ ! -o pipefail ]]
''')
        self.assertEqual(result.returncode, 0, result.stderr)


@unittest.skipUnless(sys.platform == "darwin", "AppleScript requires macOS")
class FrontAppAppleScriptTests(unittest.TestCase):
    def fixture(self, identifier, name):
        source = SOURCE.read_text()
        script = re.search(
            r"command osascript <<'APPLESCRIPT'\n(.*?)\nAPPLESCRIPT", source, re.S,
        ).group(1)
        # Replace only the live property query; exercise the production fallback.
        script, count = re.subn(
            r'tell application "System Events"\n.*?\nend tell',
            f"set {{appID, appName}} to {{{identifier}, {name}}}",
            script, count=1, flags=re.S,
        )
        self.assertEqual(count, 1)
        return subprocess.run(
            ["/usr/bin/osascript", "-"], input=script,
            text=True, capture_output=True, timeout=15,
        )

    def test_real_bundle_identifier_wins(self):
        result = self.fixture('"io.mpv"', '"mpv"')
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout, "io.mpv\n")

    def test_missing_and_empty_identifier_use_name(self):
        for identifier in ("missing value", '""'):
            with self.subTest(identifier=identifier):
                result = self.fixture(identifier, '"mpv"')
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertEqual(result.stdout, "mpv\n")

    def test_missing_identity_is_an_error(self):
        for name in ("missing value", '""'):
            with self.subTest(name=name):
                result = self.fixture("missing value", name)
                self.assertNotEqual(result.returncode, 0)
                self.assertEqual(result.stdout, "")


if __name__ == "__main__":
    unittest.main()
