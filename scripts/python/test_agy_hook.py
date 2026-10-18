import json
import os
from pathlib import Path
import subprocess
import tempfile
import unittest

from agy_accounts import record_owner, seed_owners
from agy_hook import program


class HookTest(unittest.TestCase):
    def test_missing_functions_are_never_invoked(self):
        code = program("tmux", ["%9"], {"AGY_ACCOUNT_ID": "m"})
        result = subprocess.run(["zsh", "-f", "-c", code], capture_output=True)
        self.assertEqual(result.returncode, 0)
        self.assertEqual(result.stderr, b"")

    def test_lookup_or_target_failure_returns_success(self):
        code = program("register", [], {"AGY_ACCOUNT_ID": "cave"})
        preamble = "function agy-account-hook { return 2 }; function agent-session-register { return 3 }; "
        result = subprocess.run(["zsh", "-f", "-c", preamble + code], capture_output=True)
        self.assertEqual(result.returncode, 0)
        self.assertEqual(result.stderr, b"")

    def test_arguments_are_quoted_and_legacy_label_becomes_m(self):
        with tempfile.TemporaryDirectory() as temporary:
            sentinel = Path(temporary) / "unintended"
            code = program("tmux", [f"$(touch {sentinel})"], {
                "AGY_ACCOUNT_ID": "acc1", "AGY_ACCOUNT_LABEL": "acc1",
                "ANTIGRAVITY_GEMINI_DIR": temporary + "/a 'quoted' root",
            })
            preamble = 'function agy-account-hook { print -rl -- "$@" }; function agy-session-tmux-autoname { true }; '
            result = subprocess.run(["zsh", "-f", "-c", preamble + code], capture_output=True)
            self.assertEqual(result.returncode, 0)
            self.assertFalse(sentinel.exists())
            self.assertIn("agy-account-hook m ", code)

    def test_ownership_is_first_writer_and_seeding_does_not_read_history(self):
        with tempfile.TemporaryDirectory() as temporary:
            base = Path(temporary)
            (base / "shared/antigravity-cli/brain/old").mkdir(parents=True)
            seed_owners(base / "owners", base / "shared")
            self.assertEqual((base / "owners/old").read_text(), "m\n")
            record_owner(base / "owners", "new", "cave")
            record_owner(base / "owners", "new", "fm2")
            self.assertEqual((base / "owners/new").read_text(), "cave\n")
            record_owner(base / "owners", "../outside", "fm2")
            self.assertFalse((base / "outside").exists())

    def test_missing_brish_is_success_and_empty_json(self):
        with tempfile.TemporaryDirectory() as temporary:
            environment = dict(os.environ, BRISHZGO_BIN=temporary + "/missing",
                               AGY_ACCOUNTS_DIR=temporary)
            result = subprocess.run(["python3", str(Path(__file__).with_name("agy_hook.py")), "tmux", "%9"],
                                    input=b"{}", capture_output=True, env=environment)
            self.assertEqual(result.returncode, 0)
            self.assertEqual(json.loads(result.stdout), {})
            self.assertEqual(result.stderr, b"")


if __name__ == "__main__":
    unittest.main()
