"""Account setup must not share auth or overwrite existing profile data."""
import json
from pathlib import Path
import tempfile
import unittest

from agy_accounts import prepare


class AccountsTest(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary.cleanup)
        base = Path(self.temporary.name)
        self.shared, self.account = base / "shared", base / "new"
        (self.shared / "antigravity-cli").mkdir(parents=True)
        (self.shared / "config").mkdir()
        (self.shared / "GEMINI.md").write_text("shared rules")
        (self.shared / "config/hooks.json").write_text("{}")
        (self.shared / "antigravity-cli/settings.json").write_text(json.dumps({
            "statusLine": {"command": "fixture"}, "unshared": "fixture",
        }))
        (self.shared / "antigravity-cli/antigravity-oauth-token").write_text("inert fixture")
        (self.shared / "antigravity-cli/conversations").mkdir()
        (self.shared / "antigravity-cli/conversations/fixture.pb").write_text("inert history")

    def test_share_only_configuration_and_statusline(self):
        prepare(self.account, self.shared)
        prepare(self.account, self.shared)
        self.assertTrue((self.account / "GEMINI.md").is_symlink())
        cli = self.account / "antigravity-cli"
        self.assertFalse((cli / "antigravity-oauth-token").exists())
        self.assertTrue((cli / "conversations").is_symlink())
        self.assertEqual((cli / "conversations/fixture.pb").read_text(), "inert history")
        self.assertFalse((cli / "conversation_summaries.db").exists())
        self.assertEqual(json.loads((cli / "settings.json").read_text()), {
            "statusLine": {"command": "fixture"},
        })
        self.assertEqual(cli.stat().st_mode & 0o777, 0o700)

    def test_existing_original_is_untouched(self):
        settings = self.shared / "antigravity-cli/settings.json"
        before = settings.stat().st_mtime_ns
        prepare(self.shared, self.shared)
        self.assertEqual(settings.stat().st_mtime_ns, before)

    def test_conflicts_and_nested_roots_refused(self):
        self.account.mkdir()
        (self.account / "GEMINI.md").write_text("own rules")
        with self.assertRaises(ValueError):
            prepare(self.account, self.shared)
        self.assertEqual((self.account / "GEMINI.md").read_text(), "own rules")
        with self.assertRaises(ValueError):
            prepare(self.shared / "nested", self.shared)

    def test_shared_state_symlink_refused(self):
        self.account.mkdir()
        (self.account / "antigravity-cli").symlink_to(self.shared / "antigravity-cli")
        with self.assertRaises(ValueError):
            prepare(self.account, self.shared)

    def test_preserve_private_settings_on_sync(self):
        prepare(self.account, self.shared)
        settings = self.account / "antigravity-cli/settings.json"
        settings.write_text('{"model":"private fixture"}')
        prepare(self.account, self.shared)
        self.assertEqual(json.loads(settings.read_text())["model"], "private fixture")

    def test_existing_history_is_never_replaced(self):
        (self.account / "antigravity-cli/conversations").mkdir(parents=True)
        original = self.account / "antigravity-cli/conversations/own.pb"
        original.write_text("own history")
        with self.assertRaises(ValueError):
            prepare(self.account, self.shared)
        self.assertEqual(original.read_text(), "own history")


if __name__ == "__main__":
    unittest.main()
