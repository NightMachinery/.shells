#!/usr/bin/env python3
"""Check change-only instruction sync reports using synthetic sources and HOME."""

import os
from pathlib import Path
import subprocess
import tempfile
import unittest


ROOT = Path(__file__).resolve().parents[2]


class AgentsMdSyncTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix="agents-md-sync-")
        self.addCleanup(self.temp.cleanup)
        self.home = Path(self.temp.name)
        self.sources = self.home / "sources"
        self.sources.mkdir()
        self.source = self.sources / "AGENTS.md"
        self.source.write_text("Synthetic shared instructions.\n")
        self.installed = self.home / "installed"
        self.installed.mkdir()
        self.target = self.installed / "AGENTS.md"
        self.overlay = self.installed / "overlay.md"

    def sync(self, quiet=False):
        script = r'''
unset night_basic_plugin_loaded_p
source "${SYNC_TEST_ROOT}/zshlang/basic/basic.plugin.zsh" || exit $?
source "${SYNC_TEST_ROOT}/zshlang/auto-load/others/agents-md.zsh" || exit $?
function ecgray { print -ru2 -- "$@"; }
function ecerr { print -ru2 -- "$@"; }
function h-agents-md-agents {
    printf 'fixture\t%s\n' "${HOME}/installed/AGENTS.md"
    printf 'fixture-overlay\t%s\toverlay\n' "${HOME}/installed/overlay.md"
}
agents_md_dir="${HOME}/sources"
agents_md_private_dir="${HOME}/private-sources"
agents_md_host=fixture-host
agents-md-sync
'''
        env = dict(os.environ, HOME=str(self.home), SYNC_TEST_ROOT=str(ROOT))
        env.pop("agents_md_sync_verbose_p", None)
        if quiet:
            env["agents_md_sync_verbose_p"] = "n"
        result = subprocess.run(
            ["zsh", "-f", "-c", script], cwd=ROOT,
            env=env, capture_output=True, text=True, timeout=15,
        )
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout, "")
        return result

    def test_reports_creation_and_update(self):
        created = self.sync()
        self.assertEqual(created.stderr, "agents-md-sync: wrote ~/installed/AGENTS.md\n")
        self.assertIn("Synthetic shared instructions.", self.target.read_text())
        self.source.write_text("Revised synthetic instructions.\n")
        updated = self.sync()
        self.assertEqual(updated.stderr, "agents-md-sync: wrote ~/installed/AGENTS.md\n")
        self.assertIn("Revised synthetic instructions.", self.target.read_text())

    def test_unchanged_run_is_silent_and_keeps_mtime(self):
        self.sync()
        mtime = self.target.stat().st_mtime_ns
        result = self.sync()
        self.assertEqual(result.stderr, "")
        self.assertEqual(self.target.stat().st_mtime_ns, mtime)

    def test_overlay_removal_is_reported_once(self):
        self.sync()
        self.overlay.write_text("Stale synthetic overlay.\n")
        result = self.sync()
        self.assertEqual(result.stderr, "agents-md-sync: removed empty ~/installed/overlay.md\n")
        self.assertFalse(self.overlay.exists())
        self.assertEqual(self.sync().stderr, "")

    def test_symlink_replacement_is_reported(self):
        self.sync()
        other = self.home / "other.md"
        other.write_bytes(self.target.read_bytes())
        self.target.unlink()
        self.target.symlink_to(other)
        original = other.read_bytes()
        result = self.sync()
        self.assertEqual(result.stderr, "agents-md-sync: wrote ~/installed/AGENTS.md\n")
        self.assertFalse(self.target.is_symlink())
        self.assertEqual(other.read_bytes(), original)

    def test_quiet_override_suppresses_change_reports(self):
        result = self.sync(quiet=True)
        self.assertEqual(result.stderr, "")
        self.assertTrue(self.target.is_file())


if __name__ == "__main__":
    unittest.main()
