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

    def sync(self, quiet=False, rules=False, max_bytes=None, check=True):
        script = r'''
unset night_basic_plugin_loaded_p
source "${SYNC_TEST_ROOT}/zshlang/basic/basic.plugin.zsh" || exit $?
source "${SYNC_TEST_ROOT}/zshlang/auto-load/others/agents-md.zsh" || exit $?
function ecgray { print -ru2 -- "$@"; }
function ecerr { print -ru2 -- "$@"; }
function h-agents-md-agents {
    printf 'fixture\t%s\n' "${HOME}/installed/AGENTS.md"
    printf 'fixture-overlay\t%s\toverlay\n' "${HOME}/installed/overlay.md"
    if test -n "${SYNC_TEST_RULES}" ; then
        printf 'fixture-rules\t%s\trules\n' "${HOME}/installed/rules"
    fi
}
function h-agents-md-retired {
    print -r -- "${HOME}/installed/retired.md"
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
        if rules:
            env["SYNC_TEST_RULES"] = "y"
        if max_bytes is not None:
            env["agents_md_rule_max_bytes"] = str(max_bytes)
        result = subprocess.run(
            ["zsh", "-f", "-c", script], cwd=ROOT,
            env=env, capture_output=True, text=True, timeout=15,
        )
        if check:
            self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout, "")
        return result

    def rule_files(self):
        return sorted((self.installed / "rules").glob("agents-md-*.md"))

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

    #: Blank lines between sections, as in the real sources: a chunk that
    #: kept one would read back different and be rewritten on every sync.
    SECTIONS = (
        "# One\nalpha alpha alpha alpha alpha alpha alpha alpha alpha alpha\n\n",
        "# Two\n```zsh\n# not a heading, a comment in a sample\n```\nbeta beta beta beta\n\n",
        "# Three\n## Three A\ngamma gamma gamma gamma gamma gamma gamma gamma\n\n"
        "## Three B\ndelta delta delta delta delta delta delta delta\n",
    )
    PREAMBLE = "---\ntrigger: always_on\n---\n\n"
    HEADER = "<!-- Assembled by agents-md-sync. Edit the sources, not this file. -->"

    def test_rules_are_split_under_the_limit_and_lose_nothing(self):
        self.source.write_text("".join(self.SECTIONS))
        result = self.sync(rules=True, max_bytes=110)
        files = self.rule_files()
        self.assertGreater(len(files), 2)
        self.assertEqual([f.name for f in files],
                         [f"agents-md-{i:02d}.md" for i in range(1, len(files) + 1)])
        for f in files:
            self.assertIn(f"wrote ~/installed/rules/{f.name}", result.stderr)
        bodies = []
        for f in files:
            text = f.read_text()
            self.assertTrue(text.startswith(self.PREAMBLE + self.HEADER + "\n"), text)
            body = text[len(self.PREAMBLE + self.HEADER) + 1:].rstrip("\n")
            self.assertLessEqual(len(body.encode()), 110, body)
            self.assertFalse(body.startswith("# not a heading"), body)
            bodies.append(body)
        # Only the blank lines at the cuts are dropped.
        lines = lambda text: [l for l in text.splitlines() if l.strip()]
        self.assertEqual(lines(self.HEADER + "\n" + "\n".join(bodies)), lines(self.target.read_text()))
        # The source's provenance comment travels with its first heading.
        self.assertTrue(bodies[0].startswith("<!-- ~/sources/AGENTS.md -->\n# One"), bodies[0])
        # Nothing changed, nothing written.
        self.assertEqual(self.sync(rules=True, max_bytes=110).stderr, "")

    def test_rules_shrinking_removes_extra_files(self):
        self.source.write_text("".join(self.SECTIONS))
        self.sync(rules=True, max_bytes=110)
        before = len(self.rule_files())
        self.source.write_text(self.SECTIONS[0])
        result = self.sync(rules=True, max_bytes=110)
        self.assertEqual(len(self.rule_files()), 1)
        for i in range(2, before + 1):
            self.assertIn(f"removed extra ~/installed/rules/agents-md-{i:02d}.md", result.stderr)

    def test_rules_refuse_a_section_too_big_to_split(self):
        self.source.write_text("# Big\n" + "word " * 60 + "\n")
        result = self.sync(rules=True, max_bytes=110, check=False)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("no subheadings is over 110 bytes", result.stderr)
        self.assertEqual(self.rule_files(), [])

    def test_retired_output_is_removed_only_when_ours(self):
        retired = self.installed / "retired.md"
        retired.write_text(self.HEADER + "\nOld synthetic instructions.\n")
        result = self.sync()
        self.assertIn("removed retired ~/installed/retired.md", result.stderr)
        self.assertFalse(retired.exists())
        retired.write_text("Hand-written synthetic rules.\n")
        self.assertEqual(self.sync().stderr, "")
        self.assertEqual(retired.read_text(), "Hand-written synthetic rules.\n")

    def test_sections_ignore_headings_in_code_fences(self):
        script = r'''
unset night_basic_plugin_loaded_p
source "${SYNC_TEST_ROOT}/zshlang/basic/basic.plugin.zsh" || exit $?
source "${SYNC_TEST_ROOT}/zshlang/auto-load/others/agents-md.zsh" || exit $?
h-agents-md-sections 1 "${SYNC_TEST_TEXT}" || exit $?
print -rl -- "${#reply}" "${reply[@]}"
'''
        text = "".join(self.SECTIONS).rstrip("\n")
        env = dict(os.environ, HOME=str(self.home), SYNC_TEST_ROOT=str(ROOT), SYNC_TEST_TEXT=text)
        result = subprocess.run(["zsh", "-f", "-c", script], cwd=ROOT, env=env,
                                capture_output=True, text=True, timeout=15)
        self.assertEqual(result.returncode, 0, result.stderr)
        count, _, rest = result.stdout.partition("\n")
        self.assertEqual(count, "3", result.stdout)
        self.assertEqual(rest.rstrip("\n"), text)


if __name__ == "__main__":
    unittest.main()
