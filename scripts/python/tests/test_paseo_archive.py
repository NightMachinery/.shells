"""Shell integration using a fake CLI; no real Paseo agent can be archived."""
import json
import os
from pathlib import Path
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[2]
CALLER = '11111111-1111-1111-1111-111111111111'
PARENT = '22222222-2222-2222-2222-222222222222'
IDLE = '33333333-3333-3333-3333-333333333333'
RUNNING = '44444444-4444-4444-4444-444444444444'
OTHER = '55555555-5555-5555-5555-555555555555'


class ArchiveTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.tmp = Path(self.temp.name).resolve()
        self.env = dict(os.environ, PATH=str(self.tmp) + ':' + os.environ['PATH'], PASEO_AGENT_ID=CALLER,
                        PASEO_PARENT_AGENT_ID=PARENT, TEST_ARCHIVE_DIR=str(self.tmp))
        self.agents = [dict(id=i, status=s, name=t, created='1 hour ago', provider='fixture/model', cwd=str(self.tmp))
                       for i, s, t in [(CALLER, 'idle', 'caller'), (PARENT, 'idle', 'parent'), (IDLE, 'idle', 'idle test'), (RUNNING, 'running', 'busy')]]
        self.agents += [dict(id=OTHER, status='idle', name='other dir', cwd='/elsewhere')]
        (self.tmp / 'rows.json').write_text(json.dumps(self.agents))
        fake = self.tmp / 'paseo'
        fake.write_text('#!/usr/bin/env python3\nimport os,json,sys\nfrom pathlib import Path\np=Path(os.environ["TEST_ARCHIVE_DIR"])\nif sys.argv[1]=="ls": print((p/"rows.json").read_text())\nelse:\n with (p/"calls").open("a") as f: f.write(json.dumps(sys.argv[1:])+"\\n")\n print("archived",sys.argv[2])\n')
        fake.chmod(0o755)

    def shell(self, command):
        script = '''
source "$1/zshlang/basic/basic.plugin.zsh"
source "$1/zshlang/auto-load/others/paseo.zsh"
function paseo-ls { print -r -- '[]' }
function fzf-gateway { command fzf "$@" }
'''
        return subprocess.run(['zsh', '-f', '-c', script + command, '_', str(ROOT)], cwd=self.tmp, env=self.env, text=True, capture_output=True)

    def calls(self):
        f = self.tmp / 'calls'
        return [json.loads(s) for s in f.read_text().splitlines()] if f.exists() else []

    def test_default_scope_is_global(self):
        r = self.shell('paseo-archive-all --dry-run')
        self.assertEqual(r.returncode, 0, r.stderr)
        self.assertIn('idle test', r.stdout)
        self.assertIn('other dir', r.stdout)
        self.assertEqual(self.calls(), [])

    def test_local_dir_empty_message(self):
        (self.tmp / 'empty').mkdir()
        r = self.shell('cd empty && paseo-archive-all -l --dry-run')
        self.assertEqual(r.returncode, 0, r.stderr)
        self.assertIn('drop --local-dir', r.stdout)

    def test_all_dry_scope_protection_and_running(self):
        r = self.shell('paseo-archive-all --dry-run --local-dir')
        self.assertEqual(r.returncode, 0, r.stderr)
        self.assertIn('idle test', r.stdout)
        for label in ['caller', 'parent', 'busy', 'other dir']:
            self.assertNotIn(label, r.stdout)
        self.assertEqual(self.calls(), [])

    def test_nonterminal_requires_yes(self):
        r = self.shell('paseo-archive-all')
        self.assertNotEqual(r.returncode, 0)
        self.assertIn('--yes', r.stderr)
        self.assertEqual(self.calls(), [])

    def test_yes_archives_only_eligible_and_global_scope(self):
        r = self.shell('paseo-archive-all --yes --global')
        self.assertEqual(r.returncode, 0, r.stderr)
        self.assertEqual({c[1] for c in self.calls()}, {IDLE, OTHER})
        self.assertTrue(all('--force' not in c for c in self.calls()))

    def test_picker_filter_selects_full_id(self):
        r = self.shell('paseo-archive-fz --filter "idle test"')
        self.assertEqual(r.returncode, 0, r.stderr)
        self.assertEqual(self.calls(), [['archive', IDLE]])

    def test_picker_protected_refused_and_running_marked(self):
        r = self.shell('paseo-archive-fz --filter caller')
        self.assertNotEqual(r.returncode, 0)
        self.assertEqual(self.calls(), [])
        r = self.shell('paseo-archive-fz --dry-run --filter busy')
        self.assertIn('RUNNING!', r.stdout)
        self.assertEqual(self.calls(), [])

    def test_parent_label_used_without_environment_parent(self):
        self.env.pop('PASEO_PARENT_AGENT_ID')
        records = json.dumps([{'id': CALLER, 'parentAgentId': PARENT}])
        r = self.shell("function paseo-ls { print -r -- '" + records + "' }; paseo-archive-all --dry-run")
        self.assertNotIn('parent', r.stdout)
        self.assertIn('idle test', r.stdout)
