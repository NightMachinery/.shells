"""Integration tests with a fake CLI and isolated daemon records; never real agents."""
from datetime import datetime, timedelta, timezone
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest
sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from paseo_archive import duration

ROOT = Path(__file__).resolve().parents[2]
CALLER = '11111111-1111-1111-1111-111111111111'
PARENT = '22222222-2222-2222-2222-222222222222'
IDLE = '33333333-3333-3333-3333-333333333333'
RUNNING = '44444444-4444-4444-4444-444444444444'
OTHER = '55555555-5555-5555-5555-555555555555'
GRAND = '66666666-6666-6666-6666-666666666666'


class ArchiveTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.tmp = Path(self.temp.name).resolve()
        self.home = self.tmp / 'daemon'
        self.record_dir = self.home / 'agents' / 'fixture'
        self.record_dir.mkdir(parents=True)
        self.env = dict(os.environ, PATH=str(self.tmp) + ':' + os.environ['PATH'], PASEO_AGENT_ID=CALLER,
                        PASEO_PARENT_AGENT_ID=PARENT, TEST_ARCHIVE_DIR=str(self.tmp), PASEO_HOME=str(self.home), NIGHTDIR=str(ROOT))
        self.env.pop('PASEO_HOST', None)
        self.env.pop('paseo_home', None)
        self.agents = [dict(id=i, status=s, name=t, created='10 years ago', provider='fixture/model', cwd=str(self.tmp))
                       for i, s, t in [(CALLER, 'idle', 'caller'), (PARENT, 'idle', 'parent'), (IDLE, 'idle', 'old test'), (RUNNING, 'running', 'busy')]]
        self.agents += [dict(id=OTHER, status='idle', name='other dir', cwd='/elsewhere')]
        (self.tmp / 'rows.json').write_text(json.dumps(self.agents))
        for a in self.agents:
            self.record(a['id'], title=a['name'], cwd=a['cwd'])
        fake = self.tmp / 'paseo'
        fake.write_text('''#!/usr/bin/env python3
import os,json,sys
from pathlib import Path
p=Path(os.environ['TEST_ARCHIVE_DIR'])
if sys.argv[1]=='ls': print((p/'rows.json').read_text())
else:
 with (p/'calls').open('a') as f: f.write(json.dumps(sys.argv[1:])+'\\n')
 print('updated' if sys.argv[1]=='agent' else 'archived',sys.argv[-1])
''')
        fake.chmod(0o755)

    def ago(self, hours):
        return (datetime.now(timezone.utc) - timedelta(hours=hours)).isoformat()

    def record(self, ident, **updates):
        file = self.record_dir / (ident + '.json')
        old = json.loads(file.read_text()) if file.exists() else dict(id=ident, title='fixture', cwd=str(self.tmp), labels={},
              createdAt=self.ago(1000), updatedAt=self.ago(100), lastActivityAt=self.ago(100), archivedAt=None)
        old.update(updates)
        file.write_text(json.dumps(old))

    def shell(self, command):
        script = '''
source "$1/zshlang/basic/basic.plugin.zsh"
source "$1/zshlang/auto-load/others/paseo.zsh"
function ecerr { print -ru2 -- "$@" }
function fzf-gateway { command fzf "$@" }
'''
        return subprocess.run(['zsh', '-f', '-c', script + command, '_', str(ROOT)], cwd=self.tmp, env=self.env, text=True, capture_output=True)

    def calls(self):
        f = self.tmp / 'calls'
        return [json.loads(s) for s in f.read_text().splitlines()] if f.exists() else []

    def test_default_scope_global_and_default_idle(self):
        self.record(OTHER, lastActivityAt=self.ago(71))
        r = self.shell('paseo-archive-all --dry-run')
        self.assertEqual(r.returncode, 0, r.stderr)
        self.assertIn('LAST ACTIVE', r.stdout)
        self.assertIn('old test', r.stdout)
        self.assertNotIn('other dir', r.stdout)
        self.assertNotIn('busy', r.stdout)
        self.assertEqual(self.calls(), [])

    def test_old_creation_recent_activity_not_offered(self):
        self.record(IDLE, lastActivityAt=self.ago(1))
        for cmd in ['paseo-archive-all -n', 'paseo-archive-fz -n --filter "old test"']:
            r = self.shell(cmd)
            self.assertNotIn('old test', r.stdout)
        self.assertEqual(self.calls(), [])

    def test_fallbacks_and_oldest_first(self):
        self.record(IDLE, lastActivityAt=None, updatedAt=self.ago(90))
        self.record(OTHER, lastActivityAt=None, updatedAt=None, createdAt=self.ago(200))
        r = self.shell('paseo-archive-all -n')
        self.assertEqual(r.returncode, 0, r.stderr)
        self.assertLess(r.stdout.index('other dir'), r.stdout.index('old test'))

    def test_age_overrides(self):
        self.record(IDLE, lastActivityAt=self.ago(1))
        for flags in ['--all-ages', '--min-idle 0', '--min-idle 90m', '--min-idle 3d']:
            r = self.shell('paseo-archive-all -n ' + flags)
            self.assertEqual(r.returncode, 0, r.stderr)
            self.assertEqual('old test' in r.stdout, flags in ['--all-ages', '--min-idle 0'])
        self.assertEqual(duration('72h'), duration('3d'))
        self.assertEqual(duration('90m'), 5400)
        self.assertNotEqual(self.shell('paseo-archive-all -n --min-idle rubbish').returncode, 0)

    def test_parent_running_child_and_force(self):
        self.record(RUNNING, labels={'paseo.parent-agent-id': IDLE})
        r = self.shell('paseo-archive-all -n')
        self.assertNotIn('old test', r.stdout)
        r = self.shell('paseo-archive-fz -n --force --filter "old test"')
        self.assertNotIn('old test', r.stdout)
        self.assertIn('parents of running subagents', r.stdout)
        self.assertEqual(self.calls(), [])

    def test_parent_recent_child(self):
        self.record(OTHER, lastActivityAt=self.ago(1), labels={'paseo.parent-agent-id': IDLE})
        r = self.shell('paseo-archive-all -n')
        self.assertNotIn('old test', r.stdout)

    def test_eligible_children_listed_and_requests_deduplicated(self):
        self.record(OTHER, labels={'paseo.parent-agent-id': IDLE})
        r = self.shell('paseo-archive-all --yes')
        self.assertEqual(r.returncode, 0, r.stderr)
        self.assertIn('archives 1 subagents with it:', r.stdout)
        self.assertIn('other dir [idle]', r.stdout)
        self.assertEqual(self.calls(), [['archive', IDLE]])

    def test_pin_and_pinned_grandchild_block_force_cascade(self):
        self.record(OTHER, labels={'paseo.parent-agent-id': IDLE})
        self.record(RUNNING, labels={'paseo.parent-agent-id': OTHER, 'keep': 'true'})
        r = self.shell('paseo-archive-fz --force -n --filter "old test"')
        self.assertNotIn('old test', r.stdout)
        r = self.shell('paseo-archive-fz --force -n --filter busy')
        self.assertNotIn('busy', r.stdout)
        self.assertEqual(self.calls(), [])

    def test_pin_parent_protected_without_caller_environment(self):
        self.env.pop('PASEO_AGENT_ID')
        self.env.pop('PASEO_PARENT_AGENT_ID')
        self.record(IDLE, labels={'keep': 'true'})
        r = self.shell('paseo-archive-fz --force -n --filter "old test"')
        self.assertNotIn('old test', r.stdout)

    def test_local_dir_and_cascade_scope(self):
        self.record(OTHER, labels={'paseo.parent-agent-id': IDLE})
        r = self.shell('paseo-archive-all -l -n')
        self.assertNotIn('old test', r.stdout)
        self.assertNotIn('other dir', r.stdout)
        (self.tmp / 'empty').mkdir()
        self.assertIn('drop --local-dir', self.shell('cd empty && paseo-archive-all -l -n').stdout)

    def test_nonterminal_confirmation_and_picker_filter(self):
        r = self.shell('paseo-archive-all')
        self.assertNotEqual(r.returncode, 0)
        self.assertIn('--yes', r.stderr)
        self.assertEqual(self.calls(), [])
        r = self.shell('paseo-archive-fz --filter "old test"')
        self.assertEqual(r.returncode, 0, r.stderr)
        self.assertEqual(self.calls(), [['archive', IDLE]])

    def test_caller_and_parent_cannot_be_forced(self):
        self.env.pop('PASEO_PARENT_AGENT_ID')
        self.record(CALLER, labels={'paseo.parent-agent-id': PARENT})
        r = self.shell('paseo-archive-fz --force -n --filter parent')
        self.assertNotIn('parent', r.stdout)
        r = self.shell('paseo-archive-all -n')
        self.assertNotIn('\tcaller\t', r.stdout)
        self.assertIn('old test', r.stdout)

    def test_unknown_records_fail_closed(self):
        (self.record_dir / (IDLE + '.json')).unlink()
        r = self.shell('paseo-archive-all -n')
        self.assertNotIn('old test', r.stdout)
        (self.record_dir / 'corrupt.json').write_text('{')
        self.assertNotEqual(self.shell('paseo-archive-all --yes').returncode, 0)
        self.assertEqual(self.calls(), [])

    def test_pin_commands_only_set_the_requested_label(self):
        self.assertEqual(self.shell('paseo-keep ' + IDLE).returncode, 0)
        self.assertEqual(self.shell('paseo-unkeep ' + IDLE).returncode, 0)
        self.assertEqual(self.calls(), [['agent','update', IDLE, '--label', 'keep=true'], ['agent','update', IDLE, '--label', 'keep=false']])

    def test_all_never_accepts_force(self):
        r = self.shell('paseo-archive-all --force --yes')
        self.assertNotEqual(r.returncode, 0)
        self.assertEqual(self.calls(), [])

    def test_cycle_never_offered_even_force(self):
        self.record(IDLE, labels={'paseo.parent-agent-id': OTHER})
        self.record(OTHER, labels={'paseo.parent-agent-id': IDLE})
        r = self.shell('paseo-archive-fz --force -n --filter "old test"')
        self.assertNotIn('old test', r.stdout)
        self.assertEqual(self.calls(), [])

    def test_pinned_after_selection_blocks_request(self):
        mutation = '''
function fzf-gateway {
    command python3 - <<'CODE'
import json,os
from pathlib import Path
p=Path(os.environ['PASEO_HOME'])/'agents'/'fixture'/'33333333-3333-3333-3333-333333333333.json'
d=json.loads(p.read_text());d['labels']['keep']='true';p.write_text(json.dumps(d))
CODE
    command fzf "$@"
}
paseo-archive-fz --filter "old test"
'''
        r = self.shell(mutation)
        self.assertNotEqual(r.returncode, 0)
        self.assertIn('eligibility or cascade changed', r.stderr)
        self.assertEqual(self.calls(), [])

    def test_new_child_after_selection_requires_new_confirmation(self):
        mutation = '''
function fzf-gateway {
    command python3 - <<'CODE'
import json,os
from pathlib import Path
p=Path(os.environ['PASEO_HOME'])/'agents'/'fixture'/'55555555-5555-5555-5555-555555555555.json'
d=json.loads(p.read_text());d['labels']['paseo.parent-agent-id']='33333333-3333-3333-3333-333333333333';p.write_text(json.dumps(d))
CODE
    command fzf "$@"
}
paseo-archive-fz --filter "old test"
'''
        r = self.shell(mutation)
        self.assertNotEqual(r.returncode, 0)
        self.assertIn('eligibility or cascade changed', r.stderr)
        self.assertEqual(self.calls(), [])


    def test_show_hidden_has_reasons_but_cannot_select_hidden_rows(self):
        self.record(IDLE, labels={'keep': 'true'})
        r = self.shell('paseo-archive-fz --force --show-hidden --filter "old test"')
        self.assertIn('HIDDEN 33333333', r.stdout)
        self.assertIn('REASON: pinned', r.stdout)
        self.assertEqual(self.calls(), [])
        r = self.shell('paseo-archive-all --show-hidden --dry-run')
        self.assertIn('REASON: running', r.stdout)
        self.assertIn('--show-hidden lists them', r.stdout)

    def test_all_caller_ancestors_protected_and_running_leaf_forceable(self):
        self.env.pop('PASEO_PARENT_AGENT_ID')
        self.record(CALLER, labels={'paseo.parent-agent-id': PARENT})
        self.record(PARENT, labels={'paseo.parent-agent-id': IDLE})
        r = self.shell('paseo-archive-fz --force --show-hidden -n --filter "old test"')
        self.assertIn('REASON: caller', r.stdout)
        self.assertEqual(self.calls(), [])
        r = self.shell('paseo-archive-fz --force -n --filter busy')
        self.assertIn('RUNNING!', r.stdout)

    def test_running_grandchild_blocks_force_even_if_child_recent(self):
        self.record(OTHER, lastActivityAt=self.ago(1), labels={'paseo.parent-agent-id': IDLE})
        self.record(RUNNING, labels={'paseo.parent-agent-id': OTHER})
        r = self.shell('paseo-archive-fz --force --show-hidden -n --filter "old test"')
        self.assertIn('REASON: running subagent 44444444', r.stdout)
        self.assertEqual(self.calls(), [])
