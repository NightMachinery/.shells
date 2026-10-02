import importlib.util
import json
from pathlib import Path
import sys
import tempfile
import unittest
from unittest.mock import patch

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
import paseo_auto_continue as mod


class PaseoAutoContinueTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.home = Path(self.temp.name) / 'daemon'
        self.home.mkdir()
        self.provider = 'codex-work'
        self.config = {'agents': {'providers': {self.provider: {
            'extends': 'codex', 'label': 'Work', 'env': {'CODEX_HOME': '/tmp/example-codex-work'}}}}}
        self.write_config()
        self.snapshot = {'Id': 'agent-id', 'Provider': self.provider, 'Cwd': '/tmp/project',
                         'Status': 'error', 'Archived': False, 'PendingPermissions': []}
        self.record = {'id': 'agent-id', 'lastStatus': 'error', 'lastError': 'Usage limit reached',
                       'lastUserMessageAt': 'turn-1', 'attentionTimestamp': 'error-1'}
        self.state = {'enabled': True, 'agent_id': 'agent-id', 'home': str(self.home),
                      'binding': mod.binding(self.snapshot, self.home), 'poll': 300}
        self.path = Path(self.temp.name) / 'state.json'
        mod.atomic_write(self.path, self.state)
        self.sent = []

    def write_config(self):
        (self.home / 'config.json').write_text(json.dumps(self.config))

    def tick(self, quota=True, snapshots=None, records=None, send=None):
        snap_iter = iter(snapshots) if snapshots else None
        rec_iter = iter(records) if records else None
        return mod.tick(self.path,
                        inspect_fn=lambda *_: next(snap_iter) if snap_iter else self.snapshot,
                        stored_fn=lambda *_: next(rec_iter) if rec_iter else self.record,
                        quota_fn=lambda *_: quota,
                        send_fn=send or self.sent.append)

    def test_quota_recovery_sends_to_exact_agent_and_home(self):
        self.assertIn('sent', self.tick())
        argv = self.sent[0]
        self.assertEqual(argv[:3], ['paseo', 'send', 'agent-id'])
        self.assertEqual(argv[argv.index('--home') + 1], str(self.home))
        self.assertIn('--no-wait', argv)
        self.assertEqual(len(self.sent), 1)

    def test_same_failure_sends_once_across_restart(self):
        self.tick()
        self.tick()
        self.assertEqual(len(self.sent), 1)

    def test_new_failed_turn_can_continue(self):
        self.tick()
        self.record['lastUserMessageAt'] = 'turn-2'
        self.record['attentionTimestamp'] = 'error-2'
        self.tick()
        self.assertEqual(len(self.sent), 2)

    def test_exhausted_account_waits(self):
        self.assertEqual(self.tick(quota=False), 'waiting for quota recovery')
        self.assertFalse(self.sent)
        self.tick()
        self.assertEqual(len(self.sent), 1)

    def test_idle_agent_never_continues(self):
        self.snapshot['Status'] = 'idle'
        self.record['lastStatus'] = 'idle'
        self.tick()
        self.assertFalse(self.sent)

    def test_nonquota_failure_never_continues(self):
        self.record['lastError'] = 'Network connection failed'
        self.tick()
        self.assertFalse(self.sent)

    def test_running_agent_never_continues(self):
        self.snapshot['Status'] = 'running'
        self.tick()
        self.assertFalse(self.sent)

    def test_pending_permission_never_continues(self):
        self.snapshot['PendingPermissions'] = ['permission']
        self.tick()
        self.assertFalse(self.sent)

    def test_archived_agent_disables(self):
        self.snapshot['Archived'] = True
        self.tick()
        self.assertFalse(mod.read_json(self.path)['enabled'])
        self.assertFalse(self.sent)

    def test_replaced_agent_disables(self):
        self.snapshot['Id'] = 'another-agent'
        self.tick()
        self.assertFalse(mod.read_json(self.path)['enabled'])

    def test_profile_change_disables(self):
        self.config['agents']['providers'][self.provider]['env']['CODEX_HOME'] = '/tmp/other-home'
        self.write_config()
        self.tick()
        self.assertFalse(mod.read_json(self.path)['enabled'])
        self.assertFalse(self.sent)

    def test_labels_do_not_change_profile_binding(self):
        self.config['agents']['providers'][self.provider]['label'] = 'Better label'
        self.write_config()
        self.tick()
        self.assertEqual(len(self.sent), 1)

    def test_manual_continuation_during_quota_query_skips(self):
        fresh = dict(self.snapshot, Status='running')
        self.tick(snapshots=[self.snapshot, fresh])
        self.assertFalse(self.sent)

    def test_new_turn_during_quota_query_skips(self):
        fresh = dict(self.record, lastUserMessageAt='new-user-turn')
        self.tick(records=[self.record, fresh])
        self.assertFalse(self.sent)

    def test_send_failure_claimed_and_disabled(self):
        def fail(argv):
            self.assertEqual(mod.read_json(self.path)['status'], 'sending continuation')
            raise mod.HandoffError('timeout')
        with self.assertRaises(mod.HandoffError):
            self.tick(send=fail)
        state = mod.read_json(self.path)
        self.assertFalse(state['enabled'])
        self.assertTrue(state['claimed_failure'])
        self.tick()
        self.assertFalse(self.sent)

    def test_account_query_failure_does_not_send(self):
        def unavailable(*args):
            raise mod.HandoffError('unavailable')
        with self.assertRaises(mod.HandoffError):
            mod.tick(self.path, inspect_fn=lambda *_: self.snapshot,
                     stored_fn=lambda *_: self.record, quota_fn=unavailable, send_fn=self.sent.append)
        self.assertFalse(self.sent)
        self.assertTrue(mod.read_json(self.path)['enabled'])

    def test_off_does_not_inspect_or_send(self):
        self.state['enabled'] = False
        mod.atomic_write(self.path, self.state)
        self.assertEqual(self.tick(), 'off')
        self.assertFalse(self.sent)

    def test_claude_profile_resolution_and_inheritance(self):
        work = str(Path.home() / '.claude-work')
        self.config = {'agents': {'providers': {'named': {'extends': 'intermediate'},
                        'intermediate': {'extends': 'claude', 'env': {'CLAUDE_CONFIG_DIR': work}}}}}
        self.write_config()
        identity, _ = mod.provider_config(self.home, 'named')
        self.assertEqual(identity['profile'], 'work')
        self.assertEqual(identity['native_home'], str(Path(work).resolve()))
        identity, _ = mod.provider_config(self.home, 'claude')
        self.assertEqual(identity['profile'], 'default')

    def test_provider_cycle_refused(self):
        self.config['agents']['providers'][self.provider]['extends'] = self.provider
        self.write_config()
        with self.assertRaises(mod.HandoffError):
            mod.provider_config(self.home, self.provider)

    def test_codex_quota_verdict(self):
        self.assertTrue(mod.quota_usable({'ok': True, 'quota': {'blocked': False}}, 'codex'))
        self.assertFalse(mod.quota_usable({'ok': True, 'quota': {'blocked': True}}, 'codex'))
        with self.assertRaises(mod.HandoffError):
            mod.quota_usable({'ok': False}, 'codex')

    def test_claude_scoped_weekly_window(self):
        payload = {'windows': [{'key': 'session', 'utilization_percent': 20},
                    {'key': 'weekly_scoped', 'label': '7d Opus', 'utilization_percent': 100}]}
        self.assertFalse(mod.quota_usable(payload, 'claude', 'opus'))
        self.assertTrue(mod.quota_usable(payload, 'claude', 'sonnet'))

    def test_claude_missing_verdict_refused(self):
        with self.assertRaises(mod.HandoffError):
            mod.quota_usable({'error': 'auth unavailable'}, 'claude')
        with self.assertRaises(mod.HandoffError):
            mod.quota_usable({'windows': []}, 'claude')

    def test_codex_quota_reader_uses_bound_home_and_no_all(self):
        with patch.object(mod, 'command', return_value='{"ok":true,"quota":{"blocked":false}}') as run:
            self.assertTrue(mod.quota(self.state, self.snapshot))
        argv = run.call_args.args[0]
        env = run.call_args.kwargs['env']
        self.assertIn('--no-all', argv)
        self.assertEqual(env['CODEX_HOME'], str(Path('/tmp/example-codex-work').resolve()))

    def test_claude_explicit_default_config_dir_preserved(self):
        self.config = {'agents': {'providers': {'named': {'extends': 'claude',
                       'env': {'CLAUDE_CONFIG_DIR': str(Path.home() / '.claude')}}}}}
        self.write_config()
        snapshot = dict(self.snapshot, Provider='named', Model='claude-opus-4-8')
        state = dict(self.state, binding=mod.binding(snapshot, self.home))
        with patch.object(mod, 'command', return_value='{"windows":[{"key":"session","utilization_percent":0}]}') as run:
            self.assertTrue(mod.quota(state, snapshot))
        argv = run.call_args.args[0]
        self.assertEqual(argv[argv.index('--config-dir') + 1], str(Path.home() / '.claude'))

    def test_repeated_on_preserves_consumed_failure(self):
        self.state.update(claimed_failure='already-sent', status='already continued')
        mod.atomic_write(self.path, self.state)
        # main derives its file name from daemon home + exact agent UUID.
        agent_id = '00000000-0000-4000-8000-000000000001'
        directory = Path(self.temp.name) / 'private'
        directory.mkdir(mode=0o700)
        target = directory / (mod.digest(str(self.home))[:16] + '-' + agent_id + '.json')
        mod.atomic_write(target, self.state)
        snapshot = dict(self.snapshot, Id=agent_id)
        with patch.object(sys, 'argv', ['watcher', 'on', agent_id, '--home', str(self.home),
                                       '--state-dir', str(directory)]), \
             patch.object(mod, 'inspect', return_value=snapshot), \
             patch.object(mod, 'stored_agent', return_value=self.record), \
             patch.object(mod, 'start_worker'):
            mod.main()
        self.assertEqual(mod.read_json(target)['claimed_failure'], 'already-sent')

    def test_registration_mode_is_private(self):
        self.assertEqual(self.path.stat().st_mode & 0o777, 0o600)

    def test_registration_symlink_refused(self):
        symlink = Path(self.temp.name) / 'link'
        symlink.symlink_to(self.path)
        with self.assertRaises(mod.HandoffError):
            mod.atomic_write(symlink, {})

    def test_private_dir_symlink_refused(self):
        target = Path(self.temp.name) / 'private'
        target.mkdir(mode=0o700)
        link = Path(self.temp.name) / 'link'
        link.symlink_to(target)
        with self.assertRaises(mod.HandoffError):
            mod.private_dir(link)


if __name__ == '__main__':
    unittest.main()
