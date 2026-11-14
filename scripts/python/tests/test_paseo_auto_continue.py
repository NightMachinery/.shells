import importlib.util
import json
from pathlib import Path
import sys
import tempfile
import time
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

    def test_stopped_agents_scan(self):
        agents = self.home / 'agents' / '-tmp-project'
        agents.mkdir(parents=True)
        claude_quota = {'id': 'claude-quota', 'provider': 'claude', 'lastStatus': 'idle'}
        records = [
            {'id': 'codex-quota', 'provider': self.provider, 'lastStatus': 'closed',
             'lastError': 'You have hit your usage limit.'},
            {'id': 'codex-other-error', 'provider': self.provider, 'lastStatus': 'error',
             'lastError': 'Bad Request'},
            {'id': 'codex-archived', 'provider': self.provider, 'lastStatus': 'error',
             'lastError': 'Usage limit reached', 'archivedAt': '2026-11-01T00:00:00Z'},
            {'id': 'codex-running', 'provider': self.provider, 'lastStatus': 'running',
             'lastError': 'Usage limit reached'},
            claude_quota,
            {'id': 'claude-finished', 'provider': 'claude', 'lastStatus': 'idle'},
        ]
        for record in records:
            (agents / (record['id'] + '.json')).write_text(json.dumps(record))
        found = sorted(mod.stopped_agents(
            self.home, transcript_fn=lambda record, identity: record['id'] == 'claude-quota'))
        self.assertEqual(found, ['claude-quota', 'codex-quota'])

    def test_usable_agents_filter(self):
        records = {
            'work-opus': {'provider': 'claude-work', 'config': {'model': 'claude-opus-5-5'}},
            'work-opus-2': {'provider': 'claude-work', 'config': {'model': 'claude-opus-5-5'}},
            'work-sonnet': {'provider': 'claude-work', 'config': {'model': 'claude-sonnet-5-5'}},
            'codex': {'provider': 'codex', 'runtimeInfo': {'model': 'gpt-6.1'}},
            'broken': {'provider': 'odd', 'config': {}},
        }
        calls, warnings = [], []

        def quota_fn(state, snapshot):
            calls.append((state['binding']['provider'], snapshot['Model']))
            if state['binding']['provider'] == 'odd':
                raise mod.HandoffError('Provider must resolve to Claude or Codex')
            return snapshot['Model'] != 'claude-opus-5-5'

        found = list(mod.usable_agents(
            self.home, ['work-opus', 'work-opus-2', 'work-sonnet', 'codex', 'broken', 'gone'],
            quota_fn=quota_fn, warn=warnings.append,
            checkable_fn=lambda provider: provider != 'odd',
            stored_fn=lambda agent_id, home: records[agent_id] if agent_id in records
            else (_ for _ in ()).throw(mod.HandoffError('missing'))))
        # An agent whose quota cannot be read here is kept, with a note.
        self.assertEqual(found, ['work-sonnet', 'codex', 'broken'])
        # One check per seat and model family, not per agent.
        self.assertEqual(len(calls), 3)
        self.assertEqual(len(warnings), 2)

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

    def test_registrations_lists_enabled_only(self):
        directory = Path(self.temp.name) / 'regs'
        directory.mkdir()
        mod.atomic_write(directory / 'a.json', self.state)
        mod.atomic_write(directory / 'b.json', {**self.state, 'agent_id': 'other', 'enabled': False})
        (directory / 'c.json').write_text('not json')
        self.assertEqual(mod.registrations(directory),
                         [('agent-id', str(self.home), 'codex/codex', '')])
        self.assertEqual(mod.registrations(Path(self.temp.name) / 'missing'), [])

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


class CodexLimitTests(unittest.TestCase):
    """Codex limit stops: the last root task_complete in the session log carries the error."""
    thread = '01a1bbc1-518f-7bb0-b332-515f9a0527ee'

    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.native = Path(self.temp.name) / 'codex-home'
        created = int(self.thread.replace('-', '')[:12], 16) / 1000
        day = time.strftime('%Y/%m/%d', time.localtime(created))
        self.log = self.native / 'sessions' / day / ('rollout-2026-11-08T16-22-51-' + self.thread + '.jsonl')
        self.log.parent.mkdir(parents=True)
        self.record = {'id': 'agent-id', 'lastStatus': 'idle', 'persistence': {
            'sessionId': self.thread, 'metadata': {'threadId': self.thread}}}

    def event(self, kind, turn, root=None, error=None):
        payload = {'type': kind, 'turn_id': turn, 'root_turn_id': root or turn}
        if error:
            payload['error'] = {'message': error}
        return {'type': 'event_msg', 'payload': payload}

    def write(self, *entries):
        self.log.write_text(''.join(json.dumps(e) + '\n' for e in entries))

    def failure(self):
        return mod.codex_limit_failure(self.record, {'native_home': str(self.native)})

    def test_limit_on_last_root_turn_is_a_failure(self):
        limit = 'You\u2019ve hit your usage limit. Visit https://chatgpt.com/settings/usage'
        self.write(self.event('task_started', 't1'), self.event('task_complete', 't1', error=limit),
                   self.event('task_complete', 'r1', root='t0'))
        self.assertIsNotNone(self.failure())

    def test_later_turn_or_other_error_is_not(self):
        limit = 'You\u2019ve hit your usage limit.'
        self.write(self.event('task_complete', 't1', error=limit), self.event('task_started', 't2'))
        self.assertIsNone(self.failure())
        self.write(self.event('task_complete', 't1', error='Bad Request'))
        self.assertIsNone(self.failure())
        self.write(self.event('task_complete', 't1'))
        self.assertIsNone(self.failure())

    def test_missing_log_is_not_a_failure(self):
        self.assertIsNone(self.failure())


class ClaudeLimitTests(unittest.TestCase):
    """Claude limit stops: Paseo shows the agent idle, the transcript shows the limit."""
    session = '00000000-0000-4000-8000-0000000000aa'

    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        root = Path(self.temp.name)
        self.home = root / 'daemon'
        self.home.mkdir()
        self.native = root / 'claude-home'
        self.transcript = self.native / 'projects' / '-tmp-project' / (self.session + '.jsonl')
        self.transcript.parent.mkdir(parents=True)
        self.snapshot = {'Id': 'agent-id', 'Provider': 'claude', 'Cwd': '/tmp/project',
                         'Status': 'idle', 'Archived': False, 'PendingPermissions': []}
        self.record = {'id': 'agent-id', 'lastStatus': 'idle', 'lastError': None,
                       'persistence': {'provider': 'claude', 'sessionId': self.session}}
        self.state = {'enabled': True, 'agent_id': 'agent-id', 'home': str(self.home),
                      'binding': mod.binding(self.snapshot, self.home), 'poll': 300}
        self.path = root / 'state.json'
        mod.atomic_write(self.path, self.state)
        self.sent = []

    def write(self, *entries):
        self.transcript.write_text(''.join(json.dumps(e) + '\n' for e in entries))

    def limit(self, uid='limit-1'):
        return {'type': 'assistant', 'uuid': uid, 'isApiErrorMessage': True, 'error': 'rate_limit',
                'message': {'model': '<synthetic>', 'content': [
                    {'type': 'text', 'text': "You've hit your session limit · resets 3am"}]}}

    def failure(self):
        return mod.claude_limit_failure(self.record, {'native_home': str(self.native)})

    def tick(self, transcript_fn=None):
        return mod.tick(self.path, inspect_fn=lambda *_: self.snapshot,
                        stored_fn=lambda *_: self.record, quota_fn=lambda *_: True,
                        send_fn=self.sent.append,
                        transcript_fn=transcript_fn or (lambda *_: self.failure()))

    def test_limit_as_last_entry_is_a_failure(self):
        self.write({'type': 'user', 'message': {'content': 'go'}}, self.limit(),
                   {'type': 'queue-operation'}, {'type': 'last-prompt'})
        self.assertTrue(self.failure())

    def test_later_user_turn_clears_it(self):
        self.write(self.limit(), {'type': 'user', 'message': {'content': 'Continue'}})
        self.assertIsNone(self.failure())

    def test_ordinary_finished_turn_is_not_a_failure(self):
        self.write({'type': 'assistant', 'message': {'content': [{'type': 'text', 'text': 'Done.'}]}})
        self.assertIsNone(self.failure())

    def test_other_api_errors_are_not_quota(self):
        other = dict(self.limit(), error='server_error',
                     message={'content': [{'type': 'text', 'text': 'API Error: 500'}]})
        self.write(other)
        self.assertIsNone(self.failure())

    def test_truncated_tail_line_is_skipped(self):
        self.write({'type': 'user', 'message': {'content': 'x' * 5000}}, self.limit())
        self.assertTrue(mod.claude_limit_failure(self.record, {'native_home': str(self.native)},
                                                 tail_bytes=600))

    def test_missing_or_malformed_session_is_none(self):
        self.record['persistence'] = {'sessionId': '../escape'}
        self.assertIsNone(self.failure())
        self.record['persistence'] = {'sessionId': '00000000-0000-4000-8000-0000000000bb'}
        self.assertIsNone(self.failure())

    def test_idle_claude_agent_after_limit_continues_once(self):
        self.write(self.limit())
        self.assertIn('sent', self.tick())
        self.tick()
        self.assertEqual(len(self.sent), 1)
        self.assertEqual(self.sent[0][:3], ['paseo', 'send', 'agent-id'])

    def test_second_limit_stop_continues_again(self):
        self.write(self.limit('limit-1'))
        self.tick()
        self.write(self.limit('limit-1'), {'type': 'user', 'message': {'content': 'Continue'}},
                   self.limit('limit-2'))
        self.tick()
        self.assertEqual(len(self.sent), 2)

    def test_idle_codex_agent_resumes_on_its_logged_limit(self):
        # Codex also records a limit stop only in its own log; the watcher reads it.
        config = {'agents': {'providers': {'codex-x': {'extends': 'codex'}}}}
        (self.home / 'config.json').write_text(json.dumps(config))
        self.snapshot['Provider'] = 'codex-x'
        self.state['binding'] = mod.binding(self.snapshot, self.home)
        mod.atomic_write(self.path, self.state)
        seen = []
        def codex_log(record, binding):
            seen.append(binding['agent'])
            return 'codex-failure'
        self.tick(transcript_fn=codex_log)
        self.assertEqual(set(seen), {'codex'})
        self.assertTrue(self.sent)

    def test_running_claude_agent_never_continues(self):
        self.write(self.limit())
        self.snapshot['Status'] = 'running'
        self.tick()
        self.assertFalse(self.sent)


if __name__ == '__main__':
    unittest.main()
