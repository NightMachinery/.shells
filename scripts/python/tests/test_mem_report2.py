import sys
from pathlib import Path
import unittest
sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from mem_report2 import Proc, tree, snapshot, to_bytes


class MemoryTreeTests(unittest.TestCase):
    def p(self, pid, parent, exe, args=None, env=None):
        return Proc(pid, parent, '01:00', exe, args or exe, env or {})

    def test_opaque_ancestors_mosh_and_tmux(self):
        p = {1: self.p(1, 0, 'launchd'), 10: self.p(10, 1, 'mosh-server'),
             11: self.p(11, 10, 'zsh'), 12: self.p(12, 11, 'python3'),
             20: self.p(20, 1, 'mosh-server'), 21: self.p(21, 20, 'zsh'),
             30: self.p(30, 1, 'tmux'), 31: self.p(31, 30, 'zsh')}
        n = tree(p, {q: (100, 10) for q in p}, {31: 'example'}, {}, lambda _: '?')
        self.assertEqual(n['mosh-server (2 sessions)']['foot'], 500)
        self.assertEqual(n['tmux']['children']['session: example']['count'], 1)
        self.assertEqual(sum(v['count'] for v in n.values()), len(p))

    def test_provider_subtree_and_nested_session_no_double_count(self):
        p = {1: self.p(1, 0, 'launchd'), 2: self.p(2, 1, 'codex', env={'PASEO_AGENT_ID': 'a'}),
             3: self.p(3, 2, 'codex-code-mode-', env={'PASEO_AGENT_ID': 'a'}),
             4: self.p(4, 3, 'node'), 5: self.p(5, 4, 'Chrome'),
             6: self.p(6, 3, 'codex', env={'PASEO_AGENT_ID': 'b'})}
        n = tree(p, {q: (10, 5) for q in p}, {}, {'a': 'Alpha', 'b': 'Beta'}, lambda _: '?')
        sessions = n['Codex']['children']
        self.assertEqual(n['Codex']['count'], 5)
        self.assertEqual(sorted(v['count'] for v in sessions.values()), [1, 4])
        self.assertTrue(any(k.startswith('Alpha') for k in sessions))

    def test_named_script_owns_shell_helpers(self):
        p = {1: self.p(1, 0, 'launchd'), 2: self.p(2, 1, 'dash', 'dash /tmp/bicon_zsh.dash'), 3: self.p(3, 2, 'zsh')}
        n = tree(p, {2: (10, 0), 3: (20, 0)}, {}, {}, lambda _: '?')
        self.assertEqual(n['bicon_zsh.dash']['foot'], 30)

    def test_environment_never_rendered(self):
        p = snapshot(' 9 1 03:04 codex /opt/codex app-server PASEO_AGENT_ID=abc TOKEN=secret\n')
        self.assertEqual(p[9].env, {'PASEO_AGENT_ID': 'abc'})
        self.assertNotIn('secret', p[9].args)
        self.assertEqual(to_bytes('1.5G+'), 1.5 * 1024**3)

    def test_unmapped_paseo_fallback(self):
        p = {1: self.p(1, 0, 'node', 'Paseo Daemon'), 2: self.p(2, 1, 'codex')}
        n = tree(p, {2: (10, 0)}, {}, {}, lambda _: '~/project')
        self.assertIn('Paseo (unmapped) ~/project', next(iter(n['Codex']['children'])))
