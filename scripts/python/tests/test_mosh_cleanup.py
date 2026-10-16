import sys
from pathlib import Path
import unittest
from unittest.mock import patch
sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
import mosh_cleanup as m


class MoshCleanupTests(unittest.TestCase):
    def p(self, pid, parent, name='zsh', start='start'):
        return m.Process(pid, parent, 501, 'ttys123', start, name, '01:00')

    def test_ps_padding_and_exact_names(self):
        with patch.object(m, 'run', return_value=' 22 1 501 ?? Fri Oct 16 18:30:00 2026 01:00 mosh-server     \n'):
            p = m.snapshot()[22]
        self.assertEqual(p.name, 'mosh-server')
        self.assertEqual(p.age, '01:00')

    def test_traffic_exact_row_and_missing(self):
        with patch.object(m, 'run', return_value=',bytes_in,bytes_out,\nmosh-server.22,12,4,\nnot-mosh-server.23,0,0,\nmosh-server.24,,0,\n'):
            self.assertEqual(m.traffic(), {22: 12})

    def test_fail_closed_decisions_and_force(self):
        for delta, idle, jobs, prefix in [(None, 30, [], 'skip'), (-1, 30, [], 'skip'), (1, 30, [], 'keep'), (0, None, [], 'skip'), (0, 23, [], 'keep'), (0, 30, [1], 'skip'), (0, 30, [], 'kill')]:
            self.assertTrue(m.verdict(delta, idle, jobs, 24, False).startswith(prefix))
        self.assertTrue(m.verdict(0, 30, [1], 24, True).startswith('kill'))

    def test_shell_helpers_ignored_but_nested_jobs_protect(self):
        p = {10: self.p(10, 1, 'mosh-server'), 11: self.p(11, 10), 12: self.p(12, 11, '-zsh'.lstrip('-')), 13: self.p(13, 12, 'less')}
        with patch.object(m.os, 'stat') as stat:
            stat.return_value.st_atime = 0
            idle, jobs, desc, roots = m.details(p[10], p, 100000)
        self.assertEqual([q.pid for q in jobs], [13])
        self.assertEqual(desc, {11, 12, 13})
        self.assertEqual(roots, {11})
        self.assertGreater(idle, 24)

    def test_reused_pid_never_signaled(self):
        p = self.p(10, 1, 'mosh-server')
        with patch.object(m.os, 'getuid', return_value=501), patch.object(m.os, 'kill') as kill:
            self.assertFalse(m.send(p, 15, {10: self.p(10, 1, 'mosh-server', 'different')}))
            kill.assert_not_called()

    def test_termination_escalates_and_cleans_orphan_by_identity(self):
        server = self.p(10, 1, 'mosh-server')
        shell = self.p(11, 10)
        job = self.p(12, 11, 'sleep')
        initial = {10: server, 11: shell, 12: job}
        # The pty shell exits after TERM; its job is reparented but retains
        # identity. The server ignores TERM, exercising server KILL too.
        orphan = self.p(12, 1, 'sleep')
        after_term = {10: server, 12: orphan}
        with patch.object(m, 'snapshot', side_effect=[initial, after_term, after_term, {12: orphan}, {}]), patch.object(m, 'send', return_value=True) as send, patch.object(m.time, 'sleep'):
            m.terminate(server, initial, {11})
        calls = [(c.args[0].pid, c.args[1]) for c in send.call_args_list]
        self.assertEqual(calls, [(10, m.signal.SIGTERM), (10, m.signal.SIGKILL), (12, m.signal.SIGTERM), (12, m.signal.SIGKILL)])
