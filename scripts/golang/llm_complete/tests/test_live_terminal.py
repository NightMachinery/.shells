"""Test the live harness with fabricated snapshots, without live applications."""
import json
from pathlib import Path
import runpy
import unittest
from unittest.mock import patch

HARNESS = runpy.run_path(str(Path(__file__).with_name('live-terminal.py')))


class LiveSnapshotTests(unittest.TestCase):
    def test_wrapped_foreground_snapshot(self):
        for agent, argv in [
            ('claude', ['decset-rewrite', '-map', '1003=1002', '--', 'claude-work']),
            ('codex', ['decset-rewrite', '-trace', 'claude', '--', 'node', '/bin/codex']),
        ]:
            with self.subTest(agent=agent):
                target = HARNESS['Target']('kitty', 'unix:/tmp/kitty-12.sock', '7', agent)
                target.rc = lambda *args: json.dumps([{'tabs': [{'windows': [{
                    'id': 7, 'cwd': '/fabricated', 'columns': 80,
                    'foreground_processes': [
                        {'pid': 10, 'cmdline': ['echo', agent]},
                        {'pid': 42, 'cmdline': argv},
                    ],
                }]}]}])
                with patch('subprocess.check_output', return_value=b'fabricated\r\n\x1b[1;2H'):
                    snapshot = target.snapshot()
                self.assertEqual(snapshot['screen']['agent'], agent)
                self.assertEqual(snapshot['screen']['process_id'], 42)
                self.assertEqual(snapshot['screen']['columns'], 80)

    def test_unknown_wrapped_command_is_not_a_target(self):
        target = HARNESS['Target']('kitty', 'unix:/tmp/kitty-12.sock', '7', 'codex')
        target.rc = lambda *args: json.dumps([{'tabs': [{'windows': [{
            'id': 7, 'foreground_processes': [{'pid': 42, 'cmdline': [
                'decset-rewrite', '--', 'bash', '-c', 'codex',
            ]}],
        }]}]}])
        with patch('subprocess.check_output', return_value=b'\x1b[1;1H'):
            with self.assertRaises(StopIteration):
                target.snapshot()


if __name__ == '__main__':
    unittest.main()
