"""Inert argv fixtures; no kitty process or terminal is contacted."""
import os
from pathlib import Path
import runpy
import sys
import types
import unittest
from unittest.mock import patch

ROOT = Path(__file__).resolve().parents[3]
COMMAND = runpy.run_path(str(Path(__file__).with_name('agent_complete_command.py')))['agent_from_command']


class AgentCommandTests(unittest.TestCase):
    def test_launch_commands(self):
        cases = [
            ([], ''), ([''], ''),
            (['/bin/claude', 'codex'], 'claude'),
            (['/bin/claude-work'], 'claude'), (['claude.exe'], 'claude'),
            (['/bin/codex'], 'codex'),
            (['node', '/bin/claude'], 'claude'),
            (['node', '--no-warnings', '/bin/codex'], 'codex'),
            (['decset-rewrite', '-map', '1003=1002', '--', '/bin/claude'], 'claude'),
            (['/bin/decset-rewrite', '-trace', 'claude', '--', 'node', '/bin/codex'], 'codex'),
            (['decset-rewrite', '-map', '1003=1002', '/bin/codex'], 'codex'),
            (['decset-rewrite', '-trace', 'claude', '/bin/codex'], 'codex'),
            (['decset-rewrite', '-map=1003=1002', '/bin/claude'], 'claude'),
            (['decset-rewrite'], ''), (['decset-rewrite', '--'], ''),
            (['decset-rewrite', '-map'], ''),
            (['decset-rewrite', '-trace', 'claude'], ''),
            (['decset-rewrite', '-map', 'codex', '--'], ''),
            (['decset-rewrite', '--', 'echo', 'claude'], ''),
            (['decset-rewrite', 'echo', '--', 'claude'], ''),
            (['decset-rewrite', 'node', '-e', '0', '--', 'codex'], ''),
            (['decset-rewrite', '--', 'bash', '-c', 'codex'], ''),
            (['decset-rewrite', '--', 'node', '-e', 'claude'], ''),
            (['decset-rewrite', '--', 'node', '--require', 'codex'], ''),
            (['decset-rewrite', '--', 'node', '/bin/other', 'claude'], ''),
            (['decset-rewrite', '--', 'agy'], ''), (['agy'], ''),
            (['echo', 'codex'], ''), (['node', '--eval=0', 'claude'], ''),
        ]
        for argv, expected in cases:
            with self.subTest(argv=argv):
                self.assertEqual(COMMAND(argv), expected)

    def test_custom_kitten_without_file_global(self):
        handler = types.ModuleType('kittens.tui.handler')
        handler.result_handler = lambda **kwargs: lambda fn: fn
        modules = {name: types.ModuleType(name) for name in ('kittens', 'kittens.tui')}
        modules['kittens.tui.handler'] = handler
        namespace = {'__name__': 'kitten'}
        with patch.dict(sys.modules, modules), patch.dict(os.environ, {'NIGHTDIR': str(ROOT)}):
            source = ROOT / 'configFiles/kitty/kittens/agent_complete.py'
            exec(compile(source.read_text(), str(source), 'exec'), namespace)
        self.assertNotIn('__file__', namespace)
        window = types.SimpleNamespace(child=types.SimpleNamespace(foreground_processes=[
            {'pid': 11, 'cmdline': ['decset-rewrite', '--', 'echo', 'codex']},
            {'pid': 22, 'cmdline': ['decset-rewrite', '-map', '1003=1002', '--', '/bin/codex']},
        ]))
        self.assertEqual(namespace['agent_name'](window), ('codex', 22))
        window.child.foreground_processes = [{'pid': 33, 'cmdline': ['agy']}]
        self.assertEqual(namespace['agent_name'](window), ('', 0))


if __name__ == '__main__':
    unittest.main()
