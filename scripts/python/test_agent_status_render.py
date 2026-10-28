import copy
import json
import os
from pathlib import Path
import re
import subprocess
import sys
import unittest

from agent_status_render import render_section
from agy_status_render import width

NOW = 1700000000
ANSI = re.compile(r"\x1b\[[0-9;]*m")


def claude():
    return {"agent": "claude", "exit": 1, "report": [
        {"profile": "default", "plan": "max", "cache": {"from_cache": True, "fetched_at": NOW - 120},
         "windows": [{"key": "session", "label": "5h session", "utilization_percent": 52,
                      "resets_at": NOW + 3600 + 25 * 60},
                     {"key": "weekly_scoped", "label": "7d Fable", "utilization_percent": 90,
                      "resets_at": NOW + 4 * 86400 + 7 * 3600}]},
        {"profile": "work", "error": "Login required"}]}


def codex():
    window = {"windowDurationMins": 10080, "usedPercent": 61, "resetsAt": NOW + 2 * 86400 + 11 * 3600}
    return {"agent": "codex", "report": {"authFiles": [
        {"alias": "研究", "ok": True, "identity": {"planType": "pro", "planOwnerEmail": "masked@example.test"},
         "quota": {"windows": [window], "blocked": True, "reasons": ["spend limit"],
                   "resetCredits": {"availableCount": 1, "credits": [
                       {"title": "Full reset", "status": "available", "expiresAt": NOW + 4 * 86400}]},
                   "lunaReserve": {"label": "Reserve", "limitId": "reserve", "available": True,
                                   "model": "gpt-reserve", "windows": [dict(window, usedPercent=20)]}}},
        {"alias": "broken", "ok": False, "error": "Account unavailable"}],
        "averageUsage": {"usableCount": 1, "blockedCount": 0, "windows": [window],
                         "firstTimeToReset": NOW + 3600, "firstTimeToResetAlias": "研究"}}}


class AgentStatusTest(unittest.TestCase):
    def test_shared_layout_and_profile_failures_at_80_and_wide(self):
        for columns in (80, 120):
            for envelope in (claude(), codex()):
                original = copy.deepcopy(envelope)
                text = render_section(envelope, columns, True, now=NOW)
                self.assertEqual(envelope, original)
                plain = ANSI.sub("", text)
                self.assertTrue(all(width(line) <= columns for line in plain.splitlines()))
                self.assertIn("█", text)
                self.assertIn("weekly", text)
                self.assertIn("\x1b[31m", text)
                self.assertNotIn("credentials", plain)
                self.assertNotIn("auth.json", plain)
        text = render_section(claude(), now=NOW)
        self.assertIn("cache 2m", text)
        self.assertIn("resets in 1h 25m", text)
        self.assertIn("resets in 4d 7h", text)
        self.assertIn("Fable", text)
        self.assertIn("Login required", text)

    def test_codex_credits_reserve_and_summary_survive(self):
        text = render_section(codex(), now=NOW)
        for expected in ("Reset credits: 1 available", "expires in 4d", "Reserve", "gpt-reserve",
                         "Allowance blocked: spend limit", "Account unavailable", "All accounts: 1/1 usable",
                         "Average usage: weekly 61%", "First reset: 研究 in 1h"):
            self.assertIn(expected, text)

    def test_missing_usage_is_unknown_not_zero(self):
        envelope = claude()
        envelope['report'][0]['windows'][0]['utilization_percent'] = None
        text = render_section(envelope, now=NOW)
        self.assertIn('Claude 5h: usage unavailable', text)
        self.assertNotRegex(text, r'\b0%')
        self.assertIn('Fable', text)

    def test_stream_errors_do_not_hide_other_sections_and_no_color(self):
        script = str(Path(__file__).with_name('agent_status_render.py'))
        payload = json.dumps(claude()) + '\ninvalid\n' + json.dumps(codex()) + '\n'
        env = dict(os.environ, NO_COLOR='')
        result = subprocess.run([sys.executable, script, '--color', 'always'],
                                input=payload, text=True, capture_output=True, env=env)
        self.assertEqual(result.returncode, 0)
        self.assertNotIn('\x1b', result.stdout)
        self.assertFalse(result.stderr)
        self.assertLess(result.stdout.index('Claude Code'), result.stdout.index('Unknown provider'))
        self.assertLess(result.stdout.index('Unknown provider'), result.stdout.index('Codex'))
        self.assertIn('Reset credits', result.stdout)

    def test_json_is_the_same_accounts_as_data(self):
        script = str(Path(__file__).with_name('agent_status_render.py'))
        payload = json.dumps(claude()) + '\ninvalid\n' + json.dumps(codex()) + '\n'
        result = subprocess.run([sys.executable, script, '--format', 'json'],
                                input=payload, text=True, capture_output=True)
        self.assertEqual(result.returncode, 0, result.stderr)
        document = json.loads(result.stdout)
        self.assertIsInstance(document['generated_at'], float)
        claude_section, bad, codex_section = document['providers']
        self.assertEqual((claude_section['agent'], claude_section['title'], claude_section['error']),
                         ('claude', 'Claude Code', None))
        self.assertEqual(bad['error'], 'Unknown provider')
        default, work = claude_section['accounts']
        self.assertEqual([(w['family'], w['window'], w['used_percent'], w['resets_at'])
                          for w in default['windows']],
                         [('Claude', '5h', 52.0, NOW + 3600 + 25 * 60), ('Fable', 'weekly', 90.0, NOW + 4 * 86400 + 7 * 3600)])
        self.assertEqual((default['tier'], work['error']), ('max', 'Login required'))
        self.assertTrue(default['source'].startswith('cache '))
        notes = {(n['severity'], n['text'].split(':')[0]) for n in codex_section['accounts'][0]['notes']}
        self.assertLessEqual({('alarm', 'Allowance blocked'), ('info', 'Reset credits')}, notes)
        self.assertNotIn('credentials', result.stdout)
        self.assertNotIn('_display', result.stdout)


if __name__ == '__main__':
    unittest.main()
