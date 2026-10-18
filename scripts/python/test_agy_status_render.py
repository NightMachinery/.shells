import copy
import fcntl
import json
import os
from pathlib import Path
import pty
import re
import select
import struct
import subprocess
import tempfile
import termios
import unittest

from agy_status_render import duration, render, width


NOW = 1700000000
ANSI = re.compile(r"\x1b\[[0-9;]*m")


def fixture():
    def bucket(identity, group, used, seconds):
        return {"bucket_id": identity, "group": group, "label": "Weekly Limit Remaining",
                "used_percent": used, "remaining_percent": 100 - used,
                "resets_at": NOW + seconds, "source": "cache"}
    return [
        {"account": "m", "label": "m", "signed_in": None, "tier": "Google AI Pro",
         "email": "masked@example.test", "error": None, "buckets": [
             bucket("gemini-weekly", "Gemini Models", 6, 4 * 86400 + 7 * 3600),
             bucket("3p-weekly", "Claude and GPT models", 75, 2 * 86400 + 11 * 3600),
             bucket("gemini-4-argon-weekly", "Gemini 4 Argon", 95, 12 * 60),
             bucket("gemini-5h", "Gemini Models", 6, 3600 + 25 * 60)]},
        {"account": "cave", "label": "研究 Cafe\u0301", "signed_in": True, "tier": "Pro",
         "email": "another@example.test", "error": None, "buckets": [
             bucket("3p-5h", "Claude and GPT models", 12.5, 13 * 60)]},
        {"account": "fm2", "label": "fm2", "signed_in": False, "tier": None,
         "error": None, "buckets": []},
        {"account": "broken", "label": "broken", "signed_in": None, "tier": None,
         "error": "Quota lookup failed", "buckets": []},
    ]


class RenderTest(unittest.TestCase):
    def test_two_units_and_past_unknown_resets(self):
        self.assertEqual(duration(4 * 86400 + 7 * 3600 + 59 * 60), "4d 7h")
        self.assertEqual(duration(3600 + 25 * 60), "1h 25m")
        self.assertEqual(duration(12 * 60), "12m")
        self.assertEqual(duration(5), "<1m")
        accounts = fixture()
        accounts[0]["buckets"][0]["resets_at"] = NOW - 3600
        accounts[0]["buckets"][1]["resets_at"] = None
        text = render(accounts, now=NOW)
        self.assertIn("reset 1h ago", text)
        self.assertIn("reset unknown", text)

    def test_80_and_wide_alignment_and_model_rows(self):
        for columns in (80, 120):
            with self.subTest(columns=columns):
                accounts = fixture()
                original = copy.deepcopy(accounts)
                text = render(accounts, columns=columns, now=NOW)
                self.assertEqual(accounts, original)
                self.assertTrue(all(width(line) <= columns for line in text.splitlines()))
                bars = [line for line in text.splitlines() if "░" in line]
                positions = [min(line.find(c) for c in "█░" if c in line) for line in bars]
                self.assertEqual(len({width(line[:index]) for line, index in zip(bars, positions)}), 1)
                self.assertIn("Gemini 4 Argon", text)
                self.assertIn("Claude/GPT", text)
                self.assertLess(text.index("5h"), text.index("weekly"))
                self.assertIn("resets in 4d 7h", text)
                self.assertIn("resets in 1h 25m", text)
                self.assertNotIn("Limit Remaining", text)
                self.assertIn("agy-acc-login fm2", text)
                self.assertEqual(text.count("Quota lookup failed"), 1)
                self.assertNotIn("\x1b", text)
        self.assertEqual(width("研究 Cafe\u0301"), 9)

    def test_cache_age_header_and_dim_absolute(self):
        with tempfile.TemporaryDirectory() as temporary:
            cache = Path(temporary) / "cache.json"
            accounts = fixture()
            cache.write_text(json.dumps({"buckets": {
                b["bucket_id"]: {"captured_at": NOW - 120}
                for b in accounts[0]["buckets"]}}))
            accounts[0]["_display_cache_file"] = str(cache)
            text = render(accounts, columns=120, color=True, now=NOW)
            self.assertIn("\x1b[2mcache 2m\x1b[0m", text.splitlines()[0])
            self.assertIn("\x1b[2mmasked@example.test", text.splitlines()[0])
            self.assertIn("\x1b[32m", text)  # calm <70
            self.assertIn("\x1b[33m", text)  # warning 70..89
            self.assertIn("\x1b[31m", text)  # alarm >=90, and errors
            self.assertTrue(all(width(ANSI.sub("", line)) <= 120 for line in text.splitlines()))
            self.assertRegex(text, r"\x1b\[2m\w{3} \d{2} \w{3}")
            narrow = render(accounts, columns=80, color=True, now=NOW)
            self.assertNotRegex(narrow, r"\x1b\[2m\w{3} \d{2} \w{3}")

    def test_header_columns_align_across_accounts(self):
        accounts = fixture()[:3]
        for account, label, tier, source in zip(accounts, ("m", "cave", "研究"),
                                               ("Pro", "Google AI Pro", "Ultra"),
                                               ("cache", "live", "cache")):
            account.update(label=label, signed_in=None, tier=tier, email="masked@example.test")
            for bucket in account["buckets"]:
                bucket["source"] = source
        for columns in (80, 120):
            text = render(accounts, columns=columns, color=True, now=NOW)
            headers = [ANSI.sub("", block.splitlines()[0]) for block in text.split("\n\n")]
            for tokens in (("auth stored",) * 3,
                           ("Pro", "Google AI Pro", "Ultra"),
                           ("cache", "live", ""),
                           ("masked",) * 3):
                positions = [width(line[:line.index(token)]) for line, token in zip(headers, tokens) if token]
                self.assertEqual(len(set(positions)), 1, (columns, tokens, headers))
            self.assertTrue(all(width(line) <= columns for line in headers))

    def test_color_threshold_boundaries(self):
        for used, code in ((69.9, 32), (70, 33), (89.9, 33), (90, 31)):
            accounts = fixture()[:1]
            accounts[0]["buckets"] = accounts[0]["buckets"][:1]
            accounts[0]["buckets"][0]["used_percent"] = used
            text = render(accounts, color=True, now=NOW)
            self.assertIn(f"\x1b[{code}m", text)

    def test_live_header_and_long_header_does_not_wrap(self):
        accounts = fixture()
        accounts[0].update(label="A very long " * 20, tier="Plan " * 20)
        for bucket in accounts[0]["buckets"]:
            bucket["source"] = "live"
        text = render(accounts, columns=80, now=NOW)
        self.assertIn("live", text.splitlines()[0])
        self.assertTrue(all(width(line) <= 80 for line in text.splitlines()))

    def test_pipe_and_no_color_even_with_always(self):
        command = ["python3", str(Path(__file__).with_name("agy_status_render.py")), "--color", "always"]
        payload = json.dumps(fixture()).encode()
        result = subprocess.run(command, input=payload, capture_output=True)
        self.assertEqual(result.returncode, 0)
        self.assertNotIn(b"\x1b", result.stdout)
        # A real pseudo-terminal proves both terminal color and NO_COLOR gates.
        for no_color in (False, True):
            master, slave = pty.openpty()
            try:
                fcntl.ioctl(slave, termios.TIOCSWINSZ, struct.pack("HHHH", 30, 80, 0, 0))
                environment = dict(os.environ)
                environment.pop("NO_COLOR", None)
                if no_color:
                    environment["NO_COLOR"] = ""
                process = subprocess.Popen(command, stdin=subprocess.PIPE, stdout=slave, env=environment)
                process.communicate(payload, timeout=10)
                captured = b""
                # macOS can discard unread PTY output when its last slave closes.
                while select.select([master], [], [], 0.1)[0]:
                    try:
                        block = os.read(master, 65536)
                    except OSError:
                        break
                    if not block:
                        break
                    captured += block
                self.assertEqual(process.returncode, 0)
                self.assertEqual(b"\x1b" in captured, not no_color)
            finally:
                os.close(master)
                if slave is not None:
                    os.close(slave)


if __name__ == "__main__":
    unittest.main()
