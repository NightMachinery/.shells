"""Exercise the actual phone-side alarm program with inert Android commands."""

import json
import os
from pathlib import Path
import re
import shutil
import subprocess
import sys
import tempfile
import unittest


ROOT = Path(__file__).resolve().parents[2]
SOURCE = ROOT / "zshlang/auto-load/others/termux-remote.zsh"
DATE = shutil.which("gdate") or shutil.which("date")
SCRIPT = re.search(
    r"typeset -g h_termux_alarm_script='\n(.*?)\n'\n", SOURCE.read_text(), re.S
).group(1)


class AlarmDateTest(unittest.TestCase):
    def epoch(self, value, tz):
        return subprocess.check_output(
            [DATE, "--date=" + value, "+%s"],
            env={**os.environ, "TZ": tz}, text=True,
        ).strip()

    def run_alarm(self, now, target, *, phone_tz="UTC", override="", fail_date=False):
        with tempfile.TemporaryDirectory() as tmp:
            directory = Path(tmp)
            log = directory / "intent.json"
            commands = {
                "date": (
                    "import os, sys\n"
                    "if os.environ.get('FAIL_DATE'): sys.exit(1)\n"
                    "if sys.argv[1:] == ['+%s']:\n"
                    "    print(os.environ['TEST_NOW'])\n"
                    "else:\n"
                    f"    os.execv({DATE!r}, [{DATE!r}] + sys.argv[1:])\n"
                ),
                "getprop": "import os\nprint(os.environ['TEST_PHONE_TZ'])\n",
                "am": (
                    "import json, os, sys\n"
                    "from pathlib import Path\n"
                    "Path(os.environ['TEST_LOG']).write_text(json.dumps(sys.argv[1:]))\n"
                ),
            }
            for name, body in commands.items():
                executable = directory / name
                executable.write_text(f"#!{sys.executable}\n{body}")
                executable.chmod(0o755)
            tz = override or phone_tz
            env = {
                **os.environ,
                "PATH": f"{tmp}:/usr/bin:/bin",
                "TEST_NOW": self.epoch(now, tz),
                "TEST_PHONE_TZ": phone_tz,
                "TEST_LOG": str(log),
                "alarm_epoch": self.epoch(target, tz),
                "alarm_tz": override,
                "alarm_msg": "literal $(printf SENTINEL) `printf SENTINEL`",
                "alarm_skip_ui": "true",
                "alarm_vibrate": "true",
                "alarm_component": "",
                "alarm_expect_hm": "",
            }
            if fail_date:
                env["FAIL_DATE"] = "1"
            result = subprocess.run(
                ["/bin/sh", "-c", SCRIPT], env=env, text=True, capture_output=True
            )
            intent = json.loads(log.read_text()) if log.exists() else None
            return result, intent

    def test_reject_wrong_dates_before_launch(self):
        for now, target in [
            ("2026-11-04 14:00", "2026-11-04 13:00"),
            ("2026-11-04 14:00", "2026-11-18 13:00"),
            ("2026-11-04 10:00", "2026-11-11 13:00"),
            ("2026-11-04 13:00:01", "2026-11-04 13:00:59"),
            ("2026-11-04 10:00", "2026-11-03 13:00"),
        ]:
            with self.subTest(now=now, target=target):
                result, intent = self.run_alarm(now, target)
                self.assertEqual(result.returncode, 2, result.stderr)
                self.assertIn("No alarm sent", result.stderr)
                self.assertIsNone(intent)

    def test_allow_next_occurrence_and_drop_seconds(self):
        for now, target in [
            ("2026-11-04 10:00", "2026-11-04 13:00:59"),
            ("2026-11-04 14:00", "2026-11-05 13:00"),
            ("2026-11-04 13:00:01", "2026-11-05 13:00"),
            ("2026-12-31 23:55", "2027-01-01 00:05"),
            ("2028-02-28 14:00", "2028-02-29 13:00"),
        ]:
            with self.subTest(now=now, target=target):
                result, intent = self.run_alarm(now, target)
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertIn("android.intent.action.SET_ALARM", intent)
                self.assertIn("literal $(printf SENTINEL) `printf SENTINEL`", intent)
                self.assertNotIn("android.intent.extra.alarm.DAYS", intent)
                self.assertNotIn("WEEKLY", result.stderr)
                self.assertEqual(result.stdout, "")

    def test_weekly_fallback_and_calendar_weekday_numbering(self):
        result, intent = self.run_alarm("2026-11-01 10:00", "2026-11-02 13:00")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(intent[intent.index("android.intent.extra.alarm.DAYS") + 1], "2")
        self.assertIn("WEEKLY REPEATING", result.stderr)
        # Start on a Sunday after 13:00, so all seven next weekdays are tested.
        for day, android_weekday in [(2, 2), (3, 3), (4, 4), (5, 5), (6, 6), (7, 7), (8, 1)]:
            with self.subTest(day=day):
                result, intent = self.run_alarm(
                    "2026-11-01 14:00", f"2026-11-{day:02d} 13:00"
                )
                self.assertEqual(result.returncode, 0, result.stderr)
                if day == 2:
                    self.assertNotIn("android.intent.extra.alarm.DAYS", intent)
                else:
                    index = intent.index("android.intent.extra.alarm.DAYS")
                    self.assertEqual(intent[index - 1], "--eial")
                    self.assertEqual(intent[index + 1], str(android_weekday))
                    self.assertIn("WEEKLY REPEATING", result.stderr)
                    self.assertIn("Disable or delete", result.stderr)

    def test_phone_timezone_and_explicit_override(self):
        for phone_tz, override in [
            ("Asia/Tokyo", ""),
            ("America/New_York", ""),
            ("UTC", "Asia/Tokyo"),
        ]:
            with self.subTest(phone_tz=phone_tz, override=override):
                result, intent = self.run_alarm(
                    "2026-11-04 10:00", "2026-11-05 13:00",
                    phone_tz=phone_tz, override=override,
                )
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertIn("WEEKLY REPEATING", result.stderr)
                self.assertEqual(intent[intent.index("android.intent.extra.alarm.DAYS") + 1], "5")
                self.assertIn(override or phone_tz, result.stderr)
                result, intent = self.run_alarm(
                    "2026-11-04 14:00", "2026-11-05 13:00",
                    phone_tz=phone_tz, override=override,
                )
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertEqual(intent[intent.index("android.intent.extra.alarm.HOUR") + 1], "13")

    def test_calendar_day_across_dst(self):
        for now, target in [
            ("2026-03-28 14:00", "2026-03-29 13:00"),
            ("2026-10-24 14:00", "2026-10-25 13:00"),
        ]:
            result, intent = self.run_alarm(now, target, phone_tz="Europe/Berlin")
            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertIsNotNone(intent)

    def test_date_failure_does_not_send_intent(self):
        result, intent = self.run_alarm(
            "2026-11-04 10:00", "2026-11-04 13:00", fail_date=True
        )
        self.assertEqual(result.returncode, 2)
        self.assertIsNone(intent)


if __name__ == "__main__":
    unittest.main()
