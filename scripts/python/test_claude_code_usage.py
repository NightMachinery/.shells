import unittest

from claude_code_usage import extract_cloud_credit, extract_windows, render_cloud_credit
from libs.common_sub_status import Style

NOW = 1700000000
NULL_DOLLARS = {"limit_dollars": None, "used_dollars": None, "remaining_dollars": None, "locked_reason": None}


def payload(**credit):
    entry = {"utilization": 0.8, "resets_at": NOW + 86400, "limit_dollars": 250,
             "used_dollars": 2, "remaining_dollars": 248, "locked_reason": None}
    entry.update(credit)
    return {"five_hour": dict(NULL_DOLLARS, utilization=0.0, resets_at=NOW + 3600),
            "iguana_necktie": entry}


class CloudCreditTest(unittest.TestCase):
    def test_found_by_name_and_normalized(self):
        credit = extract_cloud_credit(payload(), now=NOW)
        self.assertEqual((credit["key"], credit["remaining_dollars"], credit["limit_dollars"]),
                         ("iguana_necktie", 248.0, 250.0))
        self.assertEqual(credit["expires_at"], NOW + 86400)
        self.assertTrue(credit["available"])

    def test_found_by_shape_when_renamed(self):
        data = payload()
        data["new_name"] = data.pop("iguana_necktie")
        self.assertEqual(extract_cloud_credit(data, now=NOW)["key"], "new_name")

    def test_absent_when_dollars_are_null(self):
        self.assertIsNone(extract_cloud_credit(payload(**NULL_DOLLARS), now=NOW))
        self.assertIsNone(extract_cloud_credit({"five_hour": NULL_DOLLARS}, now=NOW))

    def test_spent_or_expired_is_hidden_unless_asked(self):
        style = Style(False)
        for credit in (extract_cloud_credit(payload(remaining_dollars=0, used_dollars=250), now=NOW),
                       extract_cloud_credit(payload(resets_at=NOW - 60), now=NOW)):
            self.assertFalse(credit["available"])
            self.assertIsNone(render_cloud_credit(style, credit))
            self.assertIn("Cloud credit: $", render_cloud_credit(style, credit, always=True))
        self.assertIsNone(render_cloud_credit(style, None, always=True))

    def test_credit_is_not_a_usage_window(self):
        keys = [window.key for window in extract_windows(payload())]
        self.assertEqual(keys, ["five_hour"])


if __name__ == "__main__":
    unittest.main()
