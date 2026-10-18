#!/usr/bin/env python3
"""Model eligibility, applicable quota windows and catalog refresh regressions."""

import contextlib
import io
import json
from pathlib import Path
import sys
import tempfile
import time
import unittest

import agy_model_catalog as catalog


ARGON = {"id": "gemini-4-argon-high", "name": "Gemini 4 Argon (High)"}
FLASH = {"id": "gemini-3.8-flash-low", "name": "Gemini 3.8 Flash (Low)"}
OPUS = {"id": "claude-opus-5-5-low", "name": "Claude Opus 5.5 (Low)"}


def bucket(name, remaining):
    return {"bucket_id": name, "remaining_percent": remaining}


class CatalogTests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory()
        self.root = Path(self.temporary.name)

    def tearDown(self):
        self.temporary.cleanup()

    def cache(self, account, models, age=0):
        filename = self.root / account / "models-cache.json"
        catalog.write_cache(filename, {"version": 1, "account": account,
                            "captured_at": time.time() - age, "models": models})
        return filename

    def test_parse_real_tsv_and_display_name_normalization(self):
        models = catalog.parse_models("\x1b[32mAvailable models:\x1b[0m\n"
                                      "gemini-4-argon-high\tGemini 4 Argon (High)\n"
                                      "gemini-4-argon-low\tGemini 4 Argon (Low)\n")
        self.assertEqual(catalog.resolve_model(models, "Gemini 4 Argon High"), ARGON)
        self.assertEqual(catalog.resolve_model(models, "GEMINI-4-ARGON-HIGH"), ARGON)
        for unavailable in ["gemini-4-argon", "gemini-4-argon-*", "Gemini 4 Argon (Medium)"]:
            with self.assertRaises(ValueError):
                catalog.resolve_model(models, unavailable)

    def test_ambiguous_display_name_is_refused(self):
        models = [ARGON, dict(ARGON, id="gemini-other-high")]
        with self.assertRaises(ValueError):
            catalog.resolve_model(models, ARGON["name"])

    def test_only_matching_specific_and_general_limits_apply(self):
        rows = [bucket("gemini-weekly", 90), bucket("gemini-5h", 80),
                bucket("gemini-4-argon-weekly", 55), bucket("gemini-4-argon-5h", 40),
                bucket("gemini-3-8-flash-5h", 0), bucket("3p-weekly", 0),
                bucket("gemini-4-argon-low-5h", 0)]
        remaining, matched = catalog.quota_for_model(rows, ARGON)
        self.assertEqual(remaining, 40)
        self.assertEqual(set(matched), {"gemini-weekly", "gemini-5h",
                                       "gemini-4-argon-weekly", "gemini-4-argon-5h"})
        self.assertEqual(catalog.quota_for_model(rows, FLASH)[0], 0)

    def test_nested_specific_groups_and_family_constraints(self):
        rows = [bucket("gemini-weekly", 65), bucket("gemini-4-weekly", 2),
                bucket("gemini-4-argon-weekly", 70)]
        self.assertEqual(catalog.quota_for_model(rows, ARGON)[0], 2)
        self.assertEqual(catalog.quota_for_model([bucket("3p-weekly", 70),
                         bucket("claude-opus-5-5-5h", 30), bucket("claude-sonnet-5-5-5h", 0)], OPUS)[0], 30)

    def test_unknown_applicable_quota_fails_closed(self):
        for value in [None, float("nan"), -1, 101, True, "50"]:
            with self.assertRaises(ValueError):
                catalog.quota_for_model([bucket("gemini-weekly", 80),
                                        bucket("gemini-4-argon-5h", value)], ARGON)

    def test_ineligible_high_quota_account_cannot_win(self):
        catalogs = {"main": self.cache("main", [ARGON, OPUS]),
                    "other": self.cache("other", [FLASH])}
        statuses = [{"account": "main", "signed_in": True,
                     "buckets": [bucket("gemini-weekly", 20), bucket("3p-weekly", 25)]},
                    {"account": "other", "signed_in": True,
                     "buckets": [bucket("gemini-weekly", 99), bucket("3p-weekly", 99)]}]
        with contextlib.redirect_stderr(io.StringIO()):
            for model in [ARGON["id"], OPUS["name"]]:
                selected = catalog.select_account(statuses, catalogs, model, 3600)
                self.assertEqual(selected["account"], "main")
            with self.assertRaises(ValueError):
                catalog.select_account(statuses, catalogs, "gemini-4-argon-medium", 3600)

    def test_expired_or_mismatched_cache_cannot_win(self):
        stale = self.cache("old", [ARGON], age=7200)
        good = self.cache("new", [ARGON])
        statuses = [{"account": account, "buckets": [bucket("gemini-weekly", remaining)]}
                    for account, remaining in [("old", 99), ("new", 10)]]
        with contextlib.redirect_stderr(io.StringIO()):
            self.assertEqual(catalog.select_account(statuses, {"old": stale, "new": good},
                                                   ARGON["id"], 3600)["account"], "new")
        with self.assertRaises(ValueError):
            catalog.read_cache(good, "other-account", 3600)

    def test_refresh_saves_only_parsed_models_and_atomic_private_cache(self):
        cache_file = self.root / "main/models-cache.json"
        command = [sys.executable, "-c", "import sys; print('gemini-4-argon-high\\tGemini 4 Argon (High)'); sys.stderr.write('not cached')"]
        result = catalog.refresh(cache_file, "main", command, 2)
        self.assertEqual(result["models"], [ARGON])
        self.assertNotIn("not cached", cache_file.read_text())
        self.assertEqual(cache_file.stat().st_mode & 0o777, 0o600)
        self.assertEqual(catalog.read_cache(cache_file, "main", 3600), result)

    def test_failed_or_empty_refresh_preserves_old_cache_without_using_it(self):
        cache_file = self.cache("main", [ARGON], age=7200)
        previous = cache_file.read_bytes()
        for command in [[sys.executable, "-c", "raise SystemExit(1)"],
                        [sys.executable, "-c", "print('not a model list')"]]:
            with self.assertRaises(ValueError):
                catalog.refresh(cache_file, "main", command, 2)
            self.assertEqual(cache_file.read_bytes(), previous)
        with self.assertRaises(ValueError):
            catalog.read_cache(cache_file, "main", 3600)

    def test_timeout_is_bounded_and_writes_no_cache(self):
        cache_file = self.root / "models-cache.json"
        with contextlib.redirect_stderr(io.StringIO()), self.assertRaises(ValueError):
            catalog.refresh(cache_file, "main", [sys.executable, "-c", "import time; time.sleep(30)"], 0.05)
        self.assertFalse(cache_file.exists())


if __name__ == "__main__":
    unittest.main()
