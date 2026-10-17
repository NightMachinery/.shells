"""Bootstrap regression tests, with all network, compiler and GUI calls stubbed."""
import importlib.util
import json
import os
from pathlib import Path
import plistlib
import subprocess
import tempfile
import unittest
from unittest.mock import patch

spec = importlib.util.spec_from_file_location("notification_builder", Path(__file__).with_name("build.py"))
builder = importlib.util.module_from_spec(spec)
spec.loader.exec_module(builder)


class BootstrapTest(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory(prefix="notification-build-test-", dir="/private/tmp")
        self.addCleanup(self.tmp.cleanup)
        self.home = Path(self.tmp.name)
        self.env = patch.dict(os.environ, HOME=str(self.home))
        self.env.start()
        self.addCleanup(self.env.stop)

    def fake_app(self, dest, fingerprint="old"):
        (dest / "Contents/Resources").mkdir(parents=True)
        builder.binary(dest).parent.mkdir()
        builder.binary(dest).write_text("working previous helper")
        (dest / "Contents/Resources/night-build.json").write_text(json.dumps({"fingerprint": fingerprint}))
        (dest / "Contents/Info.plist").write_bytes(plistlib.dumps({"CFBundleIdentifier": builder.PROFILES["mail"][1]}))

    def test_up_to_date_does_not_download_compile_register_or_prompt(self):
        dest = builder.app_path("mail")
        self.fake_app(dest, builder.fingerprint("mail"))
        with patch.object(builder, "run") as run, patch.object(builder, "source") as source:
            builder.install("mail")
        source.assert_not_called()
        self.assertEqual(run.call_count, 1)  # Signature verification only.

    def test_failed_rebuild_preserves_existing_app(self):
        dest = builder.app_path("mail")
        self.fake_app(dest)
        with patch.object(builder, "source"), patch.object(builder, "build", side_effect=ValueError("compile failed")):
            with self.assertRaisesRegex(ValueError, "compile failed"):
                builder.install("mail")
        self.assertEqual(builder.binary(dest).read_text(), "working previous helper")

    def test_failed_registration_rolls_back_previous_app(self):
        dest = builder.app_path("mail")
        self.fake_app(dest)

        def build(profile, archive, work):
            candidate = work / "candidate.app"
            self.fake_app(candidate, builder.fingerprint("mail"))
            builder.binary(candidate).write_text("new helper")
            return candidate

        def run(args, **kwargs):
            if str(args[0]) == builder.LSREGISTER:
                raise subprocess.CalledProcessError(1, ["lsregister"])

        with patch.object(builder, "run", side_effect=run), patch.object(builder, "source"), patch.object(builder, "build", side_effect=build):
            with self.assertRaises(subprocess.CalledProcessError):
                builder.install("mail")
        self.assertEqual(builder.binary(dest).read_text(), "working previous helper")

    def test_unrecognized_app_is_not_overwritten(self):
        dest = builder.app_path("mail")
        dest.mkdir(parents=True)
        (dest / "personal-file").write_text("keep")
        with self.assertRaisesRegex(ValueError, "unrecognized"):
            builder.install("mail")
        self.assertEqual((dest / "personal-file").read_text(), "keep")

    def test_source_checksum_is_enforced(self):
        def download(args, **kwargs):
            Path(args[-1]).write_bytes(b"unexpected source")

        with patch.object(builder, "run", side_effect=download):
            with self.assertRaisesRegex(ValueError, "checksum mismatch"):
                builder.source(self.home)
        self.assertFalse((self.home / "terminal-notifier-2.0.0.tar.gz").exists())

    def test_sender_patch_is_required(self):
        with self.assertRaisesRegex(ValueError, "patch no longer applies"):
            builder.patch_delegate("unexpected upstream source")


if __name__ == "__main__":
    unittest.main()
