#!/usr/bin/env python3
"""Exercise video discovery, selection, and recent rows with synthetic media."""

import os
from pathlib import Path
import re
import shutil
import subprocess
import tempfile
import unittest


ROOT = Path(__file__).resolve().parents[2]
SGR = re.compile(r"\x1b\[[0-9;]*m")


class VideoPickerTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix="video-picker-")
        self.addCleanup(self.temp.cleanup)
        self.directory = Path(self.temp.name)
        self.media = self.directory / "media library"
        self.media.mkdir()
        self.video = self.media / "Example Video 'quote' $dollar.mkv"
        self.video.touch()
        (self.media / "Example Notes.txt").touch()

    def shell(self, body, *args, cwd=None):
        preamble = r'''
unset night_basic_plugin_loaded_p
source "${PICKER_TEST_ROOT}/zshlang/basic/basic.plugin.zsh" || exit $?
function ecerr { print -ru2 -- "$@"; }
function aliasfnq { :; }
source "${PICKER_TEST_ROOT}/zshlang/auto-load/others/fuzzy/shortcuts.zsh" || exit $?
video_formats=(mkv mp4)
function fz-createquery { print -r -- "$*"; }
function ntag-lv { ntag_lv_nopriority=y ntag-lv-nocolor "$@"; }
function fz-ntag { command fzf --ansi --filter Example "$@"; }
function rgeval { printf '%s\0' "$@"; }
'''
        return subprocess.run(
            ["zsh", "-f", "-c", preamble + body, "fixture", *map(str, args)],
            cwd=cwd or ROOT, env=dict(os.environ, PICKER_TEST_ROOT=str(ROOT), FZF_DEFAULT_OPTS=""),
            text=True, capture_output=True, timeout=15,
        )

    def assert_launch(self, result):
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout.split("\0")[:-1], ["awaysh", "mpv", str(self.video.resolve())])
        self.assertEqual(result.stderr, "")

    def test_selected_file_wins_over_stale_tag_destination(self):
        result = self.shell('ntag_add_dest=/stale/unused.mkv\nplay-tag "$1"', self.video)
        self.assert_launch(result)

    def test_relative_selection_is_resolved_before_launch(self):
        result = self.shell('play-tag "$1"', self.video.name, cwd=self.media)
        self.assert_launch(result)

    def test_empty_and_missing_files_never_launch(self):
        for filename in ("", str(self.media / "missing.mkv")):
            with self.subTest(filename=filename):
                result = self.shell('play-tag "$1"', filename)
                self.assertNotEqual(result.returncode, 0)
                self.assertEqual(result.stdout, "")

    def test_missing_roots_are_skipped(self):
        result = self.shell('openv_dirs=("$1" "$2")\nopenv', self.media, self.directory / "missing")
        self.assert_launch(result)

    def test_all_missing_roots_do_not_fall_back_to_cwd(self):
        result = self.shell('openv_dirs=("$1")\nopenv', self.directory / "missing", cwd=self.media)
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual(result.stdout, "")
        self.assertNotIn("[fd error]", result.stderr)

    def test_no_configured_roots_searches_cwd(self):
        result = self.shell('unset openv_dirs\nopenv', cwd=self.media)
        self.assert_launch(result)

    def test_no_video_candidates_do_not_emit_an_empty_row(self):
        self.video.unlink()
        result = self.shell('ntag_lv_nopriority=y ntag-lv-nocolor "$1"', self.media)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout, "")
        result = self.shell('openv_dirs=("$1")\nopenv', self.media)
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual(result.stdout, "")

    def test_symlink_and_duplicate_roots_are_normalized(self):
        alias = self.directory / "media alias"
        alias.symlink_to(self.media, target_is_directory=True)
        result = self.shell('''
function ntag-lv { printf '%s\n' "$@" >&2; ntag_lv_nopriority=y ntag-lv-nocolor "$@"; }
openv_dirs=("$1" "$2" "$1")
openv
''', self.media, alias)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stderr, str(self.media.resolve()) + "\n")
        self.assertEqual(result.stdout.split("\0")[:-1], ["awaysh", "mpv", str(self.video.resolve())])

    def test_overlapping_roots_emit_each_video_once(self):
        nested = self.media / "nested"
        nested.mkdir()
        nested_video = nested / "Second Example.mp4"
        nested_video.touch()
        result = self.shell('ntag_lv_nopriority=y ntag-lv-nocolor "$1" "$2" "$1"', self.media, nested)
        self.assertEqual(result.returncode, 0, result.stderr)
        rows = SGR.sub("", result.stdout).splitlines()
        self.assertCountEqual(rows, [str(self.video.resolve()), str(nested_video.resolve())])

    def test_cancel_and_empty_selection_never_launch(self):
        for picker in ('command cat >/dev/null; return 130', 'command cat >/dev/null'):
            with self.subTest(picker=picker):
                result = self.shell(f'function fz-ntag {{ {picker}; }}\nopenv_dirs=("$1")\nopenv', self.media)
                self.assertNotEqual(result.returncode, 0)
                self.assertEqual(result.stdout, "")

    def test_discovery_failure_never_launches(self):
        result = self.shell('''
function fd { print -r -- "$2"; return 23; }
function fz-ntag { command cat >/dev/null; print -r -- "$fixture_selection"; }
fixture_selection="$1"
openv_dirs=("$2")
openv
''', self.video, self.media)
        self.assertEqual(result.returncode, 23, result.stderr)
        self.assertEqual(result.stdout, "")

    def test_explicit_directory_form_uses_selection(self):
        # indir is stubbed only to avoid unrelated environment hooks on cd.
        result = self.shell('function indir { builtin cd -- "$1" && shift && "$@"; }\nzv "$1"', self.media)
        self.assert_launch(result)

    def test_recent_plain_row_and_colored_live_row_are_one_item(self):
        env = dict(os.environ, HOME=str(self.directory), FZF_DEFAULT_OPTS="",
                   FORCE_NONINTERACTIVE="y", fzf_mru_context="video-fixture")
        colored = f"\x1b[36m{self.video}\x1b[0m\n"
        for payload in (colored, colored, str(self.video) + "\n" + colored):
            result = subprocess.run(
                ["/bin/dash", str(ROOT / "zshlang/wrappers/fzf_mru.sh"), "--ansi", "--filter", "Example"],
                input=payload, text=True, capture_output=True, env=env, timeout=15,
            )
            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertEqual(result.stdout, str(self.video) + "\n")
        cache = list((self.directory / ".cache/fzf_mru").iterdir())
        self.assertEqual(len(cache), 1)
        self.assertEqual(cache[0].read_text(), str(self.video) + "\n")


if __name__ == "__main__":
    for dependency in ("zsh", "fd", "fzf", "grealpath"):
        if shutil.which(dependency) is None:
            raise SystemExit(f"missing test dependency: {dependency}")
    unittest.main()
