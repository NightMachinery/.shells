"""Safety-boundary tests: all process inspection and signals are mocked.

External CLIs are fakes that only log their arguments and print an inert
sentinel; no test reaches a real Paseo daemon.
"""
import argparse
from contextlib import ExitStack
import importlib.util
import io
import json
import os
from pathlib import Path
import shutil
import signal
import stat
import subprocess
import sys
import tempfile
import unittest
from unittest.mock import Mock, patch


MODULE = Path(__file__).resolve().parents[1] / "paseo_handoff.py"
SPEC = importlib.util.spec_from_file_location("paseo_handoff", MODULE)
h = importlib.util.module_from_spec(SPEC)
sys.modules[SPEC.name] = h
SPEC.loader.exec_module(h)
SOURCE_ID = "12345678-1234-1234-1234-123456789abc"
IMPORTED_ID = "abcdef12-1234-1234-1234-123456789abc"
HELPER = MODULE.with_name("paseo_set_model.mjs")


def fake_cli(filename, log, label, fail=None):
    """Write a fake CLI that logs its arguments and prints an inert sentinel.

    It exits 1 when its first argument matches the `case` pattern `fail`.
    """
    filename.write_text(
        "#!/bin/sh\n"
        f"printf '%s' '{label}' >> '{log}'\n"
        f"for arg in \"$@\"; do printf ' %s' \"$arg\" >> '{log}'; done\n"
        f"printf '\\n' >> '{log}'\n"
        "printf sentinel\n"
        f"case \"$1\" in {fail or '__never__'}) exit 1 ;; esac\n")
    filename.chmod(0o700)


class HandoffTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.directory = Path(self.temp.name).resolve()
        self.directory.chmod(0o700)
        self.warnings = []
        warned = patch.object(h, "warn", side_effect=self.warnings.append)
        warned.start()
        self.addCleanup(warned.stop)
        self.stdout = io.StringIO()
        quiet = patch.object(sys, "stdout", self.stdout)
        quiet.start()
        self.addCleanup(quiet.stop)
        self.log = self.directory / "cli.log"
        self.helper = self.directory / "model-helper"
        fake_cli(self.helper, self.log, "model")
        self.home = self.directory / "profile"
        self.home.mkdir()
        root = self.home / "projects" / "project"
        root.mkdir(parents=True)
        self.transcript = root / (SOURCE_ID + ".jsonl")
        self.transcript.write_text("{}\n")
        self.executable = self.directory / "paseo"
        self.executable.write_text("#!/bin/sh\nprintf sentinel\n")
        self.executable.chmod(0o700)
        self.planfile = self.directory / "plan.json"
        self.daemon_home = self.directory / "daemon"
        self.daemon_home.mkdir(mode=0o700)
        self.source = h.Process(100, 2, os.getuid(), "Fri Oct 2 10:00:00 2026", "/usr/bin/claude")
        self.caller = h.Process(200, 100, os.getuid(), "Fri Oct 2 10:01:00 2026", "/bin/zsh")
        self.args = argparse.Namespace(
            agent="claude", id=SOURCE_ID, transcript=str(self.transcript), cwd=str(self.directory),
            provider_home=str(self.home), source_pid=100, caller_pid=200,
            paseo=str(self.executable), agent_session=str(self.executable),
            paseo_home=str(self.directory / "daemon"), tmux_session="handoff-123",
            output=str(self.planfile), wait_exit=False,
        )
        self.meta = SOURCE_ID + "\tprivate name\t" + str(self.directory) + "\n"

    def make_plan(self, table=None):
        with patch.object(h, "process_table", return_value=table or {100: self.source, 200: self.caller}), \
                patch.object(h, "command", return_value=self.meta):
            h.prepare(self.args)
        return json.loads(self.planfile.read_text())

    def flow(self, *, rows=None, response=None, identities=None, wait_error=False, preflight_error=False,
             plan_table=None):
        plan = self.make_plan(plan_table)
        stack = ExitStack()
        self.addCleanup(stack.close)
        stack.enter_context(patch.object(h, "verify_native"))
        preflight = stack.enter_context(patch.object(h, "provider_preflight", return_value="claude"))
        if preflight_error:
            preflight.side_effect = h.HandoffError("provider failed")
        wait = stack.enter_context(patch.object(h, "wait_gone"))
        if wait_error:
            wait.side_effect = [None, h.HandoffError("timeout")]
        saved = [plan["source"], plan.get("owner", plan["source"])]
        table = stack.enter_context(patch.object(h, "process_table", return_value={p["pid"]: h.Process(**p) for p in saved}))
        if identities:
            table.side_effect = identities
        if rows is None:
            rows = [[(100, SOURCE_ID, str(self.transcript), "interactive")], []]
        stack.enter_context(patch.object(h, "live_rows", side_effect=rows))
        imported = stack.enter_context(patch.object(h, "json_command", return_value=response or {
            "agentId": IMPORTED_ID, "provider": "claude", "cwd": str(self.directory), "status": "running",
        }))
        killed = stack.enter_context(patch.object(h.os, "kill"))
        attached = stack.enter_context(patch.object(h, "attach"))
        # Never the real Node helper: it would talk to the real daemon.
        stack.enter_context(patch.object(h, "model_helper_argv", return_value=[str(self.helper)]))
        return imported, killed, attached, wait

    def test_prepare_checks_ancestry_metadata_and_private_output(self):
        self.make_plan()
        self.assertEqual(stat.S_IMODE(self.planfile.stat().st_mode), 0o600)
        self.assertNotIn("environment", self.planfile.read_text())
        with self.assertRaises(h.HandoffError):
            self.make_plan()  # exclusive file creation

    def test_unrelated_caller_refused(self):
        other = h.Process(200, 2, os.getuid(), self.caller.started, "/bin/zsh")
        with patch.object(h, "process_table", return_value={100: self.source, 200: other}):
            with self.assertRaisesRegex(h.HandoffError, "not a descendant"):
                h.prepare(self.args)
        self.assertFalse(self.planfile.exists())

    def test_ancestry_through_multiple_children(self):
        caller = h.Process(200, 150, os.getuid(), self.caller.started, "zsh")
        middle = h.Process(150, 100, os.getuid(), self.caller.started, "python3")
        self.assertTrue(h.ancestor({100: self.source, 150: middle, 200: caller}, caller, self.source))
        self.assertFalse(h.ancestor({200: h.Process(200, 200, os.getuid(), "s", "zsh")}, caller, self.source))

    def test_wrong_owner_or_executable_refused(self):
        for process in (h.Process(100, 2, os.getuid() + 1, "s", "claude"),
                        h.Process(100, 2, os.getuid(), "s", "/bin/sleep"),
                        h.Process(100, 2, os.getuid(), "s", "/bin/decset-rewrite")):
            with self.subTest(process=process), self.assertRaises(h.HandoffError):
                h.source_allowed(process, "claude")

    def proxy_table(self, *, node=False, agent="claude", command=None):
        owner = h.Process(100, 2, os.getuid(), self.source.started, "/bin/decset-rewrite")
        launcher = h.Process(120, 100, os.getuid(), self.source.started, "/bin/node")
        native = h.Process(150, 120 if node else 100, os.getuid(), self.source.started,
                           command or "/bin/" + agent)
        caller = h.Process(200, 150, os.getuid(), self.caller.started, "/bin/zsh")
        table = {p.pid: p for p in (owner, native, caller)}
        if node:
            table[launcher.pid] = launcher
        return table

    def use_codex(self):
        self.args.agent = "codex"
        root = self.home / "sessions"
        root.mkdir()
        self.transcript = root / ("rollout-2026-10-02-" + SOURCE_ID + ".jsonl")
        self.transcript.write_text("{}\n")
        self.args.transcript = str(self.transcript)

    def test_proxy_ancestry_selects_native_codex_over_node_launcher(self):
        self.use_codex()
        for node in (False, True):
            with self.subTest(node=node):
                if self.planfile.exists():
                    self.planfile.unlink()
                table = self.proxy_table(node=node, agent="codex")
                plan = self.make_plan(table)
                self.assertEqual(plan["source"]["pid"], 150)
                self.assertEqual(plan["owner"]["pid"], 100)
                self.assertEqual([p["pid"] for p in plan["source_ancestors"]], [120] if node else [])
                self.assertEqual(h.validate_plan(plan), (table[150], table[200]))

    def test_claude_exe_native_and_proxy_accepted(self):
        self.source = h.Process(100, 2, os.getuid(), self.source.started, "/bin/claude.exe")
        plan = self.make_plan()
        self.assertNotIn("owner", plan)
        self.assertEqual(h.validate_plan(plan), (self.source, self.caller))
        self.planfile.unlink()
        plan = self.make_plan(self.proxy_table(command="/bin/claude.exe"))
        self.assertEqual(plan["source"]["command"], "/bin/claude.exe")
        h.validate_plan(plan)

    def test_node_owner_prefers_native_child_and_keeps_owner_as_fallback(self):
        table = self.proxy_table(node=True)
        self.args.source_pid = 120
        plan = self.make_plan(table)
        self.assertEqual(plan["source"]["pid"], 150)
        self.assertEqual(plan["owner"]["pid"], 120)
        h.validate_plan(plan)
        self.planfile.unlink()
        table[150] = h.Process(150, 120, os.getuid(), self.source.started, "/bin/node")
        plan = self.make_plan(table)
        self.assertEqual(plan["source"]["pid"], 120)
        self.assertNotIn("owner", plan)

    def test_proxy_node_fallback_is_outermost_not_nearest_tool(self):
        table = self.proxy_table(node=True)
        table[150] = h.Process(150, 120, os.getuid(), self.source.started, "/bin/node")
        plan = self.make_plan(table)
        self.assertEqual(plan["source"]["pid"], 120)
        h.validate_plan(plan)

    def test_unknown_proxy_missing_native_wrong_uid_or_unrelated_caller_refused(self):
        changes = [
            (100, h.Process(100, 2, os.getuid(), self.source.started, "/bin/unknown-proxy")),
            (150, h.Process(150, 100, os.getuid(), self.source.started, "/bin/sleep")),
            (150, h.Process(150, 100, os.getuid() + 1, self.source.started, "/bin/claude")),
            (200, h.Process(200, 2, os.getuid(), self.caller.started, "/bin/zsh")),
        ]
        for pid, replacement in changes:
            with self.subTest(replacement=replacement):
                table = self.proxy_table()
                table[pid] = replacement
                with patch.object(h, "process_table", return_value=table), self.assertRaises(h.HandoffError):
                    h.prepare(self.args)
                self.assertFalse(self.planfile.exists())

    def test_saved_proxy_graph_or_identity_invalid_refused(self):
        plan = self.make_plan(self.proxy_table(node=True))
        invalid = [
            {"owner": plan["owner"] | {"uid": os.getuid() + 1}},
            {"owner": plan["owner"] | {"command": "/bin/unknown-proxy"}},
            {"source": plan["source"] | {"command": "/bin/decset-rewrite"}},
            {"source": plan["source"] | {"ppid": 987}},
            {"source_ancestors": []},
            {"source_ancestors": [plan["source"]]},
        ]
        for changed in invalid:
            with self.subTest(changed=changed), self.assertRaises(h.HandoffError):
                h.validate_plan(plan | changed)

    def test_proxy_or_native_pid_reuse_refuses_before_or_after_live_scan(self):
        table = self.proxy_table()
        for pid in (100, 150):
            replacement = h.Process(pid, table[pid].ppid, os.getuid(), "new start", table[pid].command)
            changed = table | {pid: replacement}
            for identities in ([changed], [table, changed]):
                with self.subTest(pid=pid, identities=identities):
                    if self.planfile.exists():
                        self.planfile.unlink()
                    imported, killed, attached, _ = self.flow(plan_table=table, identities=identities)
                    with self.assertRaisesRegex(h.HandoffError, "identity changed"):
                        h.run(str(self.planfile))
                    killed.assert_not_called()
                    imported.assert_not_called()
                    attached.assert_not_called()

    def test_proxy_row_verification_then_only_native_exact_pid_signaled(self):
        table = self.proxy_table(node=True, command="/bin/claude.exe")
        imported, killed, attached, wait = self.flow(plan_table=table)
        with patch.object(h.os, "killpg") as kill_group:
            h.run(str(self.planfile))
        killed.assert_called_once_with(150, signal.SIGTERM)
        kill_group.assert_not_called()
        self.assertEqual([call.args for call in wait.call_args_list], [(table[200], 30), (table[150], 30)])
        imported.assert_called_once()
        attached.assert_called_once()
        imported.reset_mock()
        killed.reset_mock()
        with patch.object(h, "process_table", return_value={}) as inspect:
            h.run(str(self.planfile))
        inspect.assert_not_called()
        imported.assert_not_called()
        killed.assert_not_called()

    def test_proxy_shared_owner_or_target_row_refuses_signal(self):
        table = self.proxy_table()
        row = (100, SOURCE_ID, str(self.transcript), "interactive")
        invalid = [[row, (100, IMPORTED_ID, "/other.jsonl", "interactive")],
                   [(150, SOURCE_ID, str(self.transcript), "interactive")],
                   [row, (987, SOURCE_ID, str(self.transcript), "interactive")]]
        for rows in invalid:
            with self.subTest(rows=rows):
                if self.planfile.exists():
                    self.planfile.unlink()
                imported, killed, attached, _ = self.flow(plan_table=table, rows=[rows])
                with self.assertRaises(h.HandoffError):
                    h.run(str(self.planfile))
                killed.assert_not_called()
                imported.assert_not_called()
                attached.assert_not_called()

    def test_wait_exit_proxy_waits_for_native_without_signal(self):
        self.args.wait_exit = True
        table = self.proxy_table()
        imported, killed, attached, wait = self.flow(plan_table=table, rows=[[]])
        with patch.object(h.os, "killpg") as kill_group:
            h.run(str(self.planfile))
        self.assertEqual([call.args for call in wait.call_args_list], [(table[200], 30), (table[150], 600)])
        killed.assert_not_called()
        kill_group.assert_not_called()
        imported.assert_called_once()
        attached.assert_called_once()

    def test_uuid_and_store_mismatch_refused(self):
        self.args.id = SOURCE_ID.upper()
        with self.assertRaisesRegex(h.HandoffError, "filename does not match"):
            self.make_plan()
        self.args.id = SOURCE_ID
        plan = self.make_plan()
        plan["id"] = IMPORTED_ID
        with self.assertRaises(h.HandoffError):
            h.verify_native(plan)

    def test_uppercase_session_keeps_its_case(self):
        # macOS `uuidgen` ids: Claude Code names the transcript as given.
        upper = SOURCE_ID.upper()
        self.transcript.unlink()
        self.transcript = self.transcript.with_name(upper + ".jsonl")
        self.transcript.write_text("{}\n")
        self.args.id, self.args.transcript = upper, str(self.transcript)
        self.meta = upper + "\tprivate name\t" + str(self.directory) + "\n"
        plan = self.make_plan()
        self.assertEqual(plan["id"], upper)
        h.validate_plan(plan)
        for bad in (SOURCE_ID[:-2] + "Bc", "{" + SOURCE_ID + "}", SOURCE_ID.replace("-", "")):
            with self.subTest(bad=bad), self.assertRaises(h.HandoffError):
                h.valid_uuid(bad)

    def test_metadata_uuid_or_cwd_mismatch_refused(self):
        plan = self.make_plan()
        for meta in (IMPORTED_ID + "\t\t" + str(self.directory), SOURCE_ID + "\t\t/"):
            with self.subTest(meta=meta), patch.object(h, "command", return_value=meta):
                with self.assertRaises(h.HandoffError):
                    h.verify_native(plan)

    def test_ps_parser_accepts_bsd_spacing_and_paths_with_spaces(self):
        sample = " 100 2 " + str(os.getuid()) + " Fri Oct  2 10:00:00 2026 /Applications/Agent App/claude\n"
        with patch.object(h, "command", return_value=sample) as command:
            process = h.process_table()[100]
        self.assertEqual(process.started, self.source.started)
        self.assertEqual(Path(process.command).name, "claude")
        self.assertEqual(command.call_args.kwargs["env"]["LC_ALL"], "C")
        self.assertNotIn("args", " ".join(command.call_args.args[0]))
        with patch.object(h, "command", return_value="100 unknown\n"), self.assertRaises(h.HandoffError):
            h.process_table()

    def test_preflight_failure_never_signals_or_imports(self):
        imported, killed, attached, _ = self.flow(preflight_error=True)
        with self.assertRaises(h.HandoffError):
            h.run(str(self.planfile))
        killed.assert_not_called()
        imported.assert_not_called()
        attached.assert_not_called()

    def test_reused_pid_never_signaled(self):
        replaced = h.Process(100, 2, os.getuid(), "Fri Oct 2 11:00:00 2026", "claude")
        for identities in ([{100: replaced}], [{100: self.source}, {100: replaced}]):
            with self.subTest(identities=identities):
                if self.planfile.exists():
                    self.planfile.unlink()
                imported, killed, attached, _ = self.flow(identities=identities)
                with self.assertRaisesRegex(h.HandoffError, "identity changed"):
                    h.run(str(self.planfile))
                killed.assert_not_called()
                imported.assert_not_called()
                attached.assert_not_called()

    def test_multiple_threads_uuid_mismatch_background_and_second_writer_refused(self):
        selected = (100, SOURCE_ID, str(self.transcript), "interactive")
        invalid = [
            [selected, (100, IMPORTED_ID, "/different.jsonl", "-")],
            [(100, IMPORTED_ID, str(self.transcript), "interactive")],
            [(100, SOURCE_ID, str(self.transcript), "background")],
            [selected, (101, SOURCE_ID, str(self.transcript), "interactive")],
            [],
        ]
        plan = self.make_plan()
        for rows in invalid:
            with self.subTest(rows=rows), self.assertRaises(h.HandoffError):
                h.verify_live_source(plan, self.source, rows)

    def test_timeout_does_not_import(self):
        imported, killed, attached, wait = self.flow(wait_error=True)
        with self.assertRaises(h.HandoffError):
            h.run(str(self.planfile))
        killed.assert_called_once_with(100, signal.SIGTERM)
        imported.assert_not_called()
        attached.assert_not_called()
        self.assertEqual(wait.call_args.args, (self.source, 30))

    def test_exact_pid_sigterm_validated_import_and_saved_result(self):
        imported, killed, attached, wait = self.flow()
        h.run(str(self.planfile))
        killed.assert_called_once_with(100, signal.SIGTERM)
        self.assertEqual([call.args for call in wait.call_args_list], [(self.caller, 30), (self.source, 30)])
        self.assertEqual(imported.call_args.args[0], [str(self.executable), "import", SOURCE_ID,
            "--provider", "claude", "--cwd", str(self.directory), "--home", self.args.paseo_home, "--json"])
        attached.assert_called_once()
        self.assertEqual(attached.call_args.args[1], IMPORTED_ID)
        resultfile = self.directory / "status" / "result.json"
        self.assertEqual(stat.S_IMODE(resultfile.stat().st_mode), 0o600)
        self.assertEqual(json.loads(resultfile.read_text())["sourceId"], SOURCE_ID)
        imported.reset_mock()
        killed.reset_mock()
        with patch.object(h, "live_rows", return_value=[
                (301, SOURCE_ID, str(self.transcript), "interactive")]) as live:
            h.run(str(self.planfile))
        live.assert_not_called()  # observer attachment permits the destination writer
        imported.assert_not_called()
        killed.assert_not_called()

    def test_import_invalid_id_provider_or_cwd_never_attaches(self):
        for changed in ({"agentId": "invalid"}, {"provider": "codex"}, {"cwd": "/"}):
            with self.subTest(changed=changed):
                if self.planfile.exists():
                    self.planfile.unlink()
                importfile = self.directory / "status" / "import-request.json"
                if importfile.exists():
                    importfile.unlink()
                response = {"agentId": IMPORTED_ID, "provider": "claude", "cwd": str(self.directory)} | changed
                _, _, attached, _ = self.flow(response=response)
                with self.assertRaises(h.HandoffError):
                    h.run(str(self.planfile))
                attached.assert_not_called()
                self.assertFalse((self.directory / "status" / "result.json").exists())
                self.assertTrue(importfile.exists())

    def test_ambiguous_import_refuses_retry_without_kill_or_reimport(self):
        imported, killed, attached, _ = self.flow(response={"agentId": "invalid"})
        with self.assertRaises(h.HandoffError):
            h.run(str(self.planfile))
        imported.reset_mock()
        killed.reset_mock()
        with self.assertRaisesRegex(h.HandoffError, "earlier import"):
            h.run(str(self.planfile))
        imported.assert_not_called()
        killed.assert_not_called()
        attached.assert_not_called()

    def test_symlink_and_world_readable_plan_refused(self):
        self.make_plan()
        link = self.directory / "link.json"
        link.symlink_to(self.planfile)
        with self.assertRaises(h.HandoffError):
            h.run(str(link))
        self.planfile.chmod(0o644)
        with self.assertRaises(h.HandoffError):
            h.run(str(self.planfile))

    def test_post_exit_native_writer_blocks_import(self):
        rows = [[(100, SOURCE_ID, str(self.transcript), "interactive")],
                [(101, SOURCE_ID, str(self.transcript), "interactive")]]
        imported, _, attached, _ = self.flow(rows=rows)
        with patch.object(h.time, "monotonic", side_effect=[0, 5]), patch.object(h.time, "sleep") as sleep:
            with self.assertRaisesRegex(h.HandoffError, "live writer"):
                h.run(str(self.planfile))
        sleep.assert_not_called()
        imported.assert_not_called()
        attached.assert_not_called()

    def test_wait_exit_sends_no_signal_and_waits_longer(self):
        self.args.wait_exit = True
        imported, killed, attached, wait = self.flow(rows=[[]])
        h.run(str(self.planfile))
        killed.assert_not_called()
        self.assertEqual([call.args for call in wait.call_args_list], [(self.caller, 30), (self.source, 600)])
        imported.assert_called_once()
        attached.assert_called_once()

    def test_transient_native_writer_clears_before_import_in_both_modes(self):
        stale = (100, SOURCE_ID, str(self.transcript), "interactive")
        for wait_exit in (False, True):
            with self.subTest(wait_exit=wait_exit):
                if self.planfile.exists():
                    self.planfile.unlink()
                for name in ("result.json", "import-request.json"):
                    artifact = self.directory / "status" / name
                    if artifact.exists():
                        artifact.unlink()
                self.args.wait_exit = wait_exit
                rows = [[stale], [], ] if wait_exit else [[stale], [stale], []]
                imported, killed, attached, _ = self.flow(rows=rows)
                with patch.object(h.time, "monotonic", side_effect=[0, 0.1]), patch.object(h.time, "sleep") as sleep:
                    h.run(str(self.planfile))
                sleep.assert_called_once_with(0.2)
                imported.assert_called_once()
                attached.assert_called_once()
                if wait_exit:
                    killed.assert_not_called()

    def test_persistent_competitor_writer_blocks_after_bounded_wait(self):
        plan = self.make_plan()
        competitor = (987, SOURCE_ID, str(self.transcript), "interactive")
        with patch.object(h, "live_rows", return_value=[competitor]) as live, \
                patch.object(h.time, "monotonic", side_effect=[0, 0.1, 5]), \
                patch.object(h.time, "sleep") as sleep:
            with self.assertRaisesRegex(h.HandoffError, "live writer"):
                h.ensure_no_writer(plan)
        self.assertEqual(live.call_count, 2)
        sleep.assert_called_once_with(0.2)

    def test_command_uses_its_own_session_and_sanitized_environment(self):
        child = Mock(pid=4321, returncode=0)
        child.communicate.return_value = ("output", "private details")
        with patch.object(h.subprocess, "Popen", return_value=child) as popen, \
                patch.dict(os.environ, {key: "sentinel" for key in h.MARKERS}):
            self.assertEqual(h.command(["/test/cli", "$(printf sentinel)"], timeout=7), "output")
        self.assertTrue(popen.call_args.kwargs["start_new_session"])
        self.assertTrue(h.MARKERS.isdisjoint(popen.call_args.kwargs["env"]))
        self.assertNotIn("shell", popen.call_args.kwargs)
        child.communicate.assert_called_once_with(timeout=7)

    def test_timeout_cleans_only_new_cli_process_group_and_reaps_parent(self):
        for ignores_term in (False, True):
            with self.subTest(ignores_term=ignores_term):
                child = Mock(pid=4321, returncode=None)
                timeout = subprocess.TimeoutExpired(["/test/cli"], 7)
                child.communicate.side_effect = [timeout, timeout if ignores_term else ("", "")]
                with patch.object(h.subprocess, "Popen", return_value=child), \
                        patch.object(h.os, "killpg") as kill_group, patch.object(h.os, "kill") as kill_pid, \
                        patch.object(h.time, "sleep") as sleep:
                    with self.assertRaisesRegex(h.HandoffError, "timed out"):
                        h.command(["/test/cli"], timeout=7)
                self.assertEqual([call.args for call in kill_group.call_args_list], [
                    (4321, signal.SIGTERM), (4321, signal.SIGKILL)])
                kill_pid.assert_not_called()
                sleep.assert_called_once_with(0.2)
                child.wait.assert_called_once_with(timeout=2)
                child.stdout.close.assert_called_once()
                child.stderr.close.assert_called_once()

    def test_timeout_group_cleanup_refuses_worker_own_group(self):
        child = Mock(pid=os.getpgrp())
        with patch.object(h.os, "killpg") as kill_group, self.assertRaises(h.HandoffError):
            h.stop_timed_out_command(child)
        kill_group.assert_not_called()

    def diagnostic(self, provider, home=None):
        auth = "  Auth: " + json.dumps({"configDirectory": str(home or self.home)}) + "\n"
        return {"provider": provider, "diagnostic": "Claude Code\n" + auth + "  Status: Ready\n"}

    def test_profile_alias_is_local_json_and_env_is_sanitized(self):
        plan = self.make_plan()
        special = self.directory / "profile $(printf sentinel)"
        special.mkdir()
        plan["provider_home"] = str(special)
        alias = "handoff-claude-" + h.hashlib.sha256(str(special).encode()).hexdigest()[:12]
        with patch.object(h, "command") as command, \
                patch.object(h, "json_command", return_value=self.diagnostic(alias, special)):
            self.assertEqual(h.provider_preflight(plan), alias)
        config = json.loads((self.daemon_home / "config.json").read_text())
        self.assertEqual(config["agents"]["providers"][alias]["env"]["CLAUDE_CONFIG_DIR"], str(special))
        self.assertEqual(command.call_args_list[1].args[0][1:3], ["daemon", "reload"])
        self.assertTrue(all("config" not in call.args[0] for call in command.call_args_list))
        with patch.dict(os.environ, {key: "sensitive sentinel" for key in h.MARKERS}):
            env = h.clean_env()
            self.assertTrue(h.MARKERS.isdisjoint(env))

    def test_default_claude_no_alias_and_auth_root_must_match(self):
        plan = self.make_plan()
        default_home = self.directory / ".claude"
        default_home.mkdir()
        plan["provider_home"] = str(default_home)
        with patch.object(h.Path, "home", return_value=self.directory), patch.object(h, "command") as command, \
                patch.object(h, "json_command", return_value=self.diagnostic("claude", default_home)):
            self.assertEqual(h.provider_preflight(plan), "claude")
            self.assertEqual(command.call_count, 1)
        with patch.object(h.Path, "home", return_value=self.directory), patch.object(h, "command"), \
                patch.object(h, "json_command", return_value=self.diagnostic("claude", self.home)):
            with self.assertRaisesRegex(h.HandoffError, "differs"):
                h.provider_preflight(plan)

    def test_codex_always_has_home_alias(self):
        plan = self.make_plan()
        plan["agent"] = "codex"
        alias = "handoff-codex-" + h.hashlib.sha256(str(self.home).encode()).hexdigest()[:12]
        with patch.object(h, "command") as command, patch.object(h, "json_command", return_value={
                "provider": alias, "diagnostic": "Codex\n  Models: 8\n  Status: Ready"}):
            self.assertEqual(h.provider_preflight(plan), alias)
        config = json.loads((self.daemon_home / "config.json").read_text())
        self.assertEqual(config["agents"]["providers"][alias]["env"], {"CODEX_HOME": str(self.home)})
        self.assertTrue(all("config" not in call.args[0] for call in command.call_args_list))

    def test_atomic_alias_merge_preserves_private_values_and_permissions(self):
        filename = self.daemon_home / "config.json"
        original = {"auth": {"token": "private-token-sentinel"}, "arbitrary": [1, {"keep": True}],
                    "agents": {"other": "keep", "providers": {
                        "existing": {"env": {"SECRET_SENTINEL": "private-env-sentinel"}}}}}
        filename.write_text("\ufeff" + json.dumps(original))
        filename.chmod(0o644)
        alias = {"extends": "codex", "env": {"CODEX_HOME": str(self.home)}}
        self.assertTrue(h.merge_provider_alias(str(self.daemon_home), "handoff-test", alias))
        expected = json.loads(json.dumps(original))
        expected["agents"]["providers"]["handoff-test"] = alias
        self.assertEqual(json.loads(filename.read_text()), expected)
        self.assertEqual(stat.S_IMODE(filename.stat().st_mode), 0o600)
        self.assertEqual(list(self.daemon_home.glob(".handoff-config-*")), [])
        before = filename.stat()
        content = filename.read_bytes()
        self.assertFalse(h.merge_provider_alias(str(self.daemon_home), "handoff-test", alias))
        self.assertEqual(filename.read_bytes(), content)
        self.assertEqual(filename.stat().st_ino, before.st_ino)

    def test_alias_merge_preserves_custom_label(self):
        filename = self.daemon_home / "config.json"
        alias = {"extends": "claude", "label": "Imported Claude profile",
                 "env": {"CLAUDE_CONFIG_DIR": str(self.home)}}
        existing = dict(alias, label="Claude (Work)")
        filename.write_text(json.dumps({"agents": {"providers": {"handoff-test": existing}}}))
        content = filename.read_bytes()
        self.assertFalse(h.merge_provider_alias(str(self.daemon_home), "handoff-test", alias))
        self.assertEqual(filename.read_bytes(), content)

    def test_alias_merge_refuses_conflicts_malformed_and_nonobject_config(self):
        filename = self.daemon_home / "config.json"
        alias = {"extends": "codex", "env": {"CODEX_HOME": str(self.home)}}
        invalid = ["{malformed", "[]", '{"agents": []}', '{"agents": {"providers": []}}',
                   '{"agents": {"providers": {"handoff-test": {"extends": "claude"}}}}']
        for content in invalid:
            with self.subTest(content=content):
                filename.write_text(content)
                with self.assertRaises(h.HandoffError):
                    h.merge_provider_alias(str(self.daemon_home), "handoff-test", alias)
                self.assertEqual(filename.read_text(), content)

    def test_alias_merge_refuses_symlink_config_and_preserves_target(self):
        filename = self.daemon_home / "config.json"
        target = self.directory / "private-config.json"
        target.write_text('{"secret": "private-token-sentinel"}')
        filename.symlink_to(target)
        with self.assertRaises(h.HandoffError):
            h.merge_provider_alias(str(self.daemon_home), "handoff-test", {"extends": "codex"})
        self.assertTrue(filename.is_symlink())
        self.assertEqual(target.read_text(), '{"secret": "private-token-sentinel"}')

    def test_alias_merge_detects_external_edit_and_preserves_it(self):
        filename = self.daemon_home / "config.json"
        filename.write_text('{"secret": "original-sentinel"}')
        snapshot = h.config_snapshot
        calls = 0
        def external_edit(candidate):
            nonlocal calls
            calls += 1
            if calls == 2:
                filename.write_text('{"secret": "external-sentinel"}')
            return snapshot(candidate)
        with patch.object(h, "config_snapshot", side_effect=external_edit), self.assertRaisesRegex(h.HandoffError, "changed"):
            h.merge_provider_alias(str(self.daemon_home), "handoff-test", {"extends": "codex"})
        self.assertEqual(filename.read_text(), '{"secret": "external-sentinel"}')
        self.assertEqual(list(self.daemon_home.glob(".handoff-config-*")), [])

    def test_alias_merge_refuses_symlink_home_and_fifo_config(self):
        linked = self.directory / "linked-home"
        linked.symlink_to(self.daemon_home)
        with self.assertRaises(h.HandoffError):
            h.merge_provider_alias(str(linked), "handoff-test", {"extends": "codex"})
        filename = self.daemon_home / "config.json"
        os.mkfifo(filename)
        with self.assertRaises(h.HandoffError):
            h.merge_provider_alias(str(self.daemon_home), "handoff-test", {"extends": "codex"})

    def test_not_ready_or_malformed_diagnostic_fails_closed(self):
        plan = self.make_plan()
        alias = "handoff-claude-" + h.hashlib.sha256(str(self.home).encode()).hexdigest()[:12]
        for value in ({"provider": alias, "diagnostic": "  Status: Unavailable"},
                      {"provider": alias, "diagnostic": "  Auth: {}\n  Status: Ready"},
                      {"provider": "different", "diagnostic": "  Status: Ready"}):
            with self.subTest(value=value), patch.object(h, "command"), patch.object(h, "json_command", return_value=value):
                with self.assertRaises(h.HandoffError):
                    h.provider_preflight(plan)

    def test_wait_detects_pid_reuse_without_killing(self):
        reused = h.Process(100, 2, os.getuid(), "new start", "claude")
        with patch.object(h, "process_table", return_value={100: reused}), patch.object(h.os, "kill") as killed:
            h.wait_gone(self.source, 0)
        killed.assert_not_called()
        with patch.object(h, "process_table", return_value={100: self.source}), self.assertRaises(h.HandoffError):
            h.wait_gone(self.source, 0)

    def test_live_parser_rejects_unknown_shape(self):
        plan = self.make_plan()
        with patch.object(h, "command", return_value="100\twrong\n"), self.assertRaises(h.HandoffError):
            h.live_rows(plan)

    def test_attach_clears_markers_and_only_execs_observer(self):
        plan = self.make_plan()
        with patch.dict(os.environ, {key: "sentinel" for key in h.MARKERS}), patch.object(h.os, "execv") as execute:
            h.attach(plan, IMPORTED_ID)
            self.assertTrue(h.MARKERS.isdisjoint(os.environ))
        execute.assert_called_once_with(str(self.executable), ["paseo", "attach", IMPORTED_ID, "--home", self.args.paseo_home])

    def test_recovery_resumes_exact_uuid_in_cwd_with_matching_profile(self):
        plan = self.make_plan()
        command = h.recovery_command(plan)
        self.assertIn("claude --resume " + SOURCE_ID, command)
        self.assertIn("CLAUDE_CONFIG_DIR=" + str(self.home), command)
        self.assertTrue(command.startswith("cd " + str(self.directory) + " && "))
        default_home = self.directory / ".claude"
        default_home.mkdir()
        plan["provider_home"] = str(default_home)
        with patch.object(h.Path, "home", return_value=self.directory):
            command = h.recovery_command(plan)
        self.assertIn("-u CLAUDE_CONFIG_DIR", command)
        self.assertNotIn("CLAUDE_CONFIG_DIR=", command)

    # Source model and effort, and the post-import follow-up.

    def write_transcript(self, *records):
        self.transcript.write_text("".join(
            (record if isinstance(record, str) else json.dumps(record)) + "\n" for record in records))

    def settings(self, plan):
        return plan["model"], plan["model_source"], plan["effort"]

    def test_claude_model_attachment_wins_and_effort_follows_latest_reply(self):
        self.write_transcript(
            attachment("claude-opus-5[1m]"), reply("claude-opus-5", "high"),
            attachment("claude-opus-5-5[1m]"), reply("claude-opus-5-5", "high"),
            reply("claude-haiku-4-5", "low", isSidechain=True),
            {"type": "user", "message": {"content": 'quotes "assistant" and "attachment"'}},
            reply("claude-opus-5-5", "xhigh"), reply("<synthetic>"),
            '{"type": "assistant", "message": {"model": "still-being-writ')
        plan = self.make_plan()
        self.assertEqual(self.settings(plan), ("claude-opus-5-5[1m]", "attachment", "xhigh"))
        self.assertEqual(self.warnings, [])
        h.validate_plan(plan)

    def test_claude_reply_model_is_only_a_fallback_and_effort_needs_a_real_reply(self):
        self.write_transcript(reply("claude-opus-5-5", "xhigh"), reply("claude-sonnet-5", "max"),
                              reply("<synthetic>"))
        self.assertEqual(h.source_settings("claude", str(self.transcript)), ("claude-sonnet-5", "reply", "max"))
        self.assertEqual(self.warnings, [])
        # Haiku replies carry no effort; an older level must not carry over.
        self.write_transcript(reply("claude-opus-5-5", "xhigh"), reply("claude-haiku-4-5-20251001"))
        self.assertEqual(h.source_settings("claude", str(self.transcript)),
                         ("claude-haiku-4-5-20251001", "reply", None))
        self.assertEqual(len(self.warnings), 1)

    def test_codex_reads_the_latest_turn_context(self):
        self.use_codex()
        self.write_transcript(
            {"type": "session_meta", "payload": {"id": SOURCE_ID}}, turn("gpt-5.5", "high"),
            {"type": "response_item", "payload": {"text": 'prose naming "turn_context"'}},
            turn("gpt-6.1-sol", "xhigh"), {"type": "event_msg", "payload": {"type": "agent_message"}})
        plan = self.make_plan(self.proxy_table(agent="codex"))
        self.assertEqual(self.settings(plan), ("gpt-6.1-sol", "turn_context", "xhigh"))
        h.validate_plan(plan)

    def test_missing_or_malformed_settings_become_null_and_never_block(self):
        cases = [
            [{}],
            [reply("<synthetic>")],
            ["not json", "[1, 2]", '"assistant"'],
            [attachment("claude opus; $(printf sentinel)"), reply("claude-opus-5-5", "turbo")],
            [{"type": "attachment", "attachment": {"type": "model", "identity": "claude-opus-5-5"}},
             reply("claude-opus-5-5", ["high"])],
            [attachment("c" * 200), reply("claude-opus-5-5", 5)],
            [reply(None, None), {"type": "assistant", "message": "claude-opus-5-5", "effort": {}}],
        ]
        for records in cases:
            with self.subTest(records=records):
                self.warnings.clear()
                if self.planfile.exists():
                    self.planfile.unlink()
                self.write_transcript(*records)
                plan = self.make_plan()
                self.assertEqual(self.settings(plan), (None, None, None))
                self.assertEqual(len(self.warnings), 2)
                h.validate_plan(plan)
        self.use_codex()
        self.write_transcript({"type": "turn_context", "payload": "gpt-5.5"})
        self.assertEqual(h.source_settings("codex", str(self.transcript)), (None, None, None))
        with patch.object(h, "scan_transcript", side_effect=OSError("private path")):
            self.assertEqual(h.source_settings("claude", str(self.transcript)), (None, None, None))
        self.assertNotIn("private path", " ".join(self.warnings))

    def test_saved_model_effort_or_origin_invalid_refused(self):
        self.write_transcript(attachment("claude-opus-5-5[1m]"), reply("claude-opus-5-5", "xhigh"))
        plan = self.make_plan()
        for changed in ({"model": "bad model"}, {"model": ["claude-opus-5-5"]}, {"effort": "turbo"},
                        {"effort": ["high"]}, {"model_source": "guess"}):
            with self.subTest(changed=changed), self.assertRaises(h.HandoffError):
                h.validate_plan(plan | changed)

    def followup_flow(self, *, fail=None, helper_fail=False, records=None):
        fake_cli(self.executable, self.log, "paseo", fail=fail)
        if helper_fail:
            fake_cli(self.helper, self.log, "model", fail="*")
        self.write_transcript(*(records or [attachment("claude-opus-5-5[1m]"), reply("claude-opus-5-5", "xhigh")]))
        return self.flow()

    def log_lines(self):
        return self.log.read_text().splitlines() if self.log.exists() else []

    def expected_calls(self, *, model="claude-opus-5-5[1m]", effort="xhigh"):
        home, status = self.args.paseo_home, self.directory / "status"
        calls = []
        if model:
            calls.append(f"model --paseo {self.executable} --home {home} --agent-id {IMPORTED_ID} --model {model}")
        if effort:
            calls.append(f"paseo agent update {IMPORTED_ID} --thinking {effort} --home {home}")
        calls.append(f"paseo send --no-wait --home {home} --prompt-file {status / 'followup-prompt.txt'} {IMPORTED_ID}")
        return calls

    def followup_record(self):
        return json.loads((self.directory / "status" / "followup.json").read_text())

    def test_followup_sets_model_then_effort_then_briefs_before_attach(self):
        _, _, attached, _ = self.followup_flow()
        status = self.directory / "status"
        attached.side_effect = lambda *args: self.assertTrue((status / "followup.json").exists())
        h.run(str(self.planfile))
        attached.assert_called_once()
        self.assertEqual(self.log_lines(), self.expected_calls())
        record = self.followup_record()
        self.assertEqual(stat.S_IMODE((status / "followup.json").stat().st_mode), 0o600)
        self.assertEqual((record["agentId"], record["sourceId"]), (IMPORTED_ID, SOURCE_ID))
        self.assertEqual([record[step]["status"] for step in ("model", "effort", "message")], ["ok"] * 3)
        self.assertEqual((record["model"]["value"], record["effort"]["value"]), ("claude-opus-5-5[1m]", "xhigh"))
        prompt = status / "followup-prompt.txt"
        self.assertEqual(stat.S_IMODE(prompt.stat().st_mode), 0o600)
        text = prompt.read_text()
        for expected in ("running in Paseo as agent " + IMPORTED_ID,
                         "native Claude Code session " + SOURCE_ID,
                         "(working directory " + str(self.directory) + ")", "stopped the original process",
                         "Continue the task", "cron jobs", "Paseo schedules or heartbeats", "`paseo` skill",
                         "subagents", "worktrees", "workflows", "monitors", "Holds", "banner",
                         "model claude-opus-5-5[1m], effort xhigh",
                         "`paseo-model-update` skill in status-only mode", "update_agent",
                         "setting the model succeeded, and setting the effort succeeded"):
            self.assertIn(expected, text)
        self.assertFalse({"\u2014", "\u2013"} & set(text))  # no em or en dashes
        self.assertIn("model ok, effort ok, briefing ok", self.stdout.getvalue())
        attached.side_effect = None
        h.run(str(self.planfile))
        self.assertEqual(attached.call_count, 2)
        self.assertEqual(self.log_lines(), self.expected_calls())  # nothing repeated

    def test_followup_failures_are_recorded_and_never_stop_the_handoff(self):
        _, _, attached, _ = self.followup_flow(fail="agent", helper_fail=True)
        h.run(str(self.planfile))
        attached.assert_called_once()
        self.assertEqual(self.log_lines(), self.expected_calls())  # effort still tried after the model
        record = self.followup_record()
        self.assertEqual([record[step]["status"] for step in ("model", "effort", "message")],
                         ["failed", "failed", "ok"])
        self.assertIn("withheld", record["model"]["detail"])
        self.assertNotIn("sentinel", json.dumps(record))
        text = (self.directory / "status" / "followup-prompt.txt").read_text()
        self.assertIn("setting the model failed (External command failed", text)
        self.assertIn("setting the effort failed (External command failed", text)

    def test_briefing_or_node_failure_is_recorded_and_attach_still_runs(self):
        _, _, attached, _ = self.followup_flow(fail="send")
        with patch.object(h, "model_helper_argv", side_effect=h.HandoffError("Node.js is unavailable for the model update")):
            h.run(str(self.planfile))
        attached.assert_called_once()
        self.assertTrue((self.directory / "status" / "result.json").exists())
        self.assertEqual(self.log_lines(), self.expected_calls()[1:])
        record = self.followup_record()
        self.assertEqual(record["model"], {"value": "claude-opus-5-5[1m]", "status": "failed",
                                           "detail": "Node.js is unavailable for the model update"})
        self.assertEqual((record["effort"]["status"], record["message"]["status"]), ("ok", "failed"))
        self.assertIn("briefing failed", self.stdout.getvalue())

    def test_auto_or_unknown_settings_are_skipped_but_the_briefing_is_sent(self):
        cases = [([reply("claude-opus-5-5", "auto")], "claude-opus-5-5", None),
                 ([{}], None, None)]
        for records, model, effort in cases:
            with self.subTest(records=records):
                for leftover in (self.planfile, self.log):
                    if leftover.exists():
                        leftover.unlink()
                shutil.rmtree(self.directory / "status", ignore_errors=True)
                _, _, attached, _ = self.followup_flow(records=records)
                h.run(str(self.planfile))
                attached.assert_called_once()
                self.assertEqual(self.log_lines(), self.expected_calls(model=model, effort=effort))
                record = self.followup_record()
                self.assertEqual(record["effort"]["status"], "skipped")
                text = (self.directory / "status" / "followup-prompt.txt").read_text()
                if model:
                    self.assertIn("auto keeps the provider default", text)
                    self.assertIn("omits a context suffix", text)
                else:
                    self.assertIn("model unknown, effort unknown", text)
                    self.assertIn("setting the model was skipped (unknown in the source transcript)", text)

    def test_rerun_after_an_unrecorded_followup_does_it_once(self):
        imported, _, attached, _ = self.followup_flow()
        with patch.object(h, "follow_up") as interrupted:  # The worker died after saving the result.
            h.run(str(self.planfile))
        interrupted.assert_called_once()
        self.assertEqual(self.log_lines(), [])
        stale = self.directory / "status" / "followup-prompt.txt"
        stale.write_text("stale\n")
        h.run(str(self.planfile))
        self.assertEqual(self.log_lines(), self.expected_calls())
        self.assertNotIn("stale", stale.read_text())
        h.run(str(self.planfile))
        self.assertEqual(self.log_lines(), self.expected_calls())
        imported.assert_called_once()
        self.assertEqual(attached.call_count, 3)

    def test_plan_from_before_the_followup_never_briefs_an_old_agent(self):
        plan = self.make_plan()
        for key in ("model", "model_source", "effort"):
            del plan[key]
        h.validate_plan(plan)
        status = self.directory / "status"
        status.mkdir(mode=0o700)
        fake_cli(self.executable, self.log, "paseo")
        self.assertIsNone(h.follow_up(plan, status, IMPORTED_ID))
        self.assertFalse((status / "followup.json").exists())
        self.assertEqual(self.log_lines(), [])

    def test_model_helper_prefers_the_node_beside_the_paseo_shim(self):
        plan = {"paseo": str(self.executable)}
        with patch.object(h.shutil, "which", return_value="/test/node"):
            self.assertEqual(h.model_helper_argv(plan), ["/test/node", str(HELPER)])
        sibling = self.directory / "node"
        fake_cli(sibling, self.log, "node")
        self.assertEqual(h.model_helper_argv(plan), [str(sibling), str(HELPER)])
        sibling.unlink()
        with patch.object(h.shutil, "which", return_value=None), self.assertRaisesRegex(h.HandoffError, "Node.js"):
            h.model_helper_argv(plan)


def attachment(model):
    return {"type": "attachment", "isSidechain": False, "attachment": {
        "type": "model", "identity": {"modelId": model, "marketingName": "M", "knowledgeCutoff": "K"}}}


def reply(model, effort=None, **extra):
    record = {"type": "assistant", "isSidechain": False, "message": {"model": model}}
    if effort is not None:
        record["effort"] = effort
    return record | extra


def turn(model, effort):
    return {"type": "turn_context", "payload": {"model": model, "effort": effort, "cwd": "/"}}


FAKE_CLIENT = """\
import { appendFileSync } from "node:fs";
const log = (line) => appendFileSync(process.env.FAKE_CLIENT_LOG, line + "\\n");
export async function connectToDaemon(options) {
    log("connect " + JSON.stringify(options));
    if (process.env.FAKE_CLIENT_FAIL === "connect")
        throw { code: "DAEMON_UNREACHABLE", message: "no daemon sentinel" };
    return {
        getLastServerInfoMessage: () => ({ version: "fake-daemon" }),
        setAgentModel: async (agentId, model) => {
            log("setAgentModel " + agentId + " " + model);
            if (process.env.FAKE_CLIENT_FAIL === "set") throw new Error("rejected sentinel");
        },
        close: async () => log("close"),
    };
}
"""
FAKE_TARGET = """\
export function selectDaemonTarget(options, env, localOnly) {
    return { kind: "instance", home: options.home, env: Object.keys(env), localOnly };
}
"""


@unittest.skipUnless(shutil.which("node"), "Node.js is not installed")
class ModelHelperTests(unittest.TestCase):
    """The Node helper against a fake @getpaseo/cli package: no daemon involved."""

    def setUp(self):
        temp = tempfile.TemporaryDirectory()
        self.addCleanup(temp.cleanup)
        self.root = Path(temp.name).resolve()
        self.log = self.root / "client.log"
        self.home = str(self.root / "paseo-home")
        package = self.root / "pnpm" / "global" / "5" / ".pnpm" / "cli" / "node_modules" / "@getpaseo" / "cli"
        (package / "bin").mkdir(parents=True)
        (package / "dist" / "utils").mkdir(parents=True)
        (package / "package.json").write_text(json.dumps({"name": "@getpaseo/cli", "version": "9.9.9",
                                                          "type": "module"}))
        (package / "bin" / "paseo").write_text("#!/bin/sh\nprintf sentinel\n")
        (package / "dist" / "utils" / "client.js").write_text(FAKE_CLIENT)
        (package / "dist" / "utils" / "daemon-target.js").write_text(FAKE_TARGET)
        self.entry = package / "bin" / "paseo"
        # Like a pnpm shim: paths relative to $basedir, and the last one runs.
        self.shim = self.root / "pnpm" / "paseo"
        self.shim.write_text(
            '#!/bin/sh\nbasedir=$(dirname "$0")\n'
            'exec node "$basedir/global/5/.pnpm/stale/node_modules/@getpaseo/cli/bin/paseo" "$@"\n'
            'exec node "$basedir/global/5/.pnpm/cli/node_modules/@getpaseo/cli/bin/paseo" "$@"\n')

    def helper(self, *args, fail=None):
        env = dict(os.environ, FAKE_CLIENT_LOG=str(self.log), PASEO_HOST="127.0.0.1:1")
        if fail:
            env["FAKE_CLIENT_FAIL"] = fail
        return subprocess.run(["node", str(HELPER), *args], capture_output=True, text=True, env=env, timeout=60)

    def log_lines(self):
        return self.log.read_text().splitlines() if self.log.exists() else []

    def connect_line(self):
        target = {"kind": "instance", "home": self.home, "env": [], "localOnly": False}
        return "connect " + json.dumps({"target": target}, separators=(",", ":"))

    def test_sets_the_model_through_the_cli_behind_a_pnpm_shim(self):
        done = self.helper("--paseo", str(self.shim), "--home", self.home,
                           "--agent-id", IMPORTED_ID, "--model", "claude-opus-5-5[1m]")
        self.assertEqual(done.returncode, 0, done.stderr)
        self.assertEqual(json.loads(done.stdout), {"ok": True, "mode": "set", "agentId": IMPORTED_ID,
                                                   "model": "claude-opus-5-5[1m]"})
        self.assertEqual(self.log_lines(), [self.connect_line(),
                                            "setAgentModel " + IMPORTED_ID + " claude-opus-5-5[1m]", "close"])

    def test_check_only_connects_and_disconnects_also_through_a_direct_link(self):
        link = self.root / "paseo-link"
        link.symlink_to(self.entry)
        for executable in (self.shim, link):
            with self.subTest(executable=executable):
                self.log.unlink(missing_ok=True)
                done = self.helper("--paseo", str(executable), "--home", self.home, "--check")
                self.assertEqual(done.returncode, 0, done.stderr)
                self.assertEqual(json.loads(done.stdout), {"ok": True, "mode": "check", "cliVersion": "9.9.9",
                                                           "daemonVersion": "fake-daemon"})
                self.assertEqual(self.log_lines(), [self.connect_line(), "close"])

    def test_daemon_failures_exit_nonzero_and_still_close(self):
        args = ("--paseo", str(self.shim), "--home", self.home, "--agent-id", IMPORTED_ID, "--model", "gpt-6.1-sol")
        done = self.helper(*args, fail="set")
        self.assertEqual(done.returncode, 1)
        self.assertIn("rejected sentinel", done.stderr)
        self.assertEqual(self.log_lines()[-1], "close")
        self.log.unlink()
        done = self.helper(*args, fail="connect")
        self.assertEqual(done.returncode, 1)
        self.assertIn("no daemon sentinel", done.stderr)
        self.assertEqual(self.log_lines(), [self.connect_line()])

    def test_refuses_bad_arguments_and_unknown_installs_before_connecting(self):
        base = ("--paseo", str(self.shim), "--home", self.home)
        for args in ((*base, "--agent-id", IMPORTED_ID, "--model", "claude opus"),
                     (*base, "--agent-id", IMPORTED_ID, "--model", "-rf"),
                     (*base, "--agent-id", "not-a-uuid", "--model", "gpt-6.1-sol"),
                     (*base, "--check", "--model", "gpt-6.1-sol"),
                     ("--paseo", str(self.shim), "--home", "relative", "--check"),
                     ("--home", self.home, "--check"),
                     (*base, "--check", "--unknown")):
            with self.subTest(args=args):
                done = self.helper(*args)
                self.assertEqual(done.returncode, 64, done.stderr)
        other = self.root / "other-paseo"
        other.write_text("#!/bin/sh\nprintf sentinel\n")
        done = self.helper("--paseo", str(other), "--home", self.home, "--check")
        self.assertEqual(done.returncode, 1)
        self.assertIn("cannot find the @getpaseo/cli package", done.stderr)
        self.assertEqual(self.log_lines(), [])


if __name__ == "__main__":
    unittest.main()
