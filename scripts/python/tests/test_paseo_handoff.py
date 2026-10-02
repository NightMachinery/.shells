"""Safety-boundary tests: all process inspection and signals are mocked."""
import argparse
from contextlib import ExitStack
import importlib.util
import json
import os
from pathlib import Path
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


class HandoffTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.directory = Path(self.temp.name).resolve()
        self.directory.chmod(0o700)
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

    def make_plan(self):
        with patch.object(h, "process_table", return_value={100: self.source, 200: self.caller}), \
                patch.object(h, "command", return_value=self.meta):
            h.prepare(self.args)
        return json.loads(self.planfile.read_text())

    def flow(self, *, rows=None, response=None, identities=None, wait_error=False, preflight_error=False):
        self.make_plan()
        stack = ExitStack()
        self.addCleanup(stack.close)
        stack.enter_context(patch.object(h, "verify_native"))
        preflight = stack.enter_context(patch.object(h, "provider_preflight", return_value="claude"))
        if preflight_error:
            preflight.side_effect = h.HandoffError("provider failed")
        wait = stack.enter_context(patch.object(h, "wait_gone"))
        if wait_error:
            wait.side_effect = [None, h.HandoffError("timeout")]
        table = stack.enter_context(patch.object(h, "process_table", return_value={100: self.source}))
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
                        h.Process(100, 2, os.getuid(), "s", "/bin/sleep")):
            with self.subTest(process=process), self.assertRaises(h.HandoffError):
                h.source_allowed(process, "claude")

    def test_uuid_and_store_mismatch_refused(self):
        self.args.id = SOURCE_ID.upper()
        with self.assertRaises(h.HandoffError):
            h.prepare(self.args)
        self.args.id = SOURCE_ID
        plan = self.make_plan()
        plan["id"] = IMPORTED_ID
        with self.assertRaises(h.HandoffError):
            h.verify_native(plan)

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


if __name__ == "__main__":
    unittest.main()
