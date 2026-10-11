#!/usr/bin/env python3
"""Transfer one verified native coding-agent session to a local Paseo daemon.

The shell prepares a private plan and launches ``run`` in an independent tmux
session. No shell commands, native-source process groups, or environment
snapshots are used. Each external CLI gets its own process group for timeout
cleanup. External-command output is deliberately not echoed. After the import,
a best-effort follow-up carries the source model and effort over and sends the
agent one fixed briefing about what the move broke.

With ``--target-home`` (``2paseo --to``), a Claude session moves to another
account: once the source has exited, its transcript and sidecar directory are
copied into the target config directory, never overwriting anything, and the
copy is imported with the target's provider. The source files stay untouched.
"""

import argparse
from contextlib import contextmanager
from dataclasses import asdict, dataclass
import fcntl
import hashlib
import json
import os
from pathlib import Path
import re
import shlex
import shutil
import signal
import stat
import subprocess
import sys
import tempfile
import time
import uuid


MARKERS = {
    "CLAUDECODE", "CLAUDE_CODE_SESSION_ID", "CODEX_THREAD_ID", "CODEX_SESSION_ID",
    "AI_AGENT", "AGENT_SESSION_REUSE_PANE", "AGENT_SESSION_STATE",
    "PASEO_AGENT_ID", "PASEO_HOST", "CLAUDE_CONFIG_DIR", "CODEX_HOME",
}
UUID_RE = re.compile(r"[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}\Z")
# macOS reports some system daemons' uid as negative (nobody is -2). Such a
# process can never be ours, since ownership is checked against os.getuid().
PS_RE = re.compile(
    r"^\s*(\d+)\s+(\d+)\s+(-?\d+)\s+"
    r"((?:Mon|Tue|Wed|Thu|Fri|Sat|Sun)\s+"
    r"(?:Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)\s+"
    r"\d{1,2}\s+\d{2}:\d{2}:\d{2}\s+\d{4})\s+(.+?)\s*$"
)
# Model ids such as claude-opus-5-5[1m], claude-haiku-4-5-20251001 or
# gpt-6.1-sol. The daemon stores a model without checking it, so this only
# refuses garbage; the value itself must come from the source transcript.
MODEL_RE = re.compile(r"[A-Za-z0-9][A-Za-z0-9._:@-]{0,99}(?:\[[0-9]{1,4}[km]\])?\Z")
EFFORTS = frozenset({"none", "minimal", "low", "medium", "high", "xhigh", "max",
                     "ultra", "ultracode", "auto"})
MODEL_SOURCES = (None, "attachment", "reply", "turn_context")
MODEL_HELPER = Path(__file__).resolve().with_name("paseo_set_model.mjs")


class HandoffError(Exception):
    pass


@dataclass(frozen=True)
class Process:
    pid: int
    ppid: int
    uid: int
    started: str
    command: str


def clean_env():
    return {key: value for key, value in os.environ.items() if key not in MARKERS}


def stop_timed_out_command(child):
    # start_new_session=True gives only this newly launched command a private
    # process group. Never discover or signal a group from the native source.
    if child.pid <= 1 or child.pid in {os.getpid(), os.getpgrp()}:
        raise HandoffError("Unsafe external-command process group; refusing cleanup")
    print("Terminating timed-out CLI process group " + str(child.pid), file=sys.stderr, flush=True)
    try:
        os.killpg(child.pid, signal.SIGTERM)
    except ProcessLookupError:
        pass
    # Keep the direct child unreaped during signaling, reserving its PID while
    # we finish group cleanup. Waiting here could release the group leader's
    # PID before signaling descendants that ignored TERM.
    time.sleep(0.2)
    # A descendant can ignore TERM or close its pipes while continuing to run.
    # Clean the entire group, including that case, before returning the error.
    try:
        os.killpg(child.pid, signal.SIGKILL)
    except ProcessLookupError:
        pass
    try:
        child.communicate(timeout=2)
    except subprocess.TimeoutExpired:
        pass
    finally:
        for pipe in (child.stdout, child.stderr):
            if pipe is not None:
                pipe.close()
    try:
        child.wait(timeout=2)
    except subprocess.TimeoutExpired as exc:
        raise HandoffError("Timed-out external command did not exit after cleanup") from exc


def command(argv, *, timeout=60, env=None):
    try:
        child = subprocess.Popen(argv, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                                 text=True, env=env or clean_env(), start_new_session=True)
    except OSError as exc:
        raise HandoffError("External command unavailable") from exc
    try:
        stdout, _ = child.communicate(timeout=timeout)
    except subprocess.TimeoutExpired as exc:
        stop_timed_out_command(child)
        raise HandoffError("External command unavailable or timed out") from exc
    if child.returncode:
        # Diagnostics and native-tool errors can contain credentials or names.
        raise HandoffError("External command failed; its private output was withheld")
    return stdout


def json_command(argv, *, timeout=60):
    try:
        value = json.loads(command(argv, timeout=timeout))
    except json.JSONDecodeError as exc:
        raise HandoffError("External command returned invalid JSON") from exc
    if not isinstance(value, dict):
        raise HandoffError("External command returned an unexpected JSON shape")
    return value


def process_table():
    env = clean_env()
    env["LC_ALL"] = "C"
    output = command(["/bin/ps", "-axo", "pid=,ppid=,uid=,lstart=,comm="], env=env)
    table = {}
    for line in output.splitlines():
        if not line.strip():
            continue
        match = PS_RE.fullmatch(line)
        if not match:
            raise HandoffError("Cannot safely parse the process table")
        pid, ppid, uid, started, executable = match.groups()
        process = Process(int(pid), int(ppid), int(uid), " ".join(started.split()), executable)
        if process.pid in table:
            raise HandoffError("Duplicate process identity in process table")
        table[process.pid] = process
    return table


def valid_uuid(value):
    # Claude Code keeps a session id in the case it was given, so a session
    # started with macOS `uuidgen` has an uppercase id and transcript name, and
    # Paseo opens `<id>.jsonl` as spelled. Keep the case; refuse a mixed one,
    # which is no session's spelling.
    if not isinstance(value, str) or not UUID_RE.fullmatch(value):
        raise HandoffError("Expected a canonical native session UUID")
    canonical = str(uuid.UUID(value))
    if value not in (canonical, canonical.upper()):
        raise HandoffError("Expected a canonical native session UUID")
    return value


def absolute_path(value, *, kind=None):
    if not isinstance(value, str) or not os.path.isabs(value) or any(c in value for c in "\n\r\t\0"):
        raise HandoffError("Expected an absolute path without control characters")
    result = Path(value).resolve()
    if kind == "file" and not result.is_file():
        raise HandoffError("Required file does not exist")
    if kind == "dir" and not result.is_dir():
        raise HandoffError("Required directory does not exist")
    if kind == "executable" and (not result.is_file() or not os.access(result, os.X_OK)):
        raise HandoffError("Required executable does not exist")
    return str(result)


def private_directory(directory):
    info = Path(directory).lstat()
    if not stat.S_ISDIR(info.st_mode) or info.st_uid != os.getuid() or info.st_mode & 0o077:
        raise HandoffError("Plan directory must belong to you and have mode 0700")


def write_private_text(filename, text):
    flags = os.O_WRONLY | os.O_CREAT | os.O_EXCL
    flags |= getattr(os, "O_NOFOLLOW", 0)
    try:
        fd = os.open(filename, flags, 0o600)
    except OSError as exc:
        raise HandoffError("Private state already exists or cannot be created") from exc
    with os.fdopen(fd, "w", encoding="utf-8") as handle:
        handle.write(text)
        handle.flush()
        os.fsync(handle.fileno())


def write_private(filename, value):
    write_private_text(filename, json.dumps(value, sort_keys=True, indent=2) + "\n")


def read_private(filename):
    flags = os.O_RDONLY | getattr(os, "O_NOFOLLOW", 0)
    try:
        fd = os.open(filename, flags)
        with os.fdopen(fd) as handle:
            info = os.fstat(handle.fileno())
            if not stat.S_ISREG(info.st_mode) or info.st_uid != os.getuid() or info.st_mode & 0o077:
                raise HandoffError("Private state must be an owned mode-0600 regular file")
            return json.load(handle)
    except (OSError, ValueError) as exc:
        raise HandoffError("Cannot read private handoff state") from exc


def native_names(agent):
    return {"claude", "claude.exe"} if agent == "claude" else {"codex"}


def source_allowed(process, agent):
    if process.uid != os.getuid() or Path(process.command).name not in native_names(agent) | {"node"}:
        raise HandoffError("Source process has the wrong owner or executable")


def owner_allowed(process, agent):
    if process.uid != os.getuid() or Path(process.command).name not in native_names(agent) | {"node", "decset-rewrite"}:
        raise HandoffError("Source owner has the wrong owner or executable")


def ancestry(table, caller, owner):
    """Return the same-user path from owner to caller, or no proven path."""
    seen = set()
    current = caller.pid
    chain = []
    while current not in seen and current in table:
        seen.add(current)
        process = table[current]
        if process.uid != os.getuid():
            return []
        chain.append(process)
        if process.pid == owner.pid:
            return list(reversed(chain))
        current = process.ppid
    return []


def ancestor(table, caller, source):
    return caller.pid != source.pid and bool(ancestry(table, caller, source))


def select_source(table, caller, owner, agent):
    owner_allowed(owner, agent)
    chain = ancestry(table, caller, owner)
    if not chain or len(chain) < 2:
        raise HandoffError("Invoking caller is not a descendant of the source process")
    # Prefer the outermost actual agent binary over its Node/proxy launchers.
    # Ignore arbitrary Node tools nearer the invoking shell when a native
    # binary exists; without one only the owner or known proxy's outermost
    # Node child can stand in for the native agent.
    candidates = chain[:-1]
    source = next((p for p in candidates if Path(p.command).name in native_names(agent)), None)
    if source is None:
        source = next((p for p in candidates if Path(p.command).name == "node"), None)
    if source is None:
        raise HandoffError("Known source proxy has no native agent on the invoking caller ancestry")
    source_allowed(source, agent)
    return source, chain[:chain.index(source)]


def store_root(plan):
    return str(Path(plan["provider_home"]) / ("projects" if plan["agent"] == "claude" else "sessions"))


def default_claude_home():
    return str((Path.home() / ".claude").resolve())


def provider_name(agent, home):
    """Name the Paseo provider entry for a config home.

    The daemon's default Claude config uses the built-in entry; every other
    home gets an alias named after it.
    """
    if agent == "claude" and home == default_claude_home():
        return agent
    return "handoff-" + agent + "-" + hashlib.sha256(home.encode()).hexdigest()[:12]


def import_home(plan):
    """Return the config home Paseo resumes the session under."""
    return plan.get("target_home") or plan["provider_home"]


def sidecar_path(transcript, session_id):
    # Subagent transcripts and tool results live beside <id>.jsonl in <id>/.
    return str(Path(transcript).parent / session_id)


def target_layout(plan, target):
    """Validate a --to config directory and return it with the destination transcript.

    The copy keeps the source's project folder name. Paseo derives that name
    from the same cwd, so its import finds the copy under the target.
    """
    if plan["agent"] != "claude":
        raise HandoffError("--to moves Claude sessions only; a Codex session keeps its CODEX_HOME")
    home = absolute_path(target, kind="dir")
    if home == plan["provider_home"]:
        raise HandoffError("--to names the session's own config directory; drop --to to stay there")
    projects = Path(home) / "projects"
    if not projects.is_dir():
        raise HandoffError("--to is not a Claude config directory (it has no projects/): " + home)
    transcript = Path(plan["transcript"])
    source_projects = Path(store_root(plan)).resolve()
    if transcript.parent.parent != source_projects:
        raise HandoffError("Transcript is not in a project folder of its config directory")
    projects = projects.resolve()
    if projects == source_projects:
        raise HandoffError("--to shares its projects/ with the session's own config directory")
    return home, str(projects / transcript.parent.name / transcript.name)


def check_destination(plan):
    """Refuse a destination that exists: the copy never overwrites."""
    for path in (plan["target_transcript"], sidecar_path(plan["target_transcript"], plan["id"])):
        if os.path.lexists(path):
            raise HandoffError("Destination already exists, and 2paseo never overwrites it: " + path)


def verify_native(plan):
    root = Path(store_root(plan)).resolve()
    transcript = Path(plan["transcript"])
    if not transcript.is_relative_to(root) or not transcript.is_file():
        raise HandoffError("Transcript is outside its native provider store")
    if plan["agent"] == "claude":
        filename_ok = transcript.name == plan["id"] + ".jsonl"
    else:
        filename_ok = transcript.name.endswith("-" + plan["id"] + ".jsonl")
    if not filename_ok:
        raise HandoffError("Transcript filename does not match the native UUID")
    meta = command([plan["agent_session"], plan["agent"], "meta", plan["transcript"]])
    fields = meta.rstrip("\n").split("\t")
    if len(fields) != 3 or fields[0] != plan["id"] or absolute_path(fields[2], kind="dir") != plan["cwd"]:
        raise HandoffError("Native metadata does not match the requested UUID and cwd")


def warn(text):
    print("paseo-handoff: warning: " + text, file=sys.stderr, flush=True)


def valid_model(value):
    return isinstance(value, str) and MODEL_RE.match(value) is not None


def scan_transcript(agent, transcript):
    """Return the latest model candidates and effort seen in a native transcript.

    Keys are present only when the transcript has that kind of record. Claude's
    ``model`` attachment keeps a context suffix such as ``[1m]`` that its
    replies' ``message.model`` drops; Codex records both in ``turn_context``.
    """
    found = {}
    with open(transcript, "rb") as handle:
        for line in handle:
            # Skip the bulk of the transcript without parsing it.
            if agent == "claude" and b'"attachment"' not in line and b'"assistant"' not in line:
                continue
            if agent == "codex" and b'"turn_context"' not in line:
                continue
            try:
                record = json.loads(line)
            except ValueError:
                continue  # A line the live session is still writing.
            if not isinstance(record, dict) or record.get("isSidechain") is True:
                continue
            if agent == "codex":
                if record.get("type") == "turn_context":
                    payload = record.get("payload")
                    payload = payload if isinstance(payload, dict) else {}
                    found["turn_context"] = payload.get("model")
                    found["effort"] = payload.get("effort")
                continue
            attachment = record.get("attachment")
            if isinstance(attachment, dict) and attachment.get("type") == "model":
                identity = attachment.get("identity")
                found["attachment"] = identity.get("modelId") if isinstance(identity, dict) else None
            if record.get("type") == "assistant":
                message = record.get("message")
                model = message.get("model") if isinstance(message, dict) else None
                # Synthetic replies (API errors and the like) carry no real
                # model or effort. A real reply without an effort, such as
                # Haiku's, means none applies: do not carry an older one over.
                if model != "<synthetic>":
                    found["reply"] = model
                    found["effort"] = record.get("effort")
    return found


def source_settings(agent, transcript):
    """Return (model, model source, effort) the native session last used.

    Each is None when unknown. Missing or malformed values only warn: these
    settings must never block a handoff.
    """
    try:
        found = scan_transcript(agent, transcript)
    except Exception:  # Best effort by design: see the docstring.
        warn("cannot read the source model and effort; Paseo provider defaults apply")
        return None, None, None
    order = ("attachment", "reply") if agent == "claude" else ("turn_context",)
    source = next((key for key in order if key in found), None)
    model = found.get(source)
    if source is None or model is None:
        warn("source model unknown; the Paseo agent keeps its provider default model")
        model = source = None
    elif not valid_model(model):
        warn("source model is malformed; the Paseo agent keeps its provider default model")
        model = source = None
    effort = found.get("effort")
    if effort is None:
        warn("source effort unknown; the Paseo agent keeps its provider default effort")
    elif not isinstance(effort, str) or effort not in EFFORTS:
        warn("source effort is not a known level; the Paseo agent keeps its provider default effort")
        effort = None
    return model, source, effort


def prepare(args):
    plan = {"version": 1, "uid": os.getuid(), "agent": args.agent,
            "id": valid_uuid(args.id), "wait_exit": bool(args.wait_exit),
            "tmux_session": args.tmux_session}
    if not re.fullmatch(r"[A-Za-z0-9_-]+", args.tmux_session):
        raise HandoffError("Unsafe tmux session name")
    for key, kind in (("transcript", "file"), ("cwd", "dir"), ("provider_home", "dir"),
                      ("paseo", "executable"), ("agent_session", "executable"), ("paseo_home", None)):
        plan[key] = absolute_path(getattr(args, key), kind=kind)
    target = getattr(args, "target_home", None)
    if target is not None:
        plan["target_home"], plan["target_transcript"] = target_layout(plan, target)
    table = process_table()
    owner, caller = table.get(args.source_pid), table.get(args.caller_pid)
    if owner is None or caller is None or owner.pid <= 1 or owner.pid == caller.pid:
        raise HandoffError("Source or invoking caller process is missing")
    source, launchers = select_source(table, caller, owner, args.agent)
    plan["source"] = asdict(source)
    if source.pid != owner.pid:
        plan["owner"] = asdict(owner)
        # These snapshots prove the saved target's parent links to the owner.
        plan["source_ancestors"] = [asdict(p) for p in reversed(launchers[1:])]
    plan["caller"] = asdict(caller)
    verify_native(plan)
    if "target_home" in plan:
        # Refuse here, while the source still runs; run repeats both checks
        # before it stops anything, since either can change in between.
        check_destination(plan)
        plan["target_provider"] = provider_preflight(plan)
    # Always present, even when null: their presence marks a plan that expects
    # the post-import follow-up (see follow_up).
    plan["model"], plan["model_source"], plan["effort"] = source_settings(args.agent, plan["transcript"])
    output = absolute_path(args.output)
    private_directory(Path(output).parent)
    write_private(output, plan)
    return output


def validate_plan(plan):
    if not isinstance(plan, dict) or plan.get("version") != 1 or plan.get("uid") != os.getuid():
        raise HandoffError("Invalid handoff plan version or owner")
    if plan.get("agent") not in {"claude", "codex"} or type(plan.get("wait_exit")) is not bool:
        raise HandoffError("Invalid handoff plan agent or mode")
    valid_uuid(plan.get("id"))
    if not isinstance(plan.get("tmux_session"), str) or not re.fullmatch(r"[A-Za-z0-9_-]+", plan["tmux_session"]):
        raise HandoffError("Invalid handoff terminal name")
    for key, kind in (("transcript", "file"), ("cwd", "dir"), ("provider_home", "dir"),
                      ("paseo", "executable"), ("agent_session", "executable"), ("paseo_home", None)):
        if absolute_path(plan.get(key), kind=kind) != plan[key]:
            raise HandoffError("Handoff path changed since preparation")
    if plan.get("model") is not None and not valid_model(plan["model"]):
        raise HandoffError("Invalid saved source model")
    if plan.get("effort") is not None and (not isinstance(plan["effort"], str) or plan["effort"] not in EFFORTS):
        raise HandoffError("Invalid saved source effort")
    if plan.get("model_source") not in MODEL_SOURCES:
        raise HandoffError("Invalid saved source model origin")
    if any(key in plan for key in ("target_home", "target_transcript", "target_provider")):
        try:
            layout = target_layout(plan, plan.get("target_home"))
        except KeyError as exc:
            raise HandoffError("Invalid saved target account") from exc
        if (layout != (plan["target_home"], plan.get("target_transcript"))
                or plan.get("target_provider") != provider_name("claude", plan["target_home"])):
            raise HandoffError("Target account paths changed since preparation")
    try:
        source, caller = Process(**plan["source"]), Process(**plan["caller"])
        owner = Process(**plan["owner"]) if "owner" in plan else source
        ancestors = [Process(**p) for p in plan.get("source_ancestors", [])]
    except (KeyError, TypeError) as exc:
        raise HandoffError("Invalid saved process identity") from exc
    for process in (source, caller, owner, *ancestors):
        if any(type(n) is not int for n in (process.pid, process.ppid, process.uid)) or process.pid <= 1:
            raise HandoffError("Invalid saved process identity")
        if not isinstance(process.started, str) or not isinstance(process.command, str) or process.uid != os.getuid():
            raise HandoffError("Invalid saved process identity")
    source_allowed(source, plan["agent"])
    owner_allowed(owner, plan["agent"])
    if "owner" in plan:
        chain = [source, *ancestors, owner]
        if len({p.pid for p in chain}) != len(chain) or any(
                child.ppid != parent.pid for child, parent in zip(chain, chain[1:])):
            raise HandoffError("Invalid saved source-to-owner ancestry")
    elif ancestors:
        raise HandoffError("Saved source ancestry has no owner")
    if caller.pid in {source.pid, owner.pid}:
        raise HandoffError("Source and caller must be distinct")
    return source, caller


def config_snapshot(filename):
    """Read private config bytes and identity without exposing their contents."""
    try:
        entry = Path(filename).lstat()
        if not stat.S_ISREG(entry.st_mode) or entry.st_uid != os.getuid():
            raise HandoffError("Local Paseo config must be an owned regular file without symlinks")
        fd = os.open(filename, os.O_RDONLY | getattr(os, "O_NOFOLLOW", 0) | os.O_NONBLOCK)
    except FileNotFoundError:
        return None
    except OSError as exc:
        raise HandoffError("Cannot safely open local Paseo configuration") from exc
    with os.fdopen(fd, "rb") as handle:
        info = os.fstat(handle.fileno())
        if not stat.S_ISREG(info.st_mode) or info.st_uid != os.getuid():
            raise HandoffError("Local Paseo config must be an owned regular file")
        raw = handle.read()
        after = os.fstat(handle.fileno())
        signature = lambda value: (value.st_dev, value.st_ino, value.st_uid, value.st_mode,
                                   value.st_size, value.st_mtime_ns, value.st_ctime_ns)
        if signature(info) != signature(after):
            raise HandoffError("Local Paseo configuration changed while reading")
        return signature(info), raw


def merge_provider_alias(home, provider, alias):
    """Atomically add one alias, preserving every existing private config key."""
    directory = Path(home)
    info = directory.lstat()
    if not stat.S_ISDIR(info.st_mode) or info.st_uid != os.getuid() or info.st_mode & 0o022:
        raise HandoffError("Local Paseo home must be an owned directory without public write access")
    filename = directory / "config.json"
    with worker_lock(directory, "handoff-config.lock"):
        original = config_snapshot(filename)
        try:
            config = json.loads(original[1].decode("utf-8-sig")) if original is not None else {}
        except (ValueError, UnicodeError) as exc:
            raise HandoffError("Local Paseo configuration is invalid JSON; no changes made") from exc
        if not isinstance(config, dict):
            raise HandoffError("Local Paseo configuration must be a JSON object")
        agents = config.setdefault("agents", {})
        if not isinstance(agents, dict):
            raise HandoffError("Local Paseo agents configuration must be a JSON object")
        providers = agents.setdefault("providers", {})
        if not isinstance(providers, dict):
            raise HandoffError("Local Paseo providers configuration must be a JSON object")
        if provider in providers:
            existing = providers[provider]
            if not isinstance(existing, dict) or {k: v for k, v in existing.items() if k != "label"} != {k: v for k, v in alias.items() if k != "label"}:
                raise HandoffError("Existing Paseo handoff alias conflicts with the requested profile")
            return False
        providers[provider] = alias
        temporary = None
        try:
            fd, temporary = tempfile.mkstemp(prefix=".handoff-config-", suffix=".json", dir=directory)
            with os.fdopen(fd, "w", encoding="utf-8") as handle:
                os.fchmod(handle.fileno(), 0o600)
                json.dump(config, handle, indent=2, ensure_ascii=False)
                handle.write("\n")
                handle.flush()
                os.fsync(handle.fileno())
            # The lock coordinates handoff workers. This second snapshot also
            # catches edits by the daemon or another tool that ignores it.
            if config_snapshot(filename) != original:
                raise HandoffError("Local Paseo configuration changed; refusing to replace it")
            os.replace(temporary, filename)
            temporary = None
            directory_fd = os.open(directory, os.O_RDONLY)
            try:
                os.fsync(directory_fd)
            finally:
                os.close(directory_fd)
        finally:
            if temporary is not None:
                os.unlink(temporary)
    return True


def provider_preflight(plan):
    paseo, home, agent = plan["paseo"], plan["paseo_home"], plan["agent"]
    # With --to, the provider is the target account's; otherwise the source's own.
    config_dir = import_home(plan)
    command([paseo, "daemon", "start", "--home", home, "--timeout", "45", "--json"], timeout=60)
    provider = provider_name(agent, config_dir)
    if provider != agent:
        alias = {"extends": agent, "label": "Imported " + agent.capitalize() + " profile",
                 "env": {"CLAUDE_CONFIG_DIR" if agent == "claude" else "CODEX_HOME": config_dir}}
        merge_provider_alias(home, provider, alias)
        command([paseo, "daemon", "reload", "--home", home, "--json"])
    value = json_command([paseo, "provider", "diagnostic", provider, "--home", home, "--json"])
    diagnostic = value.get("diagnostic")
    if value.get("provider") != provider or not isinstance(diagnostic, str) or not re.search(r"^\s*Status: Ready\s*$", diagnostic, re.M):
        raise HandoffError("Paseo provider is not ready; native source preserved")
    if agent == "claude":
        auth_match = re.search(r"^\s*Auth:\s*", diagnostic, re.M)
        try:
            if not auth_match:
                raise ValueError()
            auth, _ = json.JSONDecoder().raw_decode(diagnostic[auth_match.end():])
            config_home = absolute_path(auth.get("configDirectory"), kind="dir")
        except (ValueError, AttributeError, HandoffError) as exc:
            raise HandoffError("Cannot verify Claude authentication profile; native source preserved") from exc
        if config_home != config_dir:
            raise HandoffError("Paseo Claude profile differs from the requested config directory; "
                               "native source preserved")
    return provider


def same_process(expected, actual):
    return actual is not None and (expected.pid, expected.uid, expected.started, expected.command) == (
        actual.pid, actual.uid, actual.started, actual.command)


def verify_process_identities(source, owner):
    table = process_table()
    for label, expected in (("Source", source), ("Source owner", owner)):
        if not same_process(expected, table.get(expected.pid)):
            raise HandoffError(label + " process identity changed; refusing termination")


def wait_gone(process, seconds):
    deadline = time.monotonic() + seconds
    while same_process(process, process_table().get(process.pid)):
        if time.monotonic() >= deadline:
            raise HandoffError("Timed out waiting for process exit; no session imported")
        time.sleep(0.2)


def live_rows(plan):
    output = command([plan["agent_session"], plan["agent"], "live", store_root(plan)])
    rows = []
    for line in output.splitlines():
        fields = line.split("\t")
        if len(fields) != 8 or not fields[0].isdigit() or int(fields[0]) <= 1:
            raise HandoffError("Cannot safely parse native live-session mapping")
        # Name and status are private and irrelevant to the decision.
        rows.append((int(fields[0]), fields[1], fields[4], fields[7]))
    return rows


def verify_live_source(plan, source, rows):
    owned = [row for row in rows if row[0] == source.pid]
    if len(owned) != 1 or owned[0][1:3] != (plan["id"], plan["transcript"]):
        raise HandoffError("Source PID does not uniquely own the selected native transcript")
    if owned[0][3] == "background":
        raise HandoffError("Background native sessions require manual exit")
    if any(row[2] == plan["transcript"] and row[0] != source.pid for row in rows):
        raise HandoffError("Another native process owns the selected transcript")


def ensure_no_writer(plan, seconds=5):
    deadline = time.monotonic() + seconds
    while any(row[2] == plan["transcript"] for row in live_rows(plan)):
        # Native liveness can briefly retain an exited, unreaped process. Give
        # that record time to clear, while refusing every persistent writer.
        if time.monotonic() >= deadline:
            raise HandoffError("Native transcript still has a live writer; no session imported")
        time.sleep(0.2)


def file_digest(filename):
    digest = hashlib.sha256()
    with open(filename, "rb") as handle:
        while chunk := handle.read(1 << 20):
            digest.update(chunk)
    return digest.hexdigest()


def copy_file(source, destination):
    """Copy one regular file byte for byte into a new private file.

    The destination is created exclusively with mode 0600, and keeps the
    source's times, so Claude's session lists still order it by its last
    turn. A destination left by a failed copy is removed. Returns its size.
    """
    flags = os.O_RDONLY | getattr(os, "O_NOFOLLOW", 0)
    with open(os.open(source, flags), "rb") as reader:
        before = os.fstat(reader.fileno())
        if not stat.S_ISREG(before.st_mode):
            raise HandoffError("Not a regular file: " + source)
        fd = os.open(destination, os.O_WRONLY | os.O_CREAT | os.O_EXCL | getattr(os, "O_NOFOLLOW", 0), 0o600)
        try:
            size = 0
            with os.fdopen(fd, "wb") as writer:
                os.fchmod(writer.fileno(), 0o600)
                while chunk := reader.read(1 << 20):
                    writer.write(chunk)
                    size += len(chunk)
                writer.flush()
                os.fsync(writer.fileno())
            after = os.fstat(reader.fileno())
            if size != after.st_size or (before.st_size, before.st_mtime_ns) != (after.st_size, after.st_mtime_ns):
                raise HandoffError("Source changed while copying: " + source)
            os.utime(destination, ns=(before.st_atime_ns, before.st_mtime_ns), follow_symlinks=False)
        except BaseException:
            try:
                os.unlink(destination)
            except OSError:
                warn("cannot remove the partial copy " + destination)
            raise
    return size


def raise_walk_error(exc):
    raise exc


def copy_tree(source, destination):
    """Copy a sidecar directory into a new, empty private directory.

    Returns what was copied. Directories get mode 0700 and files 0600.
    Anything that is neither a regular file nor a directory, such as a
    symbolic link, is skipped and counted rather than followed.
    """
    totals = {"files": 0, "directories": 0, "bytes": 0, "skipped": 0}
    for root, directories, files in os.walk(source, onerror=raise_walk_error):
        target = Path(destination) / Path(root).relative_to(source)
        kept = []
        for name in directories:
            if Path(root, name).is_symlink():
                totals["skipped"] += 1
                continue
            os.mkdir(target / name, 0o700)
            os.chmod(target / name, 0o700)
            totals["directories"] += 1
            kept.append(name)
        directories[:] = kept
        for name in files:
            if not stat.S_ISREG(Path(root, name).lstat().st_mode):
                totals["skipped"] += 1
                continue
            totals["bytes"] += copy_file(str(Path(root, name)), str(target / name))
            totals["files"] += 1
    return totals


def copy_session(plan, recordfile):
    """Copy the final transcript and its sidecar into the target account.

    Called only after the source has exited and no writer remains. It never
    overwrites, never touches the source, and removes what it created when it
    fails. The record it writes keeps a re-run from copying again.
    """
    source, destination = plan["transcript"], plan["target_transcript"]
    source_side = sidecar_path(source, plan["id"])
    destination_side = sidecar_path(destination, plan["id"])
    project = Path(destination).parent
    created = []
    try:
        check_destination(plan)
        if not os.path.lexists(project):
            os.mkdir(project, 0o700)
            created.append(("directory", project))
        info = project.lstat()
        if not stat.S_ISDIR(info.st_mode) or info.st_uid != os.getuid():
            raise HandoffError("Destination project folder must be your own directory: " + str(project))
        size = copy_file(source, destination)
        created.append(("file", destination))
        digest = file_digest(source)
        if file_digest(destination) != digest:
            raise HandoffError("Copied transcript differs from its source")
        sidecar = None
        if os.path.lexists(source_side):
            if not Path(source_side).is_dir() or Path(source_side).is_symlink():
                raise HandoffError("Session sidecar is not a directory: " + source_side)
            os.mkdir(destination_side, 0o700)  # Exclusive, so the cleanup only ever removes our own tree.
            created.append(("tree", destination_side))
            os.chmod(destination_side, 0o700)
            sidecar = copy_tree(source_side, destination_side)
            sidecar.update(source=source_side, destination=destination_side)
        record = {"planDigest": fingerprint(plan), "sourceId": plan["id"], "source": source,
                  "destination": destination, "bytes": size, "sha256": digest, "sidecar": sidecar}
        write_private(recordfile, record)
    except BaseException as exc:  # Including Ctrl+C in the worker's pane: never leave a partial copy.
        for kind, path in reversed(created):
            try:
                if kind == "tree":
                    shutil.rmtree(path)
                elif kind == "file":
                    os.unlink(path)
                else:
                    os.rmdir(path)
            except OSError:
                warn("cannot remove the partial copy " + str(path))
        if not isinstance(exc, Exception):
            raise
        raise HandoffError("Cannot copy the session from " + source + " to " + destination + ": "
                           + failure_text(exc) + ". Nothing was imported, and the original transcript is "
                           "untouched.") from exc
    print("Copied the session to " + destination + (
        "" if sidecar is None else " with " + str(sidecar["files"]) + " sidecar files"), flush=True)
    return record


def read_copy_record(plan, recordfile):
    record = read_private(recordfile)
    if (not isinstance(record, dict) or record.get("planDigest") != fingerprint(plan)
            or record.get("sourceId") != plan["id"] or record.get("destination") != plan["target_transcript"]
            or not isinstance(record.get("sha256"), str)):
        raise HandoffError("Saved copy record belongs to another handoff")
    return record


def verify_copy(plan, record):
    """Check that neither transcript changed since an earlier worker copied it."""
    for label, path in (("original", plan["transcript"]), ("copied", plan["target_transcript"])):
        try:
            unchanged = file_digest(path) == record["sha256"]
        except OSError as exc:
            raise HandoffError("Cannot read the " + label + " transcript " + path) from exc
        if not unchanged:
            raise HandoffError("The " + label + " transcript changed since the copy; inspect " + path
                               + " before retrying")


def fingerprint(plan):
    return hashlib.sha256(json.dumps(plan, sort_keys=True, separators=(",", ":")).encode()).hexdigest()


@contextmanager
def worker_lock(directory, name="worker.lock"):
    lockfile = directory / name
    fd = os.open(lockfile, os.O_RDWR | os.O_CREAT | getattr(os, "O_NOFOLLOW", 0), 0o600)
    try:
        info = os.fstat(fd)
        if not stat.S_ISREG(info.st_mode) or info.st_uid != os.getuid() or info.st_mode & 0o077:
            raise HandoffError("Unsafe worker lock")
        try:
            fcntl.flock(fd, fcntl.LOCK_EX | fcntl.LOCK_NB)
        except BlockingIOError as exc:
            raise HandoffError("Another worker is already running this handoff") from exc
        yield
    finally:
        os.close(fd)


def attach(plan, agent_id):
    for marker in MARKERS:
        os.environ.pop(marker, None)
    os.execv(plan["paseo"], ["paseo", "attach", agent_id, "--home", plan["paseo_home"]])


def recovery_command(plan):
    agent = plan["agent"]
    valid_uuid(plan["id"])
    if agent not in {"claude", "codex"}:
        raise HandoffError("Invalid recovery provider")
    home = absolute_path(plan["provider_home"], kind="dir")
    cwd = absolute_path(plan["cwd"], kind="dir")
    argv = ["env"]
    for marker in sorted(MARKERS):
        argv.extend(["-u", marker])
    if agent == "codex":
        argv.append("CODEX_HOME=" + home)
    elif home != default_claude_home():
        argv.append("CLAUDE_CONFIG_DIR=" + home)
    argv.extend([agent, "--resume" if agent == "claude" else "resume", plan["id"]])
    return "cd " + shlex.quote(cwd) + " && " + shlex.join(argv)


def model_helper_argv(plan):
    """Run the model helper with the Node.js the Paseo pnpm shim itself prefers."""
    sibling = Path(plan["paseo"]).parent / "node"
    if sibling.is_file() and os.access(sibling, os.X_OK):
        node = str(sibling)
    else:
        node = shutil.which("node")
        if node is None:
            raise HandoffError("Node.js is unavailable for the model update")
    return [node, str(MODEL_HELPER)]


def set_model(plan, agent_id, model):
    command(model_helper_argv(plan) + ["--paseo", plan["paseo"], "--home", plan["paseo_home"],
                                       "--agent-id", agent_id, "--model", model], timeout=45)


def set_effort(plan, agent_id, effort):
    command([plan["paseo"], "agent", "update", agent_id, "--thinking", effort,
             "--home", plan["paseo_home"]], timeout=45)


def failure_text(exc):
    # HandoffError texts are generic on purpose: CLI output stays withheld.
    return str(exc) if isinstance(exc, HandoffError) else type(exc).__name__ + ": " + str(exc)


def attempt(value, step, *, skipped=None):
    if value is None or skipped:
        return {"value": value, "status": "skipped",
                "detail": skipped or "unknown in the source transcript"}
    try:
        step(value)
    except Exception as exc:  # Best effort: record the failure and continue.
        return {"value": value, "status": "failed", "detail": failure_text(exc)}
    return {"value": value, "status": "ok", "detail": None}


def outcome_text(step):
    if step["status"] == "ok":
        return "succeeded"
    return ("failed" if step["status"] == "failed" else "was skipped") + " (" + step["detail"] + ")"


def followup_prompt(plan, agent_id, record):
    source = "Claude Code" if plan["agent"] == "claude" else "Codex"
    model = plan.get("model") or "unknown"
    if plan.get("model_source") == "reply":
        model += " (read from your last reply, which omits a context suffix such as [1m])"
    effort = plan.get("effort") or "unknown"
    account = []
    if plan.get("target_home"):
        account = [
            "You also moved accounts: 2paseo copied your transcript from the Claude config directory "
            + plan["provider_home"] + " to " + plan["target_home"] + ", and Paseo now runs you under "
            + plan["target_home"] + ". The original transcript stays untouched at " + plan["transcript"]
            + ". Memory directories, account-level instructions (for example a profile overlay CLAUDE.md), "
            "connectors and quotas differ between the two config directories. Re-read your project "
            "instructions, and check that the tools and skills you rely on are present.",
        ]
    return "\n\n".join([
        "You are now running in Paseo as agent " + agent_id + ". 2paseo imported you from the native "
        + source + " session " + plan["id"] + " (working directory " + plan["cwd"] + ") and stopped "
        "the original process.",
        *account,
        "Continue the task you were doing before the move. First, check what the move broke and repair it:",
        "\n".join([
            "- Scheduled tasks and cron jobs created in the old session died with it. Recreate the ones "
            "you still need as Paseo schedules or heartbeats, with the `paseo` skill.",
            "- In-process subagents, background tasks and background shells are gone. Check their "
            "outputs, results and worktrees, and relaunch whatever was unfinished.",
            "- Resume stopped workflows, and re-arm monitors.",
            "- Holds the old process owned ended with it, and its screen banner may still be up. Take "
            "again the holds you still need, and turn off a banner you no longer need.",
        ]),
        "Then confirm that your model and effort match the source session (model " + model + ", effort "
        + effort + ") with the `paseo-model-update` skill in status-only mode, and fix them with "
        "update_agent if they differ. An unknown value could not be read; keep the current one. When "
        "2paseo tried, setting the model " + outcome_text(record["model"]) + ", and setting the effort "
        + outcome_text(record["effort"]) + ".",
    ]) + "\n"


def follow_up(plan, status_dir, agent_id):
    """Carry the source model and effort over, then brief the agent, once.

    Model first: Paseo checks an effort against the agent's current model.
    Every step is best effort; a failure is recorded in followup.json and in
    the briefing, and never fails or rolls back the handoff. The record is
    written last, so a worker killed midway repeats the follow-up on its next
    run: the setters are idempotent, and a second briefing is harmless.
    """
    if "model" not in plan:
        return None  # Prepared before the follow-up existed; its agent moved on.
    recordfile = status_dir / "followup.json"
    if os.path.lexists(recordfile):
        return None
    record = {"planDigest": fingerprint(plan), "sourceId": plan["id"], "agentId": agent_id}
    record["model"] = attempt(plan.get("model"), lambda model: set_model(plan, agent_id, model))
    # No provider takes "auto" as a thinking option; it means the default.
    auto = "auto keeps the provider default" if plan.get("effort") == "auto" else None
    record["effort"] = attempt(plan.get("effort"), lambda effort: set_effort(plan, agent_id, effort),
                               skipped=auto)
    promptfile = status_dir / "followup-prompt.txt"
    try:
        if os.path.lexists(promptfile):
            os.unlink(promptfile)  # Left by an interrupted earlier run.
        write_private_text(promptfile, followup_prompt(plan, agent_id, record))
        command([plan["paseo"], "send", "--no-wait", "--home", plan["paseo_home"],
                 "--prompt-file", str(promptfile), agent_id], timeout=60)
        record["message"] = {"status": "ok", "detail": None, "promptFile": str(promptfile)}
    except Exception as exc:  # Best effort: record the failure and continue.
        record["message"] = {"status": "failed", "detail": failure_text(exc), "promptFile": str(promptfile)}
    try:
        write_private(recordfile, record)
    except (HandoffError, OSError) as exc:
        warn("cannot record the follow-up: " + failure_text(exc))
    print("Paseo follow-up: model " + record["model"]["status"] + ", effort " + record["effort"]["status"]
          + ", briefing " + record["message"]["status"] + "; details in " + str(recordfile), flush=True)
    return record


def run(filename):
    if Path(filename).is_symlink():
        raise HandoffError("Handoff plan must not be a symbolic link")
    filename = absolute_path(filename, kind="file")
    directory = Path(filename).parent
    private_directory(directory)
    plan = read_private(filename)
    source, caller = validate_plan(plan)
    owner = Process(**plan["owner"]) if "owner" in plan else source
    with worker_lock(directory):
        status_dir = directory / "status"
        status_dir.mkdir(mode=0o700, exist_ok=True)
        private_directory(status_dir)
        resultfile = status_dir / "result.json"
        if resultfile.exists():
            result = read_private(resultfile)
            if not isinstance(result, dict) or result.get("planDigest") != fingerprint(plan) or result.get("sourceId") != plan["id"]:
                raise HandoffError("Saved result belongs to another handoff")
            valid_uuid(result.get("agentId"))
            # The imported provider may now write the original native
            # transcript. This branch only attaches to an already saved ID,
            # after finishing a follow-up an earlier worker did not record.
            follow_up(plan, status_dir, result["agentId"])
            attach(plan, result["agentId"])
            return
        importfile = status_dir / "import-request.json"
        if importfile.exists():
            raise HandoffError("An earlier import may have succeeded; inspect Paseo before retrying")
        verify_native(plan)
        moving = "target_home" in plan
        copyfile = status_dir / "copy.json"
        copied = read_copy_record(plan, copyfile) if moving and os.path.lexists(copyfile) else None
        if moving and copied is None:
            check_destination(plan)
        provider = provider_preflight(plan)
        if moving and provider != plan["target_provider"]:
            raise HandoffError("Target Paseo provider changed since preparation; native source preserved")
        if copied is None:
            wait_gone(caller, 30)
            if plan["wait_exit"]:
                wait_gone(source, 600)
            else:
                verify_process_identities(source, owner)
                verify_live_source(plan, owner, live_rows(plan))
                # Check identity after the live scan too, immediately before signal.
                verify_process_identities(source, owner)
                print("Terminating native source PID " + str(source.pid), flush=True)
                try:
                    os.kill(source.pid, signal.SIGTERM)
                except ProcessLookupError:
                    pass
                wait_gone(source, 30)
            ensure_no_writer(plan)
            if moving:
                # The source has exited and no writer remains: this is the final transcript.
                copied = copy_session(plan, copyfile)
        else:
            # An earlier worker stopped the source, found no writer and copied
            # it. Never signal again; check that nothing changed since.
            ensure_no_writer(plan)
            verify_copy(plan, copied)
        # Import is an external mutation. An interrupted or invalid response
        # cannot prove it failed, so leave a durable marker and refuse replay.
        write_private(importfile, {"planDigest": fingerprint(plan), "sourceId": plan["id"],
                                   "provider": provider, "cwd": plan["cwd"],
                                   "transcript": plan.get("target_transcript", plan["transcript"])})
        value = json_command([plan["paseo"], "import", plan["id"], "--provider", provider,
                              "--cwd", plan["cwd"], "--home", plan["paseo_home"], "--json"])
        agent_id = valid_uuid(value.get("agentId"))
        if value.get("provider") != provider or value.get("cwd") != plan["cwd"]:
            raise HandoffError("Imported session provider or cwd does not match the source")
        write_private(resultfile, {"planDigest": fingerprint(plan), "agentId": agent_id,
                                   "sourceId": plan["id"], "provider": provider, "cwd": plan["cwd"]})
        follow_up(plan, status_dir, agent_id)
        attach(plan, agent_id)


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    subparsers = parser.add_subparsers(dest="mode", required=True)
    preparing = subparsers.add_parser("prepare")
    preparing.add_argument("--agent", choices=("claude", "codex"), required=True)
    for name in ("id", "transcript", "cwd", "provider-home", "paseo", "agent-session",
                 "paseo-home", "tmux-session", "output"):
        preparing.add_argument("--" + name, required=True)
    for name in ("source-pid", "caller-pid"):
        preparing.add_argument("--" + name, required=True, type=int)
    preparing.add_argument("--wait-exit", action="store_true")
    preparing.add_argument("--target-home", help="move a Claude session to this config directory")
    running = subparsers.add_parser("run")
    running.add_argument("plan")
    args = parser.parse_args(argv)
    try:
        if args.mode == "prepare":
            print(prepare(args))
        else:
            run(args.plan)
        return 0
    except (HandoffError, OSError) as exc:
        print("paseo-handoff: " + str(exc), file=sys.stderr)
        if args.mode == "run":
            try:
                plan = read_private(args.plan)
                print("Native recovery (after the original session has exited and Paseo is not using it): "
                      + recovery_command(plan), file=sys.stderr)
                if "target_transcript" in plan:
                    print("That resumes the untouched original in its own account. A completed copy for "
                          + plan["target_home"] + " stays at " + plan["target_transcript"]
                          + " (status/copy.json records it).", file=sys.stderr)
            except (HandoffError, KeyError, TypeError):
                pass
        return 1


if __name__ == "__main__":
    sys.exit(main())
