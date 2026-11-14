#!/usr/bin/env python3
"""Opt-in quota recovery for local Paseo agents, using existing quota readers.

Persist a private registration, watch only that agent's quota-failed turns, and
send one continuation after its own provider account reports usable quota.
"""
import argparse
from contextlib import contextmanager
import fcntl
import hashlib
import json
import os
from pathlib import Path
import re
import stat
import subprocess
import sys
import tempfile
import time
import uuid

from paseo_handoff import command, clean_env, HandoffError

ROOT = Path(__file__).resolve().parent
QUOTA_ERROR = re.compile(
    r"usage[_ -]limit|rate[_ -]limit|quota (?:exceeded|exhausted)|"
    r"(?:you(?:'ve| have)?|account) (?:hit|reached|exceeded) .*limit|"
    r"(?:hit|reached) your limit|out of .*usage|too many requests|quota reached", re.I)


def digest(value):
    return hashlib.sha256(json.dumps(value, sort_keys=True).encode()).hexdigest()


def read_json(path):
    value = json.loads(Path(path).read_text())
    if not isinstance(value, dict):
        raise HandoffError("Expected a JSON object")
    return value


def private_dir(path):
    path = Path(path)
    path.mkdir(parents=True, exist_ok=True, mode=0o700)
    info = path.lstat()
    if not stat.S_ISDIR(info.st_mode) or info.st_uid != os.getuid() or info.st_mode & 0o077:
        raise HandoffError("Watcher directory must be an owned private directory")
    return path


def atomic_write(path, value):
    path = Path(path)
    if path.exists() or path.is_symlink():
        info = path.lstat()
        if not stat.S_ISREG(info.st_mode) or info.st_uid != os.getuid():
            raise HandoffError("Refusing unsafe registration file")
    fd, name = tempfile.mkstemp(dir=path.parent, prefix='.write-')
    try:
        with os.fdopen(fd, 'w') as stream:
            json.dump(value, stream, indent=2)
            stream.write('\n')
            stream.flush()
            os.fsync(stream.fileno())
        os.replace(name, path)
    finally:
        if os.path.exists(name):
            os.unlink(name)


@contextmanager
def locked(path):
    fd = os.open(path, os.O_CREAT | os.O_RDWR | os.O_NOFOLLOW, 0o600)
    try:
        info = os.fstat(fd)
        if not stat.S_ISREG(info.st_mode) or info.st_uid != os.getuid():
            raise HandoffError("Refusing unsafe lock file")
        fcntl.flock(fd, fcntl.LOCK_EX)
        yield
    finally:
        os.close(fd)


def inspect(agent_id, home):
    return json.loads(command(['paseo', 'inspect', agent_id, '--home', str(home), '--json']))


def stored_agent(agent_id, home):
    matches = list((home / 'agents').glob(agent_id + '.json'))
    matches += list((home / 'agents').glob('*/' + agent_id + '.json'))
    if len(matches) != 1:
        raise HandoffError("Cannot find one local persisted Paseo agent")
    record = read_json(matches[0])
    if record.get('id') != agent_id:
        raise HandoffError("Stored agent ID does not match")
    return record


def provider_config(home, provider):
    config_file = home / 'config.json'
    providers = read_json(config_file).get('agents', {}).get('providers', {}) if config_file.exists() else {}
    result, seen = {}, set()
    current = provider
    while current not in ('claude', 'codex'):
        if current in seen or not isinstance(providers.get(current), dict):
            raise HandoffError("Provider must resolve to Claude or Codex")
        seen.add(current)
        entry = providers[current]
        # Labels do not change the quota identity; launch settings do.
        own = {k: v for k, v in entry.items() if k not in ('label', 'extends', 'env')}
        result = {**own, **result, 'env': {**entry.get('env', {}), **result.get('env', {})}}
        current = entry.get('extends')
    base = providers.get(current, {})
    if not isinstance(base, dict):
        raise HandoffError("Invalid base provider configuration")
    result = {**{k: v for k, v in base.items() if k not in ('label', 'env')}, **result,
              'env': {**base.get('env', {}), **result.get('env', {})}}
    key = 'CLAUDE_CONFIG_DIR' if current == 'claude' else 'CODEX_HOME'
    native_home = Path(result['env'].get(key) or Path.home() / ('.claude' if current == 'claude' else '.codex')).expanduser().resolve()
    if current == 'claude':
        known = {Path.home().joinpath('.claude').resolve(): 'default',
                 Path.home().joinpath('.claude-work').resolve(): 'work'}
        profile = known.get(native_home)
        if profile is None:
            raise HandoffError("Claude quota watcher currently supports default and work profiles")
    else:
        profile = 'codex'
    return {'agent': current, 'profile': profile, 'native_home': str(native_home),
            'config_hash': digest(result)}, result.get('env', {})


def binding(snapshot, home):
    identity, _ = provider_config(home, snapshot['Provider'])
    return {**identity, 'provider': snapshot['Provider'], 'cwd': snapshot['Cwd']}


def quota_error(record):
    """Whether the record's last error is a usage limit (Codex reports those this way)."""
    error = record.get('lastError')
    return isinstance(error, str) and QUOTA_ERROR.search(error) is not None


def quota_failure(record):
    """Account exhaustion elsewhere must not resume an intentionally idle agent."""
    if record.get('archivedAt') or record.get('lastStatus') != 'error':
        return None
    if not quota_error(record):
        return None
    error = record['lastError']
    # lastActivityAt can move for incidental events. The failed turn identity
    # instead uses its user message and attention timestamp, plus error text.
    return digest([error, record.get('lastUserMessageAt'), record.get('attentionTimestamp')])


def claude_limit_failure(record, binding, tail_bytes=1 << 20):
    """A Claude limit stop that Paseo records as an ordinary finished turn.

    Claude Code ends such a turn with a synthetic assistant message flagged
    isApiErrorMessage and error "rate_limit", so Paseo shows the agent idle
    with no lastError. Read the end of the session transcript instead: the
    stop counts only while that message is still the last conversation entry.
    """
    persistence = record.get('persistence') or {}
    try:
        session = str(uuid.UUID(str(persistence.get('sessionId'))))
    except ValueError:
        return None
    matches = list((Path(binding['native_home']) / 'projects').glob('*/' + session + '.jsonl'))
    if len(matches) != 1:
        return None
    with open(matches[0], 'rb') as stream:
        stream.seek(0, os.SEEK_END)
        stream.seek(max(0, stream.tell() - tail_bytes))
        lines = stream.read().splitlines()
    for raw in reversed(lines):
        try:
            entry = json.loads(raw)
        except ValueError:
            continue  # the first line of the tail is usually cut in half
        if not isinstance(entry, dict) or entry.get('type') not in ('user', 'assistant'):
            continue  # queue, mode and summary bookkeeping is not a turn
        if entry['type'] != 'assistant' or entry.get('isApiErrorMessage') is not True:
            return None
        content = (entry.get('message') or {}).get('content')
        text = ' '.join(part.get('text', '') for part in content if isinstance(part, dict)) \
            if isinstance(content, list) else str(content or '')
        if entry.get('error') != 'rate_limit' and not QUOTA_ERROR.search(text):
            return None
        return digest(['claude-limit', session, entry.get('uuid'), text])
    return None


def codex_rollout(native_home, thread_id):
    """The Codex session log of a thread, found through the creation time in its id.

    Codex thread ids are UUIDv7, so the first 48 bits are the creation time in
    milliseconds, and Codex files the log under sessions/YYYY/MM/DD of that
    (local) day. The neighbouring days cover a timezone edge; the full search is
    the last resort.
    """
    sessions = Path(native_home) / 'sessions'
    name = 'rollout-*-' + thread_id + '.jsonl'
    try:
        created = int(thread_id.replace('-', '')[:12], 16) / 1000
        days = [time.localtime(created + offset * 86400) for offset in (0, -1, 1)]
    except (ValueError, OverflowError, OSError):
        days = []
    for day in days:
        matches = list(sessions.glob(time.strftime('%Y/%m/%d/', day) + name))
        if matches:
            return matches[0]
    matches = list(sessions.glob('*/*/*/' + name))
    return matches[0] if matches else None


def codex_limit_failure(record, binding, tail_bytes=1 << 20):
    """A Codex limit stop that Paseo records as an ordinary finished turn.

    Codex ends the turn with a task_complete event whose error says the usage
    limit was hit, and Paseo shows the agent idle with no lastError. The stop
    counts only while that is the last root turn in the session log.
    """
    persistence = record.get('persistence') or {}
    thread = (persistence.get('metadata') or {}).get('threadId') or persistence.get('sessionId')
    try:
        thread = str(uuid.UUID(str(thread)))
    except ValueError:
        return None
    path = codex_rollout(binding['native_home'], thread)
    if path is None:
        return None
    with open(path, 'rb') as stream:
        stream.seek(0, os.SEEK_END)
        stream.seek(max(0, stream.tell() - tail_bytes))
        lines = stream.read().splitlines()
    for raw in reversed(lines):
        try:
            entry = json.loads(raw)
        except ValueError:
            continue
        payload = entry.get('payload') if isinstance(entry, dict) else None
        if entry.get('type') != 'event_msg' or not isinstance(payload, dict):
            continue
        if payload.get('type') not in ('task_started', 'task_complete'):
            continue
        root = payload.get('root_turn_id')
        if root and payload.get('turn_id') != root:
            continue  # a review or compaction sub-turn, not the agent's own turn
        error = payload.get('error') if payload['type'] == 'task_complete' else None
        message = error.get('message') if isinstance(error, dict) else None
        if not isinstance(message, str) or not QUOTA_ERROR.search(message):
            return None
        return digest(['codex-limit', thread, payload.get('turn_id'), message])
    return None


def limit_failure(record, binding):
    """A limit stop recorded only in the provider's own transcript, if any."""
    if binding.get('agent') == 'claude':
        return claude_limit_failure(record, binding)
    if binding.get('agent') == 'codex':
        return codex_limit_failure(record, binding)
    return None


def failure_of(snapshot, record, state, transcript_fn):
    """The current failed turn's identity, or None when nothing should resume."""
    if snapshot.get('Status') == 'error':
        return quota_failure(record)
    if snapshot.get('Status') == 'idle' and state['binding']['agent'] in ('claude', 'codex') \
            and not record.get('archivedAt'):
        return transcript_fn(record, state['binding'])
    return None


def stopped_agents(home, transcript_fn=limit_failure):
    """The local agents whose last turn stopped at a usage limit, registered or not.

    The same tests the watcher applies to one agent, read from the stored records
    alone so a scan asks the daemon nothing. A stored record says `closed' for an
    agent the daemon has not loaded since it restarted, so that counts as well
    as the live statuses.
    """
    records = list((home / 'agents').glob('*.json')) + list((home / 'agents').glob('*/*.json'))
    identities = {}
    for path in sorted(records):
        try:
            record = read_json(path)
        except (OSError, ValueError):
            continue
        if not isinstance(record, dict) or record.get('archivedAt') or record.get('internal'):
            continue
        status = record.get('lastStatus')
        if status in ('error', 'closed') and quota_error(record):
            yield record['id']
            continue
        if status not in ('idle', 'closed'):
            continue
        provider = record.get('provider')
        if provider not in identities:
            try:
                identities[provider] = provider_config(home, provider)[0]
            except (HandoffError, OSError, ValueError, KeyError):
                identities[provider] = None
        identity = identities[provider]
        if identity and identity['agent'] in ('claude', 'codex') and transcript_fn(record, identity):
            yield record['id']


def agy_account(home, provider):
    """The Antigravity account an agent of this provider runs on, or None if not agy.

    The built-in `antigravity` provider and any provider whose command is the agy
    binary (the ACP driver) count. The account is AGY_ACCOUNT_ID from the
    provider's environment, else the main account `m`, as agy itself defaults.
    """
    config_file = home / 'config.json'
    providers = read_json(config_file).get('agents', {}).get('providers', {}) if config_file.exists() else {}
    env, command_name, current, seen = {}, None, provider, set()
    while isinstance(providers.get(current), dict) and current not in seen:
        seen.add(current)
        entry = providers[current]
        env = {**entry.get('env', {}), **env}
        argv = entry.get('command')
        if command_name is None and isinstance(argv, list) and argv:
            command_name = Path(str(argv[0])).name
        current = entry.get('extends')
    if current != 'antigravity' and provider != 'antigravity' and command_name != 'agy':
        return None
    account = str(env.get('AGY_ACCOUNT_ID') or 'm')
    return 'm' if account == 'acc1' else account


def agy_quota_report():
    """Every Antigravity account's cached quota, from agy-status, keyed by account.

    Cache-only: it never starts agy, asks for a login or contacts a backend, so a
    missing or stale cache is an error for that account, not a hang.
    """
    argv = ['zsh', '-ic', 'agy_status_method=statusline agy_status_cache_only_p=y agy-status --json']
    result = subprocess.run(argv, capture_output=True, text=True, timeout=90,
                            stdin=subprocess.DEVNULL, cwd=Path.home())
    accounts = json.loads(result.stdout)  # exit 1 only means some account has an error
    return {row.get('account'): row for row in accounts if isinstance(row, dict)}


def agy_usable(report, account, model):
    """Whether every quota pool that limits this model on this account has room."""
    from agy_model_catalog import quota_for_model
    row = report.get(account)
    if not row:
        raise HandoffError('no quota report for Antigravity account ' + str(account))
    if row.get('error'):
        raise HandoffError('Antigravity account ' + str(account) + ': ' + str(row['error']))
    remaining, _ = quota_for_model(row.get('buckets') or [], {'id': model})
    return remaining > 0


def quota_checkable(home, provider):
    """Whether provider resolves to Claude or Codex, whose usage this module reads."""
    try:
        provider_config(home, provider)
    except (HandoffError, OSError, ValueError, KeyError):
        return False
    return True


def usable_agents(home, agent_ids, quota_fn=None, stored_fn=None, warn=None, checkable_fn=None,
                  agy_account_fn=None, agy_report_fn=None):
    """The agents among agent_ids whose seat has quota left for their model now.

    Antigravity agents are checked against their account's cached quota, every
    pool that limits their model (`agy-best`'s rule); a missing or stale cache
    keeps the agent, with a note. Agents of a provider whose quota cannot be
    read here at all (copilot, opencode, ...) are kept with a note too: they
    were stopped by a limit, and leaving them out would hide them entirely.

    One usage check per (provider, model family), the same check the watcher
    makes before it sends. An agent whose check fails is left out and named on
    stderr: a resume sent into a seat that is still blocked only fails again.
    """
    quota_fn = quota_fn or quota
    stored_fn = stored_fn or stored_agent
    checkable_fn = checkable_fn or (lambda provider: quota_checkable(home, provider))
    agy_account_fn = agy_account_fn or (lambda provider: agy_account(home, provider))
    agy_report_fn = agy_report_fn or agy_quota_report
    agy_report = None
    warn = warn or (lambda text: print(text, file=sys.stderr))
    verdicts = {}
    for agent_id in agent_ids:
        try:
            record = stored_fn(agent_id, home)
        except (HandoffError, OSError, ValueError):
            warn(agent_id + ': no readable record; left out')
            continue
        provider = record.get('provider')
        model = (record.get('config') or {}).get('model') or (record.get('runtimeInfo') or {}).get('model') or ''
        snapshot = {'Model': model}
        account = None if checkable_fn(provider) else agy_account_fn(provider)
        if account is not None:
            key = ('agy', account, model)
            if key not in verdicts:
                try:
                    if agy_report is None:
                        agy_report = agy_report_fn()
                    verdicts[key] = agy_usable(agy_report, account, model)
                except (HandoffError, OSError, ValueError, KeyError, subprocess.SubprocessError) as error:
                    warn('Antigravity ' + account + ' ' + model + ': quota unknown (' + str(error) + '); its agents are kept')
                    verdicts[key] = True
            if verdicts[key]:
                yield agent_id
            continue
        if not checkable_fn(provider):
            warn(agent_id + ': ' + str(provider) + ' quota is not checked here; kept')
            yield agent_id
            continue
        key = (provider, model_family(snapshot))
        if key not in verdicts:
            try:
                verdicts[key] = quota_fn({'home': str(home), 'binding': {'provider': provider}}, snapshot)
            except (HandoffError, OSError, ValueError, KeyError, subprocess.SubprocessError) as error:
                warn(str(provider) + ': quota check failed (' + str(error) + '); its agents are left out')
                verdicts[key] = False
        if verdicts[key]:
            yield agent_id


def model_family(snapshot):
    model = snapshot.get('Model') or ''
    return model.removeprefix('claude-').split('-')[0].lower()


def quota_usable(payload, agent, family=''):
    if agent == 'codex':
        if payload.get('ok') is not True or not isinstance(payload.get('quota'), dict):
            raise HandoffError("Codex quota check failed")
        verdict = payload['quota'].get('blocked')
        if not isinstance(verdict, bool):
            raise HandoffError("Codex quota verdict unavailable")
        return not verdict
    if payload.get('error') or not isinstance(payload.get('windows'), list):
        raise HandoffError("Claude quota check failed")
    windows = []
    for window in payload['windows']:
        key = window.get('key', '')
        if key in ('session', 'five_hour', 'weekly_all', 'seven_day') or (
            family and key in ('weekly_scoped', 'seven_day_' + family)
            and family in window.get('label', '').lower()
        ):
            pct = window.get('utilization_percent')
            if not isinstance(pct, (int, float)):
                raise HandoffError("Claude quota utilization unavailable")
            windows.append(pct)
    if not windows:
        raise HandoffError("No applicable Claude quota window")
    return all(pct < 100 for pct in windows)


def quota(state, snapshot):
    identity, provider_env = provider_config(Path(state['home']), state['binding']['provider'])
    env = clean_env()
    # Do not inherit the caller's seat or global OAuth token for another seat.
    for key in list(env):
        if key.startswith('CLAUDE_CODE_OAUTH_TOKEN'):
            env.pop(key)
    env.update({k: str(v) for k, v in provider_env.items()})
    if identity['agent'] == 'codex':
        env['CODEX_HOME'] = identity['native_home']
        argv = [sys.executable, str(ROOT / 'codex_status.py'), '--json', '--no-all',
                '--timeout', '30', '--retries', '0', '--color', 'never']
    else:
        # An explicitly set ~/.claude can use a different keychain service
        # than an unset CLAUDE_CONFIG_DIR. Keep that distinction intact.
        native = provider_env.get('CLAUDE_CONFIG_DIR') or ''
        # Matches existing claude-code-usage's profile-specific token fallback.
        token_file = Path.home() / '.keys' / ('claude-code-oauth-' + identity['profile'])
        token_name = 'CLAUDE_CODE_OAUTH_TOKEN_' + identity['profile'].upper()
        if token_file.is_file() and token_name not in env:
            env[token_name] = token_file.read_text().strip()
        argv = [sys.executable, str(ROOT / 'claude_code_usage.py'), '--json',
                '--config-dir', native, '--profile-label', identity['profile'],
                '--refresh', '--no-relogin', '--timeout', '15', '--color', 'never']
    payload = json.loads(command(argv, timeout=60, env=env))
    return quota_usable(payload, identity['agent'], model_family(snapshot))


def tick(path, *, inspect_fn=inspect, stored_fn=stored_agent, quota_fn=quota, send_fn=command,
         transcript_fn=limit_failure):
    """Return one status; record a claim before sending to prevent ambiguous retries."""
    with locked(path.with_suffix('.lock')):
        state = read_json(path)
        if not state['enabled']:
            return 'off'
        home = Path(state['home'])
        snapshot = inspect_fn(state['agent_id'], home)
        if snapshot.get('Id') != state['agent_id'] or snapshot.get('Archived'):
            state.update(enabled=False, status='agent unavailable')
        elif binding(snapshot, home) != state['binding']:
            state.update(enabled=False, status='provider changed; re-arm explicitly')
        else:
            record = stored_fn(state['agent_id'], home)
            failure = failure_of(snapshot, record, state, transcript_fn)
            if snapshot.get('PendingPermissions') or (failure is None and snapshot.get('Status') != 'error'):
                state['status'] = 'waiting for a quota-failed turn'
            elif not failure or failure == state.get('claimed_failure'):
                state['status'] = 'waiting for a new quota-failed turn'
            elif not quota_fn(state, snapshot):
                state['status'] = 'waiting for quota recovery'
            else:
                # Recheck state after a potentially slow account query. The user
                # might have continued, archived or changed profiles meanwhile.
                fresh = inspect_fn(state['agent_id'], home)
                current = stored_fn(state['agent_id'], home)
                if (fresh.get('Id') != state['agent_id'] or fresh.get('Archived')
                    or fresh.get('Status') != snapshot.get('Status') or fresh.get('PendingPermissions')
                    or binding(fresh, home) != state['binding']
                    or failure_of(fresh, current, state, transcript_fn) != failure):
                    state['status'] = 'agent changed during quota check; skipped'
                else:
                    state.update(claimed_failure=failure, status='sending continuation')
                    atomic_write(path, state)
                    try:
                        send_fn(['paseo', 'send', state['agent_id'], 'Continue. If the task is already finished, say so briefly and run: zsh -c agent-auto-continue-off',
                                 '--home', state['home'], '--no-wait', '--json'])
                    except Exception:
                        # A timeout can mean Paseo accepted the send. Never retry
                        # that failed turn automatically; explicit re-arm clears it.
                        state.update(enabled=False, status='send uncertain; inspect agent and re-arm explicitly')
                        atomic_write(path, state)
                        raise
                    state['status'] = 'continuation sent; waiting for a new quota-failed turn'
        atomic_write(path, state)
        return state['status']


def session_name(path):
    return 'paseo-auto-continue-' + digest(str(path))[:16]


def worker_alive(path):
    try:
        command(['tmux', 'has-session', '-t', '=' + session_name(path)], timeout=10)
        return True
    except HandoffError:
        return False


def start_worker(path):
    if not worker_alive(path):
        argv = [sys.executable, str(Path(__file__).resolve()), 'watch', '--state', str(path)]
        # Multiple command arguments make tmux exec the worker directly rather
        # than invoking a shell that might change profiles or read shell secrets.
        command(['tmux', 'new-session', '-d', '-s', session_name(path), *argv], timeout=10)


def registrations(directory):
    """Enabled registrations as (agent id, home, agent/profile, status) rows.

    For pickers in another shell, so they need not parse the state files.
    """
    directory = Path(directory)
    if not directory.is_dir():
        return []
    rows = []
    for path in sorted(directory.glob('*.json')):
        try:
            state = read_json(path)
        except (OSError, ValueError, HandoffError):
            continue
        if not state.get('enabled'):
            continue
        binding = state.get('binding') or {}
        rows.append((state.get('agent_id', ''), state.get('home', ''),
                     str(binding.get('agent', '?')) + '/' + str(binding.get('profile', '?')),
                     str(state.get('status', ''))))
    return [row for row in rows if row[0] and row[1]]


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('action', choices=('on', 'off', 'status', 'list', 'watch', 'check', 'stopped'))
    parser.add_argument('agent_id', nargs='?', default=os.environ.get('PASEO_AGENT_ID'))
    parser.add_argument('--home', default=os.environ.get('PASEO_HOME') or str(Path.home() / '.paseo'))
    parser.add_argument('--poll', type=int, default=300)
    parser.add_argument('--usable', action='store_true',
                        help='with stopped: only agents whose seat has quota left now')
    parser.add_argument('--state')
    parser.add_argument('--state-dir', default=str(Path(os.environ.get('XDG_STATE_HOME', Path.home() / '.local/state')) / 'agent-sessions/auto-continue-paseo'))
    args = parser.parse_args()
    if args.action == 'stopped':
        # One agent ID per line: every local agent stopped at a usage limit.
        home = Path(args.home).expanduser().resolve()
        found = stopped_agents(home)
        if args.usable:
            found = usable_agents(home, list(found))
        for agent_id in found:
            print(agent_id, flush=True)
        return
    if args.action == 'list':
        for row in registrations(Path(args.state_dir).expanduser()):
            print('\t'.join(' '.join(field.split()) for field in row))
        return
    if args.action in ('watch', 'check'):
        if not args.state:
            parser.error('--state is required')
        path = Path(args.state)
        if args.action == 'check':
            print(tick(path))
            return
        while path.exists():
            try:
                status = tick(path)
                print(status, flush=True)
            except (HandoffError, OSError, ValueError, KeyError):
                print('Check unavailable; no message sent', flush=True)
            state = read_json(path)
            if not state.get('enabled'):
                return
            time.sleep(state['poll'])
        return
    if not args.agent_id:
        parser.error('pass an agent ID or run inside a Paseo agent')
    try:
        agent_id = str(uuid.UUID(args.agent_id))
    except ValueError:
        parser.error('an exact agent UUID is required')
    if args.poll < 30:
        parser.error('--poll must be at least 30 seconds')
    if os.environ.get('PASEO_HOST'):
        parser.error('remote hosts are unsupported; run on the daemon host with --home')
    home = Path(args.home).expanduser().resolve()
    directory = private_dir(Path(args.state_dir).expanduser())
    path = directory / (digest(str(home))[:16] + '-' + agent_id + '.json')
    if args.action == 'on':
        snapshot = inspect(agent_id, home)
        if snapshot.get('Id') != agent_id or snapshot.get('Archived'):
            raise HandoffError('Agent is unavailable or archived')
        state = {'version': 1, 'enabled': True, 'home': str(home), 'agent_id': agent_id,
                 'binding': binding(snapshot, home), 'poll': args.poll,
                 'status': 'waiting for a quota-failed turn', 'registered_at': time.time()}
        stored_agent(agent_id, home)  # Establish readable local record first.
        with locked(path.with_suffix('.lock')):
            if path.exists():
                previous = read_json(path)
                if previous.get('enabled') and previous.get('binding') == state['binding']:
                    state.update(claimed_failure=previous.get('claimed_failure'),
                                 status=previous.get('status', state['status']))
            atomic_write(path, state)
        try:
            start_worker(path)
        except HandoffError:
            with locked(path.with_suffix('.lock')):
                state.update(enabled=False, status='worker unavailable; re-arm explicitly')
                atomic_write(path, state)
            raise
        print('auto-continue on: ' + state['binding']['agent'] + '/' + state['binding']['profile']
              + ', target: paseo:' + agent_id + ', poll: ' + str(args.poll) + 's')
    elif args.action == 'off':
        if not path.exists():
            print('auto-continue was not on for this Paseo agent')
            return
        with locked(path.with_suffix('.lock')):
            state = read_json(path)
            state.update(enabled=False, status='off')
            atomic_write(path, state)
        # Disabled before return. The worker exits on its next tick; there is
        # no need to kill a process tree or race an in-flight send.
        print('auto-continue off for paseo:' + agent_id)
    else:
        if not path.exists():
            print('this Paseo agent: off')
            return
        state = read_json(path)
        print('this Paseo agent: ' + ('on' if state['enabled'] else 'off') + ', ' + state['status'])
        print('profile: ' + state['binding']['agent'] + '/' + state['binding']['profile'])
        print('target: paseo:' + agent_id + ', poll: ' + str(state['poll']) + 's')
        print('watcher: ' + ('running' if worker_alive(path) else 'not running') + ' (' + session_name(path) + ')')


if __name__ == '__main__':
    try:
        main()
    except (HandoffError, OSError, ValueError, KeyError) as exc:
        print('paseo-auto-continue: ' + (str(exc) if isinstance(exc, HandoffError) else 'state/check unavailable; no message sent'), file=sys.stderr)
        sys.exit(1)
