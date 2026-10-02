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
import sys
import tempfile
import time
import uuid

from paseo_handoff import command, clean_env, HandoffError

ROOT = Path(__file__).resolve().parent
QUOTA_ERROR = re.compile(
    r"usage[_ -]limit|rate[_ -]limit|quota (?:exceeded|exhausted)|"
    r"(?:you(?:'ve| have)?|account) (?:hit|reached|exceeded) .*limit|"
    r"(?:hit|reached) your limit|out of .*usage|too many requests", re.I)


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


def quota_failure(record):
    """Account exhaustion elsewhere must not resume an intentionally idle agent."""
    if record.get('archivedAt') or record.get('lastStatus') != 'error':
        return None
    error = record.get('lastError')
    if not isinstance(error, str) or not QUOTA_ERROR.search(error):
        return None
    # lastActivityAt can move for incidental events. The failed turn identity
    # instead uses its user message and attention timestamp, plus error text.
    return digest([error, record.get('lastUserMessageAt'), record.get('attentionTimestamp')])


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


def tick(path, *, inspect_fn=inspect, stored_fn=stored_agent, quota_fn=quota, send_fn=command):
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
            failure = quota_failure(record)
            if snapshot.get('Status') != 'error' or snapshot.get('PendingPermissions'):
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
                    or fresh.get('Status') != 'error' or fresh.get('PendingPermissions')
                    or binding(fresh, home) != state['binding']
                    or quota_failure(current) != failure):
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


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('action', choices=('on', 'off', 'status', 'watch', 'check'))
    parser.add_argument('agent_id', nargs='?', default=os.environ.get('PASEO_AGENT_ID'))
    parser.add_argument('--home', default=os.environ.get('PASEO_HOME') or str(Path.home() / '.paseo'))
    parser.add_argument('--poll', type=int, default=300)
    parser.add_argument('--state')
    parser.add_argument('--state-dir', default=str(Path(os.environ.get('XDG_STATE_HOME', Path.home() / '.local/state')) / 'agent-sessions/auto-continue-paseo'))
    args = parser.parse_args()
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
