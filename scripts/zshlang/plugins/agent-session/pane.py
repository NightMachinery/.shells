#!/usr/bin/env python3
"""Private pane state and a lock inherited by the shell and its agent process."""
import fcntl
import json
import os
from pathlib import Path
import re
import shlex
import sys
import tempfile
import time


def read(path):
    return json.loads(Path(path).read_text())


def write(path, data):
    path = Path(path)
    fd, tmp = tempfile.mkstemp(prefix='.state-', dir=path.parent)
    try:
        with os.fdopen(fd, 'w') as out:
            json.dump(data, out)
            out.write('\n')
        os.replace(tmp, path)
    finally:
        if os.path.exists(tmp):
            os.unlink(tmp)


def identity(state, provider, ident, transcript=''):
    spec = read(Path(state) / 'launch.json')
    if provider != spec['provider'] or not re.fullmatch(r'[0-9a-fA-F]{8}(?:-[0-9a-fA-F]{4}){3}-[0-9a-fA-F]{12}', ident):
        raise ValueError('identity does not match the launched provider')
    with open(Path(state) / 'identity.lock', 'a') as lock:
        fcntl.flock(lock, fcntl.LOCK_EX)
        dest = Path(state) / 'identity.json'
        old = read(dest) if dest.exists() else {}
        if old and old['id'] != ident:
            raise ValueError('refusing to replace the pane conversation with another ID')
        write(dest, dict(provider=provider, id=ident,
                         transcript=transcript or old.get('transcript', '')))


def main():
    os.umask(0o077)
    op, *args = sys.argv[1:]
    if op == 'identity':
        identity(*args)
    elif op == 'hook':
        state, provider = args
        payload = json.load(sys.stdin)
        ident = payload.get('session_id') or payload.get('thread-id') or payload.get('conversationId')
        if ident:
            identity(state, provider, ident, payload.get('transcript_path') or payload.get('transcriptPath', ''))
    elif op == 'matches':
        state, ident = args
        sys.exit(0 if read(Path(state) / 'identity.json')['id'] == ident else 1)
    elif op == 'hooks':
        sys.stdout.write('\0'.join(read(args[0])))
    elif op == 'shell':
        state, mode = args
        spec = read(Path(state) / 'launch.json')
        ident = read(Path(state) / 'identity.json') if mode == 'resume' else {}
        if mode == 'resume' and (not ident.get('id') or not spec['resume']):
            raise ValueError('exact resume is unavailable; no new conversation was started')
        env = dict(spec['environment'])
        env.update(AGENT_SESSION_CWD=spec['cwd'], AGENT_SESSION_ID=ident.get('id', ''),
                   AGENT_SESSION_INITIAL_COMMAND=spec['initial'], AGENT_SESSION_RESUME_COMMAND=spec['resume'],
                   AGENT_SESSION_HOOK_ARGS_FILE=str(Path(state) / 'hooks.json'),
                   AGENT_SESSION_STATE=state, AGENT_SESSION_REUSE_PANE=None)
        # Parent conversation IDs must never be mistaken for child IDs.
        for key in ('CLAUDE_CODE_SESSION_ID', 'CODEX_THREAD_ID', 'CODEX_SESSION_ID', 'ANTIGRAVITY_CONVERSATION_ID'):
            env[key] = None
        for key, value in env.items():
            print('unset ' + key if value is None else 'export ' + key + '=' + shlex.quote(value))
    elif op == 'run':
        state, plugin = args
        spec = read(Path(state) / 'launch.json')
        fd = os.open(str(Path(state) / 'process.lock'), os.O_CREAT | os.O_RDWR, 0o600)
        deadline = time.monotonic() + 5
        while True:
            try:
                fcntl.flock(fd, fcntl.LOCK_EX | fcntl.LOCK_NB)
                break
            except BlockingIOError:
                if time.monotonic() >= deadline:
                    raise ValueError('previous pane process is still running; refusing a duplicate resume')
                time.sleep(.1)
        # Keep the lock across exec and in descendants that outlive the shell.
        os.set_inheritable(fd, True)
        marker = Path(state) / 'started'
        mode = 'resume' if marker.exists() else 'initial'
        if mode == 'resume' and not (Path(state) / 'identity.json').exists():
            raise ValueError('conversation identity not recorded yet; no new conversation was started')
        if mode == 'resume' and not spec['resume']:
            raise ValueError('no resume command configured; no new conversation was started')
        marker.touch(mode=0o600, exist_ok=True)
        command = 'source "$1" && agent-session-pane-run "$2" "$3"'
        os.execvp('zsh', ['zsh', '-ic', command, 'agent-session', plugin, state, mode])
    else:
        raise ValueError('unknown operation: ' + op)


if __name__ == '__main__':
    try:
        main()
    except (ValueError, OSError, KeyError) as error:
        print('\033[H\033[2J\nagent-session: ' + str(error), file=sys.stderr)
        sys.exit(1)
