"""macOS disconnected mosh cleanup. Only explicit, identity-checked PIDs are signaled."""
import argparse
import csv
from dataclasses import dataclass
import math
import os
from pathlib import Path
import re
import signal
import subprocess
import sys
import time

SHELLS = {'zsh', 'bash', 'sh', 'dash'}


@dataclass(frozen=True)
class Process:
    pid: int
    parent: int
    uid: int
    tty: str
    start: str
    name: str
    age: str

    @property
    def identity(self):
        return self.pid, self.uid, self.start, self.name


def run(args, timeout=25):
    return subprocess.check_output(args, text=True, stderr=subprocess.PIPE, timeout=timeout)


def snapshot():
    # lstart distinguishes reused PIDs; ucomm is independent of argv text.
    result = {}
    for line in run(['ps', '-axo', 'pid=,ppid=,uid=,tty=,lstart=,etime=,ucomm=']).splitlines():
        f = line.split(None, 10)
        if len(f) != 11:
            continue
        pid, parent, uid = map(int, f[:3])
        result[pid] = Process(pid, parent, uid, f[3], ' '.join(f[4:9]), f[10].strip().lstrip('-'), f[9])
    return result


def traffic():
    data = run(['nettop', '-P', '-L', '1', '-x', '-J', 'bytes_in,bytes_out'])
    result = {}
    for row in csv.reader(data.splitlines()):
        if row and (m := re.fullmatch(r'mosh-server\.(\d+)', row[0])) and len(row) >= 3:
            if row[1].isdigit() and row[2].isdigit():
                result[int(m[1])] = int(row[1])
    return result


def descendants(pid, procs):
    found, pending = set(), [pid]
    children = {}
    for p in procs.values():
        children.setdefault(p.parent, []).append(p.pid)
    while pending:
        for child in children.get(pending.pop(), []):
            if child not in found and child != pid:
                found.add(child)
                pending.append(child)
    return found


def details(server, procs, now):
    direct = [p for p in procs.values() if p.parent == server.pid]
    # Before a throwaway server's first client arrives its pty child is still
    # mosh-server, waiting to exec the shell. It is the session root too.
    roots = [p for p in direct if p.tty != '??']
    idle = None
    try:
        if roots:
            idle = min(max(0, now - os.stat('/dev/' + p.tty).st_atime) / 3600 for p in roots)
    except OSError:
        pass
    all_desc = descendants(server.pid, procs)
    bootstrap = {p.pid for p in roots if p.name == 'mosh-server'}
    jobs = [procs[pid] for pid in sorted(all_desc) if procs[pid].name not in SHELLS and pid not in bootstrap]
    return idle, jobs, all_desc, {p.pid for p in roots}


def verdict(delta, idle, jobs, hours, force):
    if delta is None or delta < 0:
        return 'skip: unknown traffic'
    if delta:
        return 'keep: receiving'
    if idle is None:
        return 'skip: unknown tty idle'
    if idle < hours:
        return 'keep: recent input'
    if jobs and not force:
        return 'skip: running job'
    return 'kill: silent and idle' + (' (forced jobs)' if jobs else '')


def same(p, current):
    return p.pid in current and p.identity == current[p.pid].identity


def send(p, sig, current):
    if p.uid != os.getuid() or not same(p, current):
        return False
    try:
        os.kill(p.pid, sig)
        print(f'  {signal.Signals(sig).name} {p.pid} ({p.name})', flush=True)
        return True
    except ProcessLookupError:
        return False


def terminate(server, initial, roots):
    current = snapshot()
    if not same(server, current) or server.name != 'mosh-server':
        print(f'  skip {server.pid}: process exited or changed')
        return
    tracked = {pid: p for pid, p in initial.items() if pid in descendants(server.pid, current)}
    tracked.update({pid: current[pid] for pid in descendants(server.pid, current)})
    send(server, signal.SIGTERM, current)
    time.sleep(3)
    current = snapshot()
    send(server, signal.SIGKILL, current)
    # Capture late children still attached to the old pty roots. Never discover
    # by name/pattern: only ancestry from identity-checked session processes.
    for pid in roots:
        old = initial.get(pid)
        if old and same(old, current):
            tracked.update({q: current[q] for q in descendants(pid, current)})
    survivors = [p for p in tracked.values() if same(p, current)]
    if survivors:
        print('  orphan/session survivors:', ' '.join(str(p.pid) for p in survivors))
        for p in survivors:
            send(p, signal.SIGTERM, snapshot())
        time.sleep(2)
        for p in survivors:
            send(p, signal.SIGKILL, snapshot())
    time.sleep(0.2)
    current = snapshot()
    remaining = [p.pid for p in [server, *tracked.values()] if same(p, current)]
    if remaining:
        raise RuntimeError(f'surviving session processes: {remaining}')
    print(f'  cleaned {server.pid}; no tracked descendants remain')


def main():
    ap = argparse.ArgumentParser(description=__doc__, epilog='Future sessions: MOSH_SERVER_NETWORK_TMOUT exits after network silence; MOSH_SERVER_SIGNAL_TMOUT gates SIGUSR1. Set them before mosh-server starts, e.g. in ~/.zshenv. Without SIGNAL_TMOUT, SIGUSR1 can terminate connected sessions. No dotfiles are changed.')
    ap.add_argument('-n', '--dry-run', action='store_true')
    ap.add_argument('--idle-hours', type=float, default=24)
    ap.add_argument('--force', action='store_true', help='allow non-shell jobs; shell-only helpers never block cleanup')
    ap.add_argument('--only', type=int, nargs='+', help='restrict to these exact mosh-server PIDs')
    opts = ap.parse_args()
    if not math.isfinite(opts.idle_hours) or opts.idle_hours < 0 or opts.only is not None and any(p <= 1 for p in opts.only):
        ap.error('idle hours must be finite and nonnegative; PIDs must exceed 1')
    if sys.platform != 'darwin':
        ap.error('macOS ps/nettop and tty access times are required')
    before = snapshot()
    servers = [p for p in before.values() if p.name == 'mosh-server' and p.uid == os.getuid() and (opts.only is None or p.pid in opts.only)]
    # Only actual parent servers: the not-yet-execed pty child shares ucomm.
    servers = [p for p in servers if before.get(p.parent, p).name != 'mosh-server' or p.parent not in before]
    if opts.only and set(opts.only) - {p.pid for p in servers}:
        ap.error('--only includes a PID that is not an owned parent mosh-server')
    if not servers:
        print('No owned mosh-server sessions in scope.')
        return
    print(f'Sampling {len(servers)} sessions over 15 seconds (dry run: {opts.dry_run})...', flush=True)
    first = traffic()
    time.sleep(15)
    second = traffic()
    after = snapshot()
    print('PID       AGE           BYTES_IN    INPUT_IDLE   NON-SHELL DESCENDANTS                       VERDICT', flush=True)
    selected = []
    for p in sorted(servers, key=lambda p: p.pid):
        idle, jobs, desc, roots = details(p, after, time.time())
        delta = second[p.pid] - first[p.pid] if p.pid in first and p.pid in second else None
        decision = verdict(delta, idle, jobs, opts.idle_hours, opts.force) if same(p, after) else 'skip: exited or changed'
        job_text = ', '.join(f'{q.pid}:{q.name}' for q in jobs) or '-'
        idle_text = f'{idle:.1f}h' if idle is not None else '?'
        print(f'{p.pid:<9} {p.age:<13} {str(delta) if delta is not None else "?":>9} {idle_text:>12}   {job_text:<42} {decision}', flush=True)
        if decision.startswith('kill:'):
            selected.append((p, roots))
    if opts.dry_run:
        return
    for p, roots in selected:
        # Revalidate identity, input idle and jobs immediately before signaling.
        current = snapshot()
        if not same(p, current):
            continue
        idle, jobs, _, roots = details(p, current, time.time())
        if not verdict(0, idle, jobs, opts.idle_hours, opts.force).startswith('kill:'):
            print(f'  skip {p.pid}: input or jobs changed')
            continue
        terminate(p, current, roots)


# Mosh 1.4.0 source: src/frontend/mosh-server.cc, serve(), SIGUSR1 condition.
# MOSH_SERVER_NETWORK_TMOUT and MOSH_SERVER_SIGNAL_TMOUT are seconds in the
# server's startup environment. An unset/zero SIGNAL_TMOUT does NOT protect
# connected sessions from SIGUSR1. Leave prevention settings to the owner.
if __name__ == '__main__':
    try:
        main()
    except (OSError, subprocess.SubprocessError, RuntimeError) as exc:
        sys.exit(f'mosh-cleanup: {exc}')
