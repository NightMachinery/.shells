"""Process ancestry accounting for [agfi:mem-report2]; never signals processes."""
import argparse
from collections import defaultdict
from dataclasses import dataclass, field
import fnmatch
import json
import os
from pathlib import Path
import re
import shlex
import subprocess
import sys

# One policy list for shells/interpreters whose executable alone says little.
OPAQUE = 'zsh bash sh dash fish ksh tcsh csh python* pypy* node nodejs perl ruby bun deno uv'.split()


def run(args, optional=False, timeout=15):
    try:
        return subprocess.check_output(args, text=True, stderr=subprocess.DEVNULL, timeout=timeout)
    except (OSError, subprocess.SubprocessError):
        if optional:
            return ''
        raise RuntimeError(f'cannot collect {args[0]} data') from None


def clean(s):
    return re.sub(r'[\x00-\x1f\x7f]', ' ', str(s))


def shorten(s):
    home = str(Path.home())
    return '~' + s[len(home):] if s == home or s.startswith(home + '/') else s


def human(b):
    # Same binary units and rounding as [agfi:mem-humanize].
    units = 'BKM GTP'.replace(' ', '')
    i = 0
    while b >= 1024 and i < len(units) - 1:
        b /= 1024
        i += 1
    return f'{b:.1f}{units[i]}' if i else f'{int(b)}B'


def to_bytes(s):
    m = re.fullmatch(r'([\d.]+)([BKMGT]?)[+\-]?', s)
    if not m:
        raise ValueError(f'unknown top memory value: {s}')
    return float(m[1]) * 1024 ** ('BKMGT'.index(m[2]) if m[2] else 0)


@dataclass
class Proc:
    pid: int
    parent: int
    age: str
    exe: str
    args: str
    env: dict = field(default_factory=dict)


def snapshot(text):
    procs = {}
    for line in text.splitlines():
        f = line.split(None, 4)
        if len(f) != 5 or not f[0].isdigit():
            continue
        pid, parent = map(int, f[:2])
        # ps -E appends the environment. Keep only selected ownership markers,
        # never render or persist an environment or a complete command line.
        env = {k: m[1] for k in ('PASEO_AGENT_ID', 'CLAUDE_CONFIG_DIR', 'CODEX_THREAD_ID')
               if (m := re.search(r'(?:^|\s)' + k + r'=(\S+)', f[4]))}
        args = re.split(r'\s+[A-Za-z_][A-Za-z_0-9]*=', f[4], maxsplit=1)[0]
        procs[pid] = Proc(pid, parent, f[2], f[3].lstrip('-'), args, env)
    return procs


def ancestors(pid, procs):
    seen = set()
    while pid in procs and pid not in seen:
        seen.add(pid)
        yield pid
        pid = procs[pid].parent


def provider(p):
    if p.exe in ('claude', 'claude.exe'):
        return 'Claude'
    if p.exe == 'codex' or p.exe.startswith('codex-code-mode'):
        return 'Codex'
    return None


def label(p):
    app = re.search(r'/([^/]+)\.app/', p.args)
    if app:
        return app[1]
    try:
        args = shlex.split(p.args)
    except ValueError:
        args = p.args.split()
    # A named script is a meaningful owner even when its runtime is opaque.
    if p.exe in OPAQUE or any(fnmatch.fnmatch(p.exe.lower(), x) for x in OPAQUE):
        if len(args) > 1 and not args[1].startswith('-') and re.search(r'\.(py|js|mjs|ts|dash|sh|zsh)$', args[1]):
            return Path(args[1]).name
    return p.exe


def opaque(p):
    return label(p) == p.exe and any(fnmatch.fnmatch(p.exe.lower(), x) for x in OPAQUE)


def session_roots(procs):
    roots = {}
    for pid, p in procs.items():
        kind = provider(p)
        if not kind:
            continue
        # Helpers/native children belong to the outer session. A different
        # explicit Paseo id denotes a separate nested session.
        outer = next((q for q in list(ancestors(pid, procs))[1:] if provider(procs[q]) == kind), None)
        if outer and (not p.env.get('PASEO_AGENT_ID') or p.env.get('PASEO_AGENT_ID') == procs[outer].env.get('PASEO_AGENT_ID')):
            continue
        roots[pid] = kind
    return roots


def tree(procs, memory, panes, titles, cwd):
    roots = session_roots(procs)
    # A tree of group keys; each PID contributes once to each enclosing total.
    nodes = {}
    def add(keys, values):
        current = nodes
        for key in keys:
            node = current.setdefault(key, {'foot': 0, 'cmprs': 0, 'count': 0, 'children': {}})
            node['foot'] += values[0]
            node['cmprs'] += values[1]
            node['count'] += 1
            current = node['children']
    names = {}
    for root, kind in roots.items():
        p = procs[root]
        chain = list(ancestors(root, procs))
        aid = p.env.get('PASEO_AGENT_ID')
        name = titles.get(aid)
        if not name:
            name = next((panes[q] for q in chain if q in panes), None)
        if not name:
            m = re.search(r'(?:^|\s)(?:--name(?:=|\s+)|-n\s+)("[^"]*"|\S+)', p.args)
            name = m[1].strip('"') if m else None
        if not name:
            under_paseo = any('paseo' in procs[q].args.lower() for q in chain[1:])
            name = ('Paseo (unmapped) ' if under_paseo else '') + cwd(root)
        account = ''
        if kind == 'Claude':
            config = p.env.get('CLAUDE_CONFIG_DIR', '')
            account = ' [account: ' + ('default' if not config or config == str(Path.home() / '.claude') else ('work' if Path(config).name == '.claude-work' else shorten(config))) + ']'
        names[root] = clean(f'{name}{account} [pid {root}, age {p.age}]')
    mosh_sessions = set()
    for pid, values in memory.items():
        chain = list(ancestors(pid, procs))
        root = next((q for q in chain if q in roots), None)
        if root:
            # Actual process ancestry, including MCP/REPL/browser descendants.
            branch = chain[:chain.index(root) + 1][::-1]
            keys = [roots[root], names[root]] + [f'{label(procs[q])} [{q}]' for q in branch]
        else:
            owner = next((q for q in chain if not opaque(procs[q])), pid)
            name = label(procs[owner]) if owner in procs else '(exited before ps)'
            pane = next((panes[q] for q in chain if q in panes), None)
            if pane:
                keys = ['tmux', 'session: ' + clean(pane)]
            elif name == 'mosh-server':
                mosh_sessions.add(owner)
                keys = ['mosh-server']
            else:
                keys = [clean(name)]
        add(keys, values)
    if 'mosh-server' in nodes:
        nodes[f'mosh-server ({len(mosh_sessions)} sessions)'] = nodes.pop('mosh-server')
    return nodes


def render(nodes, limit, ratio):
    print(f'{"PROCESS GROUP":<64} {"PROCS":>6} {"FOOTPRINT":>10} {"CMPRS RAW":>10} {"~IN RAM":>10}')
    def rows(items, depth=0):
        for key, node in sorted(items.items(), key=lambda kv: (-kv[1]['foot'], kv[0])):
            print(f'{"  " * depth + key:<64} {node["count"]:>6} {human(node["foot"]):>10} {human(node["cmprs"]):>10} {human(node["cmprs"] / ratio):>10}')
            rows(node['children'], depth + 1)
    ranked = sorted(nodes, key=lambda k: (-nodes[k]['foot'], k))
    rows({k: nodes[k] for k in ranked[:limit]})
    if len(nodes) > limit:
        print(f'({len(nodes) - limit} more top-level groups)')
    total = [sum(n[k] for n in nodes.values()) for k in ('count', 'foot', 'cmprs')]
    print(f'\nTOTAL: {total[0]} processes, footprint {human(total[1])}, compressed raw {human(total[2])}')
    print(f'CMPRS RAW is pre-compression; ~IN RAM uses the live {ratio:.1f}x ratio.')
    print('Footprints overlap on shared memory; totals are an upper bound. Process snapshots can race exits.')


def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument('limit', type=int, nargs='?', default=15)
    ap.add_argument('--ratio', type=float, default=1)
    opts = ap.parse_args()
    if opts.limit < 0 or opts.ratio <= 0:
        ap.error('limit must be nonnegative and ratio positive')
    if sys.platform != 'darwin':
        ap.error('macOS top physical footprint is required')
    # One ps, including environments: avoids a second scan or per-PID ps.
    procs = snapshot(run(['ps', '-E', '-axo', 'pid=,ppid=,etime=,ucomm=,command=']))
    memory = {}
    for line in run(['top', '-l', '1', '-stats', 'pid,mem,cmprs'], timeout=30).splitlines():
        f = line.split()
        if len(f) >= 3 and f[0].isdigit():
            memory[int(f[0])] = (to_bytes(f[1]), to_bytes(f[2]))
    if not memory or not procs:
        raise RuntimeError('empty process snapshot; check process inspection permissions')
    panes = {}
    for line in run(['tmux', 'list-panes', '-a', '-F', '#{pane_pid} #{session_name}'], optional=True).splitlines():
        f = line.split(None, 1)
        if len(f) == 2 and f[0].isdigit():
            panes[int(f[0])] = f[1]
    titles = {}
    data = run(['paseo', 'ls', '--all', '--global', '--json'], optional=True)
    if data:
        try:
            titles = {a['id']: a.get('name') or a.get('title') or a['id'][:8] for a in json.loads(data)}
        except (ValueError, KeyError, TypeError):
            print('warning: Paseo title listing unavailable', file=sys.stderr)
    def cwd(pid):
        text = run(['lsof', '-a', '-p', str(pid), '-d', 'cwd', '-Fn'], optional=True, timeout=3)
        return clean(shorten(next((s[1:] for s in text.splitlines() if s.startswith('n')), '(cwd unavailable)')))
    render(tree(procs, memory, panes, titles, cwd), opts.limit, opts.ratio)


if __name__ == '__main__':
    try:
        main()
    except (RuntimeError, ValueError) as exc:
        sys.exit(str(exc))
