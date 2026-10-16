"""Read-only eligibility planner for [agfi:paseo-archive-fz] and -all."""
import argparse
from datetime import datetime, timezone
import json
from pathlib import Path
import re
import sys


def duration(value):
    if value == '0':
        return 0
    m = re.fullmatch(r'(\d+(?:\.\d+)?)([mhd])', value)
    if not m:
        raise ValueError('minimum idle must be 0 or a duration such as 90m, 72h, 3d')
    return float(m[1]) * {'m': 60, 'h': 3600, 'd': 86400}[m[2]]


def activity(record):
    for key in ('lastActivityAt', 'updatedAt', 'createdAt'):
        value = record.get(key)
        if value is not None:
            try:
                dt = datetime.fromisoformat(value.replace('Z', '+00:00'))
                return dt.timestamp() if dt.tzinfo else None
            except (ValueError, TypeError, AttributeError):
                return None
    return None


def load_records(home):
    directory = Path(home) / 'agents'
    files = sorted(set(directory.glob('*.json')) | set(directory.glob('*/*.json')))
    if not files:
        raise ValueError('no local agent records; refusing an incomplete cascade/pin check')
    records = {}
    for file in files:
        try:
            raw = json.loads(file.read_text())
            ident = raw['id']
            labels = raw.get('labels') or {}
            if not isinstance(labels, dict) or not isinstance(ident, str):
                raise ValueError()
            # Never expose persistence metadata, transcript data or credentials.
            record = {key: raw.get(key) for key in ('id', 'title', 'cwd', 'lastStatus', 'archivedAt',
                                                    'lastActivityAt', 'updatedAt', 'createdAt')}
            record['keep'] = str(labels.get('keep', '')).lower() == 'true'
            record['parent'] = labels.get('paseo.parent-agent-id')
            if ident in records and records[ident] != record:
                raise ValueError()
            records[ident] = record
        except (OSError, ValueError, KeyError, TypeError):
            raise ValueError('cannot read consistent agent records; refusing archive planning') from None
    return records


def plan(live, records, *, scope='global', cwd='', caller='', parent='', minimum=72*3600,
         force=False, kind='fz', now=None, show_hidden=False, summary=False):
    now = datetime.now(timezone.utc).timestamp() if now is None else now
    current = {a['id']: a for a in live}
    if current.keys() - records.keys():
        raise ValueError('live agents lack local records; refusing an incomplete cascade/pin check')
    active = {i: r for i, r in records.items() if r['archivedAt'] is None}
    protected = set()
    for root in (caller, parent):
        seen = set()
        while root and root not in seen:
            seen.add(root)
            protected.add(root)
            root = records.get(root, {}).get('parent')
    children = {}
    for ident, r in active.items():
        if r.get('parent'):
            children.setdefault(r['parent'], []).append(ident)
    stamp = {i: activity(r) for i, r in active.items()}
    def in_scope(r):
        return scope == 'global' or Path(str(r.get('cwd') or '')).expanduser().resolve() == Path(cwd).resolve()
    def own_reason(ident, descendant=False):
        r = active[ident]
        if ident in protected:
            return 'caller'
        if r['keep']:
            return 'pinned'
        if ident not in current:
            return 'missing live metadata'
        if not in_scope(r):
            return 'outside directory'
        status = current[ident].get('status')
        if status not in ('idle', 'running'):
            return 'unknown status'
        # Running descendants are ALWAYS blockers, including with --force.
        if status == 'running' and (descendant or not (force and kind == 'fz')):
            return 'running'
        if not force and minimum > 0:
            if stamp[ident] is None:
                return 'unknown activity'
            idle = max(0, now - stamp[ident])
            if idle < minimum:
                return f'recent {idle / 3600:.1f}h'
        return ''
    def descend(ident, seen=None):
        seen = {ident} if seen is None else seen
        for child in sorted(children.get(ident, []), key=lambda i: (stamp[i] or float('inf'), i)):
            if child in seen:
                raise ValueError('cyclic subagent relationship')
            seen.add(child)
            yield child
            yield from descend(child, seen)
    def safe(value):
        return re.sub(r'[\x00-\x1f\x7f]', ' ', str(value))
    rows, hidden = [], {}
    for ident in sorted(active, key=lambda i: (stamp[i] if stamp[i] is not None else float('inf'), i)):
        if ident not in current or not in_scope(active[ident]):
            continue
        try:
            descendants = list(descend(ident))
            # Prefer running-descendant reason over recency so a busy
            # coordinator is clearly identifiable in --show-hidden output.
            blockers = [(child, own_reason(child, descendant=True)) for child in descendants]
            blockers.sort(key=lambda b: 0 if b[1] == 'running' else 1)
            reason = own_reason(ident)
            if reason not in ('pinned', 'caller', 'running'):
                for child, why in blockers:
                    if why:
                        reason = f'{why.split()[0]} subagent {child[:8]}'
                        break
        except ValueError:
            descendants, reason = [], 'cyclic subagents'
        if reason:
            category = ('parents of running subagents' if reason.startswith('running subagent') else
                        'parents of recent subagents' if reason.startswith('recent subagent') else
                        'recent' if reason.startswith('recent ') else reason.split(' subagent ')[0])
            hidden[category] = hidden.get(category, 0) + 1
            if not show_hidden:
                continue
        a = current[ident]
        age = max(0, now - stamp[ident]) if stamp[ident] is not None else None
        relative = '?' if age is None else (f'{age / 86400:.1f}d ago' if age >= 86400 else f'{age / 3600:.1f}h ago' if age >= 3600 else f'{age / 60:.0f}m ago')
        names = [f'{child[:8]} {active[child].get("title") or "(untitled)"} [{current.get(child, {}).get("status", "?")}]' for child in descendants]
        cascade = f'archives {len(names)} subagents with it: ' + '; '.join(names) if names else '-'
        row = [ident, a.get('shortId') or ident[:8], 'RUNNING!' if a.get('status') == 'running' else a.get('status', '?'),
               relative, a.get('provider', '?'), a.get('name') or a.get('title') or '(untitled)', a.get('cwd', '?'),
               'hidden' if reason else 'selectable', cascade, ','.join(descendants) or '-', reason or '-']
        rows.append('\t'.join(map(safe, row)))
    if summary:
        count = sum(hidden.values())
        detail = ', '.join(f'{n} {why}' for why, n in sorted(hidden.items()))
        rows.append(f'#summary\t{count} hidden' + (f' ({detail})' if detail else '') + '; --show-hidden lists them')
    return rows


def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument('--home', required=True)
    ap.add_argument('--scope', choices=['global', 'local'], default='global')
    ap.add_argument('--cwd', default='')
    ap.add_argument('--caller', default='')
    ap.add_argument('--parent', default='')
    ap.add_argument('--min-idle', default='72h')
    ap.add_argument('--kind', choices=['fz', 'all'], default='fz')
    ap.add_argument('--force', action='store_true')
    ap.add_argument('--show-hidden', action='store_true')
    ap.add_argument('--summary', action='store_true')
    opts = ap.parse_args()
    minimum = duration(opts.min_idle)
    live = json.load(sys.stdin)
    records = load_records(opts.home)
    for row in plan(live, records, scope=opts.scope, cwd=opts.cwd, caller=opts.caller,
                    parent=opts.parent, minimum=minimum, force=opts.force, kind=opts.kind,
                    show_hidden=opts.show_hidden, summary=opts.summary):
        print(row)


if __name__ == '__main__':
    try:
        main()
    except (ValueError, KeyError, TypeError, OSError) as exc:
        sys.exit(f'paseo-archive: {exc}')
