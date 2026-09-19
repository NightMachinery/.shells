import { describe, expect, test } from 'bun:test';
import { settling } from '../src/page/data.ts';
import type { BoardStatus } from '../src/page/types.ts';

// The bug these cover reached a reader's phone and stayed there for a whole
// visit: the first board on the screen showed a full list of departures with a
// progress line under it saying it was still on page one. Nothing was still
// loading. A later board's interchange lookup had reported its progress under
// the first board's name, because the stop it asked about belonged to no board
// and an unattributable stop was charged to board zero.

describe('settling', () => {
  const record = (): { seen: Array<[number, BoardStatus]>; report: (index: number, status: BoardStatus) => void } => {
    const seen: Array<[number, BoardStatus]> = [];
    const report = settling((index, status) => seen.push([index, status]));
    return { seen, report };
  };

  test('passes progress through until the board answers', () => {
    const { seen, report } = record();
    report(0, { kind: 'loading', backend: null, page: 0 });
    report(0, { kind: 'loading', backend: 'mvg', page: 1 });
    report(0, { kind: 'ready' });
    expect(seen.map(([, status]) => status.kind)).toEqual(['loading', 'loading', 'ready']);
  });

  test('a board that has answered cannot go back to loading', () => {
    const { seen, report } = record();
    report(0, { kind: 'loading', backend: 'mvg', page: 1 });
    report(0, { kind: 'ready' });
    report(0, { kind: 'loading', backend: 'mvg', page: 1 });
    expect(seen).toHaveLength(2);
    expect(seen[1]?.[1].kind).toBe('ready');
  });

  test('an error settles a board too', () => {
    const { seen, report } = record();
    report(1, { kind: 'error', detail: 'timeout' });
    report(1, { kind: 'loading', backend: 'transitous', page: 2 });
    expect(seen).toHaveLength(1);
    expect(seen[0]?.[1].kind).toBe('error');
  });

  test('one board settling says nothing about another', () => {
    const { seen, report } = record();
    report(0, { kind: 'ready' });
    report(2, { kind: 'loading', backend: 'mvg', page: 1 });
    expect(seen).toHaveLength(2);
    expect(seen[1]?.[0]).toBe(2);
  });
});
