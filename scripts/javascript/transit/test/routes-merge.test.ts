import { describe, expect, test } from 'bun:test';
import { mergeBoardRoutes, ROUTE_AGEING_MS, type BoardRoutes } from '../src/page/commute.ts';
import type { PlannedRow } from '../src/plan.ts';

// A journey search is a search, not a lookup: the aggregator answers for the
// rows it found in the window it looked at, and a row at the edge of that
// window drops out of one answer and comes back in the next. On the real page
// one row in fifty lost its journey for forty-one seconds that way, and the
// slot went blank, which reads as "there is no way to get there from this one".
// These are the tests for not doing that.

function row(id: string): PlannedRow {
  return { departure: { line: id } as never, options: [], best: { exitStop: id } as never };
}

/** A row the search answered for by saying there is no way. */
function miss(id: string): PlannedRow {
  return { departure: { line: id } as never, options: [], best: null, miss: 'nothing found' };
}

function routes(keys: string[], destinationKey = 'work'): BoardRoutes {
  return { rows: new Map(keys.map((key) => [key, row(key)])), origin: null, destinationKey };
}

describe('carrying a journey across a search that did not answer for it', () => {
  const NOW = 1_700_000_000_000;

  test('a row the new search answered for takes the new answer, dated now', () => {
    const previous: BoardRoutes = { ...routes(['a']), plannedAt: new Map([['a', NOW - 60_000]]) };
    const merged = mergeBoardRoutes(previous, routes(['a']), NOW);
    expect(merged.rows.get('a')?.best?.exitStop).toBe('a');
    expect(merged.plannedAt?.get('a')).toBe(NOW);
  });

  test('a row it left out keeps what it had, and keeps its date', () => {
    const previous: BoardRoutes = { ...routes(['a', 'b']), plannedAt: new Map([['a', NOW - 60_000], ['b', NOW - 90_000]]) };
    const merged = mergeBoardRoutes(previous, routes(['a']), NOW);
    expect([...merged.rows.keys()].sort()).toEqual(['a', 'b']);
    expect(merged.plannedAt?.get('b')).toBe(NOW - 90_000);
    // Which is what the view needs to know to dim it rather than to pretend.
    expect(NOW - (merged.plannedAt?.get('b') ?? 0) < ROUTE_AGEING_MS).toBe(true);
  });

  test('a row the new search answered with nothing keeps the journey it had', () => {
    // The failure the harness caught: the row was still in the answer, so it
    // was not "left out", but the answer was empty. Overwriting on that is the
    // same blank slot by another route.
    const previous: BoardRoutes = { ...routes(['a']), plannedAt: new Map([['a', NOW - 60_000]]) };
    const fresh: BoardRoutes = { rows: new Map([['a', miss('a')]]), origin: null, destinationKey: 'work' };
    const merged = mergeBoardRoutes(previous, fresh, NOW);
    expect(merged.rows.get('a')?.best?.exitStop).toBe('a');
    // Dated when it was found, not when it was last asked about, so it ages.
    expect(merged.plannedAt?.get('a')).toBe(NOW - 60_000);
  });

  test('a row that had nothing takes the new nothing, and its reason', () => {
    const previous: BoardRoutes = { rows: new Map([['a', miss('a')]]), origin: null, destinationKey: 'work' };
    const merged = mergeBoardRoutes(previous, { rows: new Map([['a', miss('a')]]), origin: null, destinationKey: 'work' }, NOW);
    expect(merged.rows.get('a')?.miss).toBe('nothing found');
    expect(merged.plannedAt?.get('a')).toBe(NOW);
  });

  test('the carried journey is shown against the departure as it stands now', () => {
    // The train it belongs to may be running four minutes late since; that is
    // news, and it is the fresh run's news, not the old plan's.
    const previous: BoardRoutes = { ...routes(['a']), plannedAt: new Map([['a', NOW - 60_000]]) };
    const late = miss('a');
    (late.departure as unknown as { delayMin: number }).delayMin = 4;
    const merged = mergeBoardRoutes(previous, { rows: new Map([['a', late]]), origin: null, destinationKey: 'work' }, NOW);
    expect((merged.rows.get('a')?.departure as unknown as { delayMin?: number }).delayMin).toBe(4);
    expect(merged.rows.get('a')?.best?.exitStop).toBe('a');
  });

  test('a row the previous answer never had is simply new', () => {
    const previous: BoardRoutes = { ...routes(['a']), plannedAt: new Map([['a', NOW - 60_000]]) };
    const merged = mergeBoardRoutes(previous, routes(['a', 'c']), NOW);
    expect(merged.plannedAt?.get('c')).toBe(NOW);
  });

  test('nothing is carried across a change of destination', () => {
    // Those journeys answer a different question. A stale truth may be shown;
    // an answer about somewhere else may not.
    const previous: BoardRoutes = { ...routes(['a', 'b'], 'home'), plannedAt: new Map([['a', NOW - 1000]]) };
    const merged = mergeBoardRoutes(previous, routes(['a'], 'work'), NOW);
    expect([...merged.rows.keys()]).toEqual(['a']);
    expect(merged.destinationKey).toBe('work');
  });

  test('a previous answer with no dates at all is still carried, dated as one', () => {
    // Plans written by an earlier version of the page, restored from disk.
    const previous: BoardRoutes = routes(['a', 'b']);
    const merged = mergeBoardRoutes(previous, routes(['a']), NOW);
    expect(merged.rows.has('b')).toBe(true);
    expect(merged.plannedAt?.get('b')).toBe(NOW);
  });
});
