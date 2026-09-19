import { describe, expect, test } from 'bun:test';
import { decodeRoutes, encodeRoutes, profileSearch, readProfileQuery, type ProfileQuery } from '../src/page/wire.ts';
import { carryBoards, type BoardRoutes, type ProfileRoutes } from '../src/page/commute.ts';
import type { PlannedRow } from '../src/plan.ts';

// The wire format is the one place a server and a page agree on what a plan
// looks like, and the only thing worth testing about it is that nothing is
// lost crossing it: a `Map` goes out as pairs and comes back a `Map`, a board
// the server did not answer for comes back as "no journeys" rather than as an
// empty one (which is what lets `carryBoards` keep the previous answer for
// it), and the query string that asks for an answer is read back the same
// way it was written.

const NOW = 1_700_000_000_000;
const DEST = 'de:00000:9';

function row(id: string): PlannedRow {
  return { departure: { line: id } as never, options: [], best: { exitStop: id } as never };
}

function board(keys: string[], destinationKey = DEST): BoardRoutes {
  return { rows: new Map(keys.map((key) => [key, row(key)])), origin: null, destinationKey };
}

function profileRoutes(boards: Map<number, BoardRoutes>): ProfileRoutes {
  return { boards, at: NOW, destinationKey: DEST, stale: false, key: 'k' };
}

describe('encodeRoutes', () => {
  test('nothing planned yet encodes to null, not an empty list', () => {
    expect(encodeRoutes(null, new Map([[0, DEST]]))).toBeNull();
  });

  test('one entry per wanted board, in the order asked for, whether or not the routes answer for it', () => {
    const routes = profileRoutes(new Map([[0, board(['a', 'b'])]]));
    // Board 1 is on the wanted list with no matching board in `routes`, and
    // board 0 has one; asking for 1 before 0 also proves the output follows
    // `wanted`'s own order rather than the routes map's.
    const wanted = new Map([
      [1, DEST],
      [0, DEST],
    ]);

    const wire = encodeRoutes(routes, wanted);

    expect(wire?.map((entry) => entry.index)).toEqual([1, 0]);
    const unanswered = wire?.[0];
    expect(unanswered?.answered).toBe(false);
    expect(unanswered?.rows).toEqual([]);
    const answered = wire?.[1];
    expect(answered?.answered).toBe(true);
    expect(answered?.rows).toEqual([...board(['a', 'b']).rows]);
  });
});

describe('decodeRoutes', () => {
  test('a round trip through encodeRoutes gives back real Maps holding the same data', () => {
    const routes = profileRoutes(new Map([[0, board(['a', 'b'])]]));
    const wanted = new Map([
      [0, DEST],
      [1, DEST],
    ]);

    const decoded = decodeRoutes(encodeRoutes(routes, wanted));

    expect(decoded.wanted).toEqual(wanted);
    const answered = decoded.results.find((result) => result.index === 0);
    expect(answered?.routes?.rows).toBeInstanceOf(Map);
    expect(answered?.routes?.rows.get('a')?.best?.exitStop).toBe('a');
    expect(answered?.routes?.rows.get('b')?.best?.exitStop).toBe('b');
  });

  test('no wire answer at all decodes to an empty answer, not a thrown error', () => {
    const decoded = decodeRoutes(null);
    expect(decoded.results).toEqual([]);
    expect(decoded.wanted).toEqual(new Map());
  });

  test('a board the wire says was not answered comes back with null routes, not empty ones', () => {
    const decoded = decodeRoutes([{ index: 0, destinationKey: DEST, answered: false, origin: null, rows: [] }]);
    expect(decoded.results).toEqual([{ index: 0, routes: null }]);
  });

  test('carryBoards keeps the previous journey for a board the wire says it did not answer', () => {
    // This is the whole point of `{ index, routes: null }` rather than an
    // empty `BoardRoutes`: it has to be indistinguishable, to `carryBoards`,
    // from a board the page itself failed to plan.
    const previous: ProfileRoutes = profileRoutes(new Map([[0, board(['a'])]]));
    const wanted = new Map([[0, DEST]]);
    const wire = [{ index: 0, destinationKey: DEST, answered: false, origin: null, rows: [] }];

    const { results } = decodeRoutes(wire);
    const merged = carryBoards(previous, wanted, results, NOW);

    expect(merged.get(0)?.rows.get('a')?.best?.exitStop).toBe('a');
  });
});

describe('profileSearch', () => {
  const query: ProfileQuery = { profileKey: 'home', horizonMinutes: 60, startMs: NOW, destinationKey: null };

  test('always carries the horizon and the start instant', () => {
    const params = new URLSearchParams(profileSearch(query));
    expect(params.get('horizon')).toBe('60');
    expect(params.get('start')).toBe(String(NOW));
  });

  test('leaves out a destination, walk weight and early buffer that were never set', () => {
    const params = new URLSearchParams(profileSearch(query));
    expect(params.has('to')).toBe(false);
    expect(params.has('walk_weight')).toBe(false);
    expect(params.has('early_buffer')).toBe(false);
  });

  test('carries a destination, walk weight and early buffer once they are set', () => {
    const params = new URLSearchParams(
      profileSearch({ ...query, destinationKey: DEST, walkWeight: 1.5, earlyBufferMinutes: 3 }),
    );
    expect(params.get('to')).toBe(DEST);
    expect(params.get('walk_weight')).toBe('1.5');
    expect(params.get('early_buffer')).toBe('3');
  });

  test('is stable: asking the same question twice writes the same string', () => {
    expect(profileSearch(query)).toBe(profileSearch({ ...query }));
  });
});

describe('readProfileQuery', () => {
  const DEFAULT_HORIZON = 90;

  test('round-trips a query with no destination', () => {
    const query: ProfileQuery = { profileKey: 'home', horizonMinutes: 45, startMs: NOW, destinationKey: null };
    const params = new URLSearchParams(profileSearch(query));

    const read = readProfileQuery('home', params, NOW, DEFAULT_HORIZON);

    expect(read.profileKey).toBe('home');
    expect(read.horizonMinutes).toBe(45);
    expect(read.startMs).toBe(NOW);
    expect(read.destinationKey).toBeNull();
    expect(read.walkWeight).toBeUndefined();
    expect(read.earlyBufferMinutes).toBeUndefined();
  });

  test('round-trips a query with a destination and the optional knobs set', () => {
    const query: ProfileQuery = {
      profileKey: 'home',
      horizonMinutes: 45,
      startMs: NOW,
      destinationKey: DEST,
      walkWeight: 2,
      earlyBufferMinutes: 5,
    };
    const params = new URLSearchParams(profileSearch(query));

    const read = readProfileQuery('home', params, NOW, DEFAULT_HORIZON);

    expect(read.destinationKey).toBe(DEST);
    expect(read.walkWeight).toBe(2);
    expect(read.earlyBufferMinutes).toBe(5);
  });

  test('falls back to the passed now and default horizon when the parameters are simply absent', () => {
    const read = readProfileQuery('home', new URLSearchParams(), NOW, DEFAULT_HORIZON);
    expect(read.horizonMinutes).toBe(DEFAULT_HORIZON);
    expect(read.startMs).toBe(NOW);
    expect(read.destinationKey).toBeNull();
  });

  test('falls back the same way when a parameter is present but will not parse as a number', () => {
    const params = new URLSearchParams({ horizon: 'abc', start: 'xyz' });
    const read = readProfileQuery('home', params, NOW, DEFAULT_HORIZON);
    expect(read.horizonMinutes).toBe(DEFAULT_HORIZON);
    expect(read.startMs).toBe(NOW);
  });
});
