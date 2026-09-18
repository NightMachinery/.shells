import { describe, expect, test } from 'bun:test';
import { applyFilters, catchable, catchableOnBoard, mergeBoards, normaliseLine, walkMinutesFor } from '../src/filter.ts';
import type { BoardConfig, Departure, Direction, Mode } from '../src/model.ts';

const NOW = Date.parse('2026-01-01T08:00:00Z');

function departure(overrides: Partial<Departure> = {}): Departure {
  return {
    line: 'S8',
    mode: 'SBAHN' as Mode,
    destination: 'Synthetic East',
    planned: NOW + 5 * 60_000,
    realtime: NOW + 5 * 60_000,
    delayMin: 0,
    cancelled: false,
    sev: false,
    platform: null,
    direction: 'H' as Direction,
    backend: 'mvg',
    stop: 'de:00000:1',
    realtimeKnown: true,
    ...overrides,
  };
}

function board(overrides: Partial<BoardConfig> = {}): BoardConfig {
  return { title: 'test', stops: ['de:00000:1'], walkMinutes: 0, ...overrides };
}

describe('line comparison', () => {
  test('ignores whitespace and case in both directions', () => {
    expect(normaliseLine('RE 72')).toBe(normaliseLine('RE72'));
    expect(normaliseLine('re72')).toBe(normaliseLine('RE 72'));
  });

  test('a spaced config label matches an unspaced row label and the reverse', () => {
    const spacedConfig = board({ lines: ['RE 72'] });
    const unspacedRow = departure({ line: 'RE72', mode: 'BAHN' });
    expect(applyFilters([unspacedRow], spacedConfig)).toHaveLength(1);

    const unspacedConfig = board({ lines: ['RE72'] });
    const spacedRow = departure({ line: 'RE 72', mode: 'BAHN' });
    expect(applyFilters([spacedRow], unspacedConfig)).toHaveLength(1);
  });
});

describe('filter application', () => {
  const rows = [
    departure({ line: 'S8', mode: 'SBAHN', direction: 'H', destination: 'Synthetic East' }),
    departure({ line: 'S8', mode: 'SBAHN', direction: 'R', destination: 'Synthetic West' }),
    departure({ line: 'U6', mode: 'UBAHN', direction: 'H', destination: 'Synthetic North' }),
    departure({ line: 'RE 72', mode: 'BAHN', direction: null, destination: 'Synthetic West' }),
  ];

  test('a board with no filters shows everything', () => {
    expect(applyFilters(rows, board())).toHaveLength(rows.length);
  });

  test('modes narrow first', () => {
    expect(applyFilters(rows, board({ modes: ['SBAHN'] })).map((row) => row.line)).toEqual(['S8', 'S8']);
  });

  test('every filter in sequence leaves only the intended row', () => {
    const narrow = board({
      modes: ['SBAHN'],
      lines: ['S8'],
      direction: 'H',
      destinations: ['^Synthetic E'],
    });
    const result = applyFilters(rows, narrow);
    expect(result).toHaveLength(1);
    expect(result[0]?.destination).toBe('Synthetic East');
  });

  test('a mode filter applied before a line filter cannot be rescued by the line filter', () => {
    // Order matters: the regional row is removed by `modes` and the later
    // `lines` entry naming it never sees it.
    const result = applyFilters(rows, board({ modes: ['SBAHN'], lines: ['RE 72'] }));
    expect(result).toHaveLength(0);
  });

  test('destination regexes match case-insensitively, any one is enough', () => {
    const result = applyFilters(rows, board({ destinations: ['synthetic north', 'synthetic west$'] }));
    expect(result.map((row) => row.destination).sort()).toEqual(['Synthetic North', 'Synthetic West', 'Synthetic West']);
  });

  test('a direction filter drops unknown directions along with the wrong ones', () => {
    const result = applyFilters(rows, board({ direction: 'H' }));
    expect(result.map((row) => row.line)).toEqual(['S8', 'U6']);
    expect(result.some((row) => row.direction === null)).toBe(false);
  });
});

describe('merging a board', () => {
  test('concatenates and orders by expected departure, stably', () => {
    const early = departure({ stop: 'de:00000:1', realtime: NOW + 60_000 });
    const lateA = departure({ stop: 'de:00000:1', realtime: NOW + 600_000, line: 'A' });
    const lateB = departure({ stop: 'de:00000:2', realtime: NOW + 600_000, line: 'B' });
    const merged = mergeBoards([[lateA], [early, lateB]]);
    expect(merged.map((row) => row.line)).toEqual(['S8', 'A', 'B']);
  });
});

describe('reachability', () => {
  test('no walking time makes everything catchable, including the past', () => {
    expect(catchable(departure({ realtime: NOW - 60_000 }), 0, NOW)).toBe(true);
    expect(catchable(departure({ realtime: NOW + 60_000 }), 0, NOW)).toBe(true);
  });

  test('a walking time excludes anything leaving sooner than the walk', () => {
    const dep = departure({ realtime: NOW + 5 * 60_000 });
    expect(catchable(dep, 4, NOW)).toBe(true);
    expect(catchable(dep, 5, NOW)).toBe(true);
    expect(catchable(dep, 6, NOW)).toBe(false);
  });
});

describe('per-stop walking times', () => {
  const near = 'de:00000:1';
  const far = 'de:00000:2';
  const merged = {
    walkMinutes: 2,
    walkMinutesByStop: { [near]: 1, [far]: 9 },
  };

  test('falls back to the board figure for a stop with no override', () => {
    expect(walkMinutesFor({ walkMinutes: 4 }, near)).toBe(4);
    expect(walkMinutesFor({ walkMinutes: 4, walkMinutesByStop: { [far]: 9 } }, near)).toBe(4);
    expect(walkMinutesFor(merged, far)).toBe(9);
  });

  test('two stops on one board are judged differently at the same instant', () => {
    const when = NOW + 5 * 60_000;
    const fromNear = departure({ stop: near, realtime: when });
    const fromFar = departure({ stop: far, realtime: when });
    expect(catchableOnBoard(fromNear, merged, NOW)).toBe(true);
    expect(catchableOnBoard(fromFar, merged, NOW)).toBe(false);
  });
});
