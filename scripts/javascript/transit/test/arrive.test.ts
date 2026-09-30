import { describe, expect, test } from 'bun:test';
import { ARRIVE_REACH_MINUTES, arriveLabel, attachArrivals, attachArrivalsThrough, byArrival } from '../src/arrive.ts';
import type { ConnectionConfig, Departure } from '../src/model.ts';

const T0 = Date.parse('2026-01-01T08:00:00Z');
const MIN = 60_000;

function dep(overrides: Partial<Departure> & { at: number }): Departure {
  const { at, ...rest } = overrides;
  return {
    line: '16',
    mode: 'TRAM',
    destination: 'Synthetic East',
    planned: at,
    realtime: at,
    delayMin: 0,
    cancelled: false,
    sev: false,
    platform: null,
    direction: 'H',
    backend: 'mvg',
    stop: 'de:00000:1',
    realtimeKnown: true,
    ...rest,
  };
}

describe('arrivals at a far stop', () => {
  test('finds the same run at the far stop and takes its live time', () => {
    const rows = [
      dep({ at: T0, runId: 'mvg:1', stop: 'de:00000:1' }),
      dep({ at: T0 + 2 * MIN, runId: 'mvg:1', stop: 'de:00000:2' }),
      // Not live here either, so there is no delay to carry to the far stop.
      dep({ at: T0 + 5 * MIN, runId: 'mvg:2', stop: 'de:00000:1', realtimeKnown: false }),
    ];
    const far = [
      dep({ at: T0 + 20 * MIN, runId: 'mvg:1', stop: 'de:00000:9', realtime: T0 + 22 * MIN, realtimeKnown: true }),
      dep({ at: T0 + 25 * MIN, runId: 'mvg:2', stop: 'de:00000:9', realtimeKnown: false }),
    ];
    attachArrivals(rows, { far }, { lines: ['16'], direction: 'H' });
    expect(rows[0]?.arrival).toEqual({ at: T0 + 22 * MIN, realtimeKnown: true });
    // The same vehicle from a later stop reaches the far one at the same time.
    expect(rows[1]?.arrival).toEqual({ at: T0 + 22 * MIN, realtimeKnown: true });
    expect(rows[2]?.arrival).toEqual({ at: T0 + 25 * MIN, realtimeKnown: false });
  });

  test('a run that is not at the far stop gets null, not a neighbour', () => {
    const rows = [dep({ at: T0, runId: 'mvg:1' })];
    attachArrivals(rows, { far: [dep({ at: T0 + 20 * MIN, runId: 'mvg:7' })] }, {});
    expect(rows[0]?.arrival).toBeNull();
  });

  test('a row with no run identifier gets null', () => {
    const rows = [dep({ at: T0 })];
    attachArrivals(rows, { far: [dep({ at: T0 + 20 * MIN, runId: 'mvg:1' })] }, {});
    expect(rows[0]?.arrival).toBeNull();
  });

  test('identifiers from two backends never match', () => {
    const rows = [dep({ at: T0, runId: 'mvg:1' })];
    attachArrivals(rows, { far: [dep({ at: T0 + 20 * MIN, runId: 'transitous:1', backend: 'transitous' })] }, {});
    expect(rows[0]?.arrival).toBeNull();
  });

  test('the far row must be later than this one and within reach', () => {
    const rows = [dep({ at: T0, runId: 'mvg:1' })];
    const far = [
      dep({ at: T0 - 5 * MIN, runId: 'mvg:1' }),
      dep({ at: T0 + (ARRIVE_REACH_MINUTES + 1) * MIN, runId: 'mvg:1' }),
    ];
    attachArrivals(rows, { far }, {});
    expect(rows[0]?.arrival).toBeNull();
  });

  test('a reused identifier takes the earliest far row after this one', () => {
    const rows = [dep({ at: T0, runId: 'mvg:1' })];
    const far = [dep({ at: T0 + 40 * MIN, runId: 'mvg:1' }), dep({ at: T0 + 20 * MIN, runId: 'mvg:1' })];
    attachArrivals(rows, { far }, {});
    expect(rows[0]?.arrival?.at).toBe(T0 + 20 * MIN);
  });

  test('matches on scheduled times, so a delay cannot push a row out of its window', () => {
    // Late enough at the far stop that its expected time is past the reach;
    // its scheduled time is not, and it is still the same vehicle.
    const rows = [dep({ at: T0, runId: 'mvg:1' })];
    const late = T0 + (ARRIVE_REACH_MINUTES + 10) * MIN;
    attachArrivals(rows, { far: [dep({ at: T0 + 20 * MIN, realtime: late, runId: 'mvg:1' })] }, {});
    expect(rows[0]?.arrival?.at).toBe(late);
  });

  test('the far stop is read through the board line and letter filters', () => {
    const rows = [dep({ at: T0, runId: 'mvg:1' })];
    const far = [
      dep({ at: T0 + 10 * MIN, runId: 'mvg:1', line: '17' }),
      dep({ at: T0 + 12 * MIN, runId: 'mvg:1', direction: 'R' }),
      dep({ at: T0 + 14 * MIN, runId: 'mvg:1', cancelled: true }),
      dep({ at: T0 + 20 * MIN, runId: 'mvg:1' }),
    ];
    attachArrivals(rows, { far }, { lines: ['16'], direction: 'H' });
    expect(rows[0]?.arrival?.at).toBe(T0 + 20 * MIN);
  });
});

describe('arrivals found through the timetable', () => {
  // The live feed names runs only for the next half hour or so, so a row whose
  // vehicle is further from the far stop than that has no run id there.
  const board = { lines: ['16'], direction: 'H' as const };

  test('boarding row to timetable run to far timetable minute to the live row there', () => {
    const rows = [dep({ at: T0, delayMin: 1, realtime: T0 + MIN, stop: 'de:00000:1' })];
    const timetableAt = new Map([
      [
        'de:00000:1',
        [
          dep({ at: T0 - 10 * MIN, tripId: 'trip-earlier', backend: 'transitous' }),
          dep({ at: T0, tripId: 'trip-a', backend: 'transitous' }),
          dep({ at: T0, tripId: 'trip-other-line', line: '17', backend: 'transitous' }),
        ],
      ],
    ]);
    const farTimetable = [
      dep({ at: T0 + 10 * MIN, tripId: 'trip-earlier', stop: 'de:00000:9', backend: 'transitous' }),
      dep({ at: T0 + 20 * MIN, tripId: 'trip-a', stop: 'de:00000:9', backend: 'transitous' }),
    ];
    const far = [
      // The earlier tram, running ten minutes late: its expected minute is the
      // right tram's timetabled one, and it must not be taken for it.
      dep({ at: T0 + 10 * MIN, realtime: T0 + 20 * MIN, stop: 'de:00000:9' }),
      dep({ at: T0 + 20 * MIN, realtime: T0 + 23 * MIN, stop: 'de:00000:9' }),
    ];
    attachArrivals(rows, { far, farTimetable, timetableAt }, board);
    expect(rows[0]?.arrival).toEqual({ at: T0 + 23 * MIN, realtimeKnown: true });
  });

  test('with no live row at the far stop the timetable time stands, marked as such', () => {
    const rows = [dep({ at: T0, realtimeKnown: false })];
    const timetableAt = new Map([['de:00000:1', [dep({ at: T0, tripId: 'trip-a', backend: 'transitous' })]]]);
    const farTimetable = [dep({ at: T0 + 20 * MIN, tripId: 'trip-a', realtimeKnown: false, backend: 'transitous' })];
    attachArrivals(rows, { far: [], farTimetable, timetableAt }, board);
    expect(rows[0]?.arrival).toEqual({ at: T0 + 20 * MIN, realtimeKnown: false });
  });

  test('a row that already carries its aggregator run skips the boarding-stop match', () => {
    const rows = [dep({ at: T0, tripId: 'trip-a', backend: 'transitous' })];
    const farTimetable = [dep({ at: T0 + 20 * MIN, tripId: 'trip-a', backend: 'transitous' })];
    attachArrivals(rows, { far: [], farTimetable }, board);
    expect(rows[0]?.arrival?.at).toBe(T0 + 20 * MIN);
  });

  test('a line crossing itself at the boarding minute is split by its letter', () => {
    const rows = [dep({ at: T0, direction: 'H' })];
    const timetableAt = new Map([
      [
        'de:00000:1',
        [
          dep({ at: T0, tripId: 'trip-back', direction: 'R', destination: 'Synthetic West', backend: 'transitous' }),
          dep({ at: T0, tripId: 'trip-a', direction: 'H', backend: 'transitous' }),
        ],
      ],
    ]);
    const farTimetable = [dep({ at: T0 + 20 * MIN, tripId: 'trip-a', backend: 'transitous' })];
    attachArrivals(rows, { far: [], farTimetable, timetableAt }, board);
    expect(rows[0]?.arrival?.at).toBe(T0 + 20 * MIN);
  });

  test('two timetable runs at the boarding minute is no answer', () => {
    const rows = [dep({ at: T0 })];
    const timetableAt = new Map([
      [
        'de:00000:1',
        [dep({ at: T0, tripId: 'trip-a', backend: 'transitous' }), dep({ at: T0, tripId: 'trip-b', backend: 'transitous' })],
      ],
    ]);
    const farTimetable = [
      dep({ at: T0 + 20 * MIN, tripId: 'trip-a', backend: 'transitous' }),
      dep({ at: T0 + 21 * MIN, tripId: 'trip-b', backend: 'transitous' }),
    ];
    attachArrivals(rows, { far: [], farTimetable, timetableAt }, board);
    expect(rows[0]?.arrival).toBeNull();
  });

  test('the live run id wins over the timetable when both answer', () => {
    const rows = [dep({ at: T0, runId: 'mvg:1' })];
    const timetableAt = new Map([['de:00000:1', [dep({ at: T0, tripId: 'trip-a', backend: 'transitous' })]]]);
    const farTimetable = [dep({ at: T0 + 20 * MIN, tripId: 'trip-a', backend: 'transitous' })];
    const far = [dep({ at: T0 + 19 * MIN, runId: 'mvg:1' })];
    attachArrivals(rows, { far, farTimetable, timetableAt }, board);
    expect(rows[0]?.arrival?.at).toBe(T0 + 19 * MIN);
  });
});

describe('an arrival estimated from the delay at the boarding stop', () => {
  test('a far stop with no live figure yet gets its timetable moved by the live delay here', () => {
    const rows = [dep({ at: T0, realtime: T0 + 4 * MIN, delayMin: 4, runId: 'mvg:1' })];
    attachArrivals(rows, { far: [dep({ at: T0 + 20 * MIN, runId: 'mvg:1', realtimeKnown: false })] }, {});
    expect(rows[0]?.arrival).toEqual({ at: T0 + 24 * MIN, realtimeKnown: false, estimated: true });
  });

  test('on time and live here is an estimate too: the timetable, now with evidence', () => {
    const rows = [dep({ at: T0, runId: 'mvg:1' })];
    attachArrivals(rows, { far: [dep({ at: T0 + 20 * MIN, runId: 'mvg:1', realtimeKnown: false })] }, {});
    expect(rows[0]?.arrival).toEqual({ at: T0 + 20 * MIN, realtimeKnown: false, estimated: true });
  });

  test('a live figure at the far stop wins over the estimate', () => {
    const rows = [dep({ at: T0, realtime: T0 + 4 * MIN, runId: 'mvg:1' })];
    attachArrivals(rows, { far: [dep({ at: T0 + 20 * MIN, realtime: T0 + 22 * MIN, runId: 'mvg:1' })] }, {});
    expect(rows[0]?.arrival).toEqual({ at: T0 + 22 * MIN, realtimeKnown: true });
  });

  test('the timetable path is estimated the same way', () => {
    const rows = [dep({ at: T0, realtime: T0 + 3 * MIN })];
    const timetableAt = new Map([['de:00000:1', [dep({ at: T0, tripId: 'trip-a', backend: 'transitous' })]]]);
    const farTimetable = [dep({ at: T0 + 20 * MIN, tripId: 'trip-a', realtimeKnown: false, backend: 'transitous' })];
    attachArrivals(rows, { far: [], farTimetable, timetableAt }, { lines: ['16'] });
    expect(rows[0]?.arrival).toEqual({ at: T0 + 23 * MIN, realtimeKnown: false, estimated: true });
  });
});

describe('arrivals through a connection', () => {
  const connection: ConnectionConfig = { stop: 'de:00000:5', lines: ['16'], direction: 'H', rideMinutes: 3, transferMinutes: 4 };

  test('each row takes the arrival of the onward departure it would catch', () => {
    const rows = [
      dep({ at: T0, line: 'U4', mode: 'UBAHN' }),
      dep({ at: T0 + 5 * MIN, line: 'U5', mode: 'UBAHN' }),
    ];
    const onward = [
      // Leaves before the first row could be there: 3 on board and 4 to walk.
      dep({ at: T0 + 6 * MIN, stop: 'de:00000:5', arrival: { at: T0 + 10 * MIN, realtimeKnown: true } }),
      dep({ at: T0 + 9 * MIN, stop: 'de:00000:5', arrival: { at: T0 + 13 * MIN, realtimeKnown: true } }),
      dep({ at: T0 + 19 * MIN, stop: 'de:00000:5', arrival: { at: T0 + 23 * MIN, realtimeKnown: false, estimated: true } }),
      // Right line, other way: never the one caught.
      dep({ at: T0 + 12 * MIN, stop: 'de:00000:5', direction: 'R', arrival: { at: T0 + 16 * MIN, realtimeKnown: true } }),
    ];
    attachArrivalsThrough(rows, onward, connection);
    expect(rows[0]?.arrival).toEqual({ at: T0 + 13 * MIN, realtimeKnown: true });
    expect(rows[1]?.arrival).toEqual({ at: T0 + 23 * MIN, realtimeKnown: false, estimated: true });
  });

  test('nothing catchable, or a catchable one with no arrival, is a dash', () => {
    const rows = [dep({ at: T0, line: 'U4' }), dep({ at: T0 + 30 * MIN, line: 'U4' })];
    const onward = [dep({ at: T0 + 10 * MIN, stop: 'de:00000:5', arrival: null })];
    attachArrivalsThrough(rows, onward, connection);
    expect(rows[0]?.arrival).toBeNull();
    expect(rows[1]?.arrival).toBeNull();
  });
});

describe('arrival order', () => {
  test('soonest arrival first, one vehicle rows together in departure order', () => {
    const a = dep({ at: T0, arrival: { at: T0 + 20 * MIN, realtimeKnown: true } });
    const b = dep({ at: T0 + 2 * MIN, arrival: { at: T0 + 20 * MIN, realtimeKnown: true } });
    const c = dep({ at: T0 + 5 * MIN, arrival: { at: T0 + 30 * MIN, realtimeKnown: true } });
    const d = dep({ at: T0 + 12 * MIN, arrival: { at: T0 + 20 * MIN, realtimeKnown: true } });
    expect(byArrival([c, a, d, b])).toEqual([a, b, d, c]);
  });

  test('a row with no arrival sorts by its own departure', () => {
    const known = dep({ at: T0, arrival: { at: T0 + 20 * MIN, realtimeKnown: true } });
    const unknown = dep({ at: T0 + 10 * MIN, arrival: null });
    expect(byArrival([known, unknown])).toEqual([unknown, known]);
  });
});

describe('the far stop name', () => {
  test('its label, else the id last field', () => {
    expect(arriveLabel({ stop: 'de:00000:9', label: 'Far' })).toBe('Far');
    expect(arriveLabel({ stop: 'de:00000:9' })).toBe('9');
  });
});
