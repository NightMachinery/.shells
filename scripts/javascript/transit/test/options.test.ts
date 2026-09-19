import { beforeEach, describe, expect, test } from 'bun:test';
import { clearPlanCache, planBoard, transitLegs, type PlanTarget } from '../src/plan.ts';
import { clearOriginCache } from '../src/origin.ts';
import type { Departure } from '../src/model.ts';
import { mockFetch } from './helpers.ts';

// The reported bug: a row's alternatives could be later rides of the very
// route already recommended, same exit stop, same onward line, only later.
// That is the next row on the board, not a choice being offered for this one.
// This file exercises the fix directly through `planBoard`, the same way
// `plan.test.ts` does, because the dedup lives inside the recombination that
// function drives and a fixture of raw itineraries is what exercises it
// honestly, rather than reaching into `plan.ts`'s private helpers.

const BASE = 'https://example.invalid/api';
const HOME = 'de:00000:41';
const EXIT_SAME = 'de:00000:42';
const EXIT_OTHER = 'de:00000:43';

const BOARD_DEPARTURE = '2026-01-01T23:10:00Z';
const TO_EXIT_SAME_ARRIVAL = '2026-01-01T23:25:00Z';
const TO_EXIT_OTHER_ARRIVAL = '2026-01-01T23:22:00Z';

const DESTINATION: PlanTarget[] = [{ place: { lat: 0, lon: 0 }, name: 'Maxmonument', walkMinutes: null }];

function row(): Departure {
  return {
    line: 'S1',
    mode: 'SBAHN',
    destination: 'Maxmonument',
    planned: Date.parse(BOARD_DEPARTURE),
    realtime: Date.parse(BOARD_DEPARTURE),
    delayMin: 0,
    cancelled: false,
    sev: false,
    platform: null,
    direction: 'H',
    backend: 'synthetic',
    stop: HOME,
    realtimeKnown: true,
  };
}

/**
 * The leg every itinerary below opens with: line S1, leaving the board's stop
 * at the row's own minute. It is the same first leg in every case, exactly
 * like the reported row, whose options were all later rides reachable from
 * that one departure.
 */
function firstLeg(tripId: string, toStop: string, toName: string, toTime: string) {
  return {
    mode: 'METRO',
    tripId,
    routeShortName: 'S1',
    agencyName: 'Synthetic Rail',
    startTime: BOARD_DEPARTURE,
    endTime: toTime,
    from: { name: 'Heimplatz', stopId: HOME },
    to: { name: toName, stopId: toStop },
  };
}

function onwardLeg(tripId: string, line: string, fromStop: string, fromName: string, departure: string, arrival: string) {
  return {
    mode: 'METRO',
    tripId,
    routeShortName: line,
    agencyName: 'Synthetic Rail',
    startTime: departure,
    endTime: arrival,
    from: { name: fromName, stopId: fromStop },
    to: { name: 'Maxmonument', stopId: 'de:00000:99' },
  };
}

/**
 * Three itineraries riding the same S1 to Umstiegspunkt and changing there
 * onto the same onward line S8, at 23:32, 23:52 and 00:12: the shape of the
 * Westkreuz report, a repeat masquerading as three alternatives. A fourth
 * itinerary changes at a different stop, Nordweg, onto a different line, U6:
 * a genuinely different route that must survive.
 */
function itineraries() {
  return [
    {
      startTime: BOARD_DEPARTURE,
      endTime: '2026-01-01T23:47:00Z',
      legs: [
        firstLeg('trip-1a', EXIT_SAME, 'Umstiegspunkt', TO_EXIT_SAME_ARRIVAL),
        onwardLeg('trip-1b', 'S8', EXIT_SAME, 'Umstiegspunkt', '2026-01-01T23:32:00Z', '2026-01-01T23:47:00Z'),
      ],
    },
    {
      startTime: BOARD_DEPARTURE,
      endTime: '2026-01-02T00:07:00Z',
      legs: [
        firstLeg('trip-2a', EXIT_SAME, 'Umstiegspunkt', TO_EXIT_SAME_ARRIVAL),
        onwardLeg('trip-2b', 'S8', EXIT_SAME, 'Umstiegspunkt', '2026-01-01T23:52:00Z', '2026-01-02T00:07:00Z'),
      ],
    },
    {
      startTime: BOARD_DEPARTURE,
      endTime: '2026-01-02T00:27:00Z',
      legs: [
        firstLeg('trip-3a', EXIT_SAME, 'Umstiegspunkt', TO_EXIT_SAME_ARRIVAL),
        onwardLeg('trip-3b', 'S8', EXIT_SAME, 'Umstiegspunkt', '2026-01-02T00:12:00Z', '2026-01-02T00:27:00Z'),
      ],
    },
    {
      startTime: BOARD_DEPARTURE,
      endTime: '2026-01-01T23:45:00Z',
      legs: [
        firstLeg('trip-4a', EXIT_OTHER, 'Nordweg', TO_EXIT_OTHER_ARRIVAL),
        onwardLeg('trip-4b', 'U6', EXIT_OTHER, 'Nordweg', '2026-01-01T23:30:00Z', '2026-01-01T23:45:00Z'),
      ],
    },
  ];
}

function planMock() {
  const { fetchImpl, urls } = mockFetch((url) => {
    if (url.includes('/stoptimes')) return { stopTimes: [{}] };
    return { itineraries: itineraries(), nextPageCursor: '' };
  });
  return { fetchImpl, urls };
}

beforeEach(() => {
  clearPlanCache();
  clearOriginCache();
});

describe('deduplicating a row by route', () => {
  test('a later ride of the same line from the same exit is not a separate option', async () => {
    const { fetchImpl } = planMock();
    const planned = await planBoard({
      stop: HOME,
      targets: DESTINATION,
      rows: [row()],
      startMs: Date.parse('2026-01-01T23:00:00Z'),
      baseUrl: BASE,
      fetchImpl,
    });

    const options = planned[0]?.options ?? [];
    expect(options).toHaveLength(2);

    const sameExit = options.find((option) => option.exitStop === EXIT_SAME);
    expect(sameExit).toBeDefined();
    // The 23:32 change is the one that arrives earliest of the three; the
    // 23:52 and 00:12 rides of the same line are repeats of it and must not
    // survive as options of their own.
    expect(sameExit?.arrival).toBe(Date.parse('2026-01-01T23:47:00Z'));
    expect(transitLegs(sameExit as NonNullable<typeof sameExit>).map((leg) => leg.line)).toEqual(['S1', 'S8']);

    const otherExit = options.find((option) => option.exitStop === EXIT_OTHER);
    expect(otherExit).toBeDefined();
    expect(otherExit?.arrival).toBe(Date.parse('2026-01-01T23:45:00Z'));
    expect(transitLegs(otherExit as NonNullable<typeof otherExit>).map((leg) => leg.line)).toEqual(['S1', 'U6']);
  });

  test('two different onward lines from the same exit are both kept', async () => {
    // Same exit as the group above, but this time the second itinerary changes
    // onto a different line there instead of a later run of the same one:
    // that is a genuinely different route and must not be collapsed away.
    const { fetchImpl } = mockFetch((url) => {
      if (url.includes('/stoptimes')) return { stopTimes: [{}] };
      return {
        itineraries: [
          {
            startTime: BOARD_DEPARTURE,
            endTime: '2026-01-01T23:47:00Z',
            legs: [
              firstLeg('trip-a', EXIT_SAME, 'Umstiegspunkt', TO_EXIT_SAME_ARRIVAL),
              onwardLeg('trip-a2', 'S8', EXIT_SAME, 'Umstiegspunkt', '2026-01-01T23:32:00Z', '2026-01-01T23:47:00Z'),
            ],
          },
          {
            startTime: BOARD_DEPARTURE,
            endTime: '2026-01-01T23:50:00Z',
            legs: [
              firstLeg('trip-b', EXIT_SAME, 'Umstiegspunkt', TO_EXIT_SAME_ARRIVAL),
              onwardLeg('trip-b2', 'U9', EXIT_SAME, 'Umstiegspunkt', '2026-01-01T23:35:00Z', '2026-01-01T23:50:00Z'),
            ],
          },
        ],
        nextPageCursor: '',
      };
    });

    const planned = await planBoard({
      stop: HOME,
      targets: DESTINATION,
      rows: [row()],
      startMs: Date.parse('2026-01-01T23:00:00Z'),
      baseUrl: BASE,
      fetchImpl,
    });

    const options = planned[0]?.options ?? [];
    const lines = options.map((option) => transitLegs(option).map((leg) => leg.line).join('>'));
    expect(new Set(lines).size).toBe(2);
    expect(lines).toContain('S1>S8');
    expect(lines).toContain('S1>U9');
  });

  test('a comfortable ride represents its route even when a tight one lands earlier', async () => {
    // Both rides are the same route: same exit, same onward line. The earlier
    // one leaves two minutes before the change is comfortably makeable, so it is
    // a gamble, and a gamble is never recommended. Electing it to stand for the
    // route on the strength of its arrival time would leave the row with an
    // option it can show and no journey it can recommend, while the ride that
    // could be recommended sat unshown behind it.
    const { fetchImpl } = mockFetch((url) => {
      if (url.includes('/stoptimes')) return { stopTimes: [{}] };
      return {
        itineraries: [
          {
            startTime: BOARD_DEPARTURE,
            endTime: '2026-01-01T23:38:00Z',
            legs: [
              firstLeg('trip-tight', EXIT_SAME, 'Umstiegspunkt', TO_EXIT_SAME_ARRIVAL),
              onwardLeg('trip-tight2', 'S8', EXIT_SAME, 'Umstiegspunkt', '2026-01-01T23:23:00Z', '2026-01-01T23:38:00Z'),
            ],
          },
          {
            startTime: BOARD_DEPARTURE,
            endTime: '2026-01-01T23:47:00Z',
            legs: [
              firstLeg('trip-easy', EXIT_SAME, 'Umstiegspunkt', TO_EXIT_SAME_ARRIVAL),
              onwardLeg('trip-easy2', 'S8', EXIT_SAME, 'Umstiegspunkt', '2026-01-01T23:32:00Z', '2026-01-01T23:47:00Z'),
            ],
          },
        ],
        nextPageCursor: '',
      };
    });

    const planned = await planBoard({
      stop: HOME,
      targets: DESTINATION,
      rows: [row()],
      startMs: Date.parse('2026-01-01T23:00:00Z'),
      baseUrl: BASE,
      fetchImpl,
    });

    const options = planned[0]?.options ?? [];
    expect(options).toHaveLength(1);
    expect(options[0]?.tight).toBe(false);
    expect(options[0]?.arrival).toBe(Date.parse('2026-01-01T23:47:00Z'));
    expect(planned[0]?.best?.arrival).toBe(Date.parse('2026-01-01T23:47:00Z'));
  });
});
