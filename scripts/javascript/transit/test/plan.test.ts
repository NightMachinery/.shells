import { beforeEach, describe, expect, test } from 'bun:test';
import {
  clearPlanCache,
  planBoard,
  PLAN_MAX_PAGES,
  sameStopArea,
  WALK_METRES_PER_MINUTE,
  type RouteOption,
} from '../src/plan.ts';
import type { Departure } from '../src/model.ts';
import { FIXTURE_NOW, fixture, mockFetch } from './helpers.ts';

const BASE = 'https://example.invalid/api';

/** The board's parent stop; every identifier in this file is made up. */
const HOME = 'de:00000:1';
const DESTINATION = { lat: 0, lon: 0 };

/** Minutes past the instant the fixtures are written around. */
function at(minutes: number): number {
  return FIXTURE_NOW + minutes * 60_000;
}

function row(line: string, minutes: number): Departure {
  return {
    line,
    mode: 'UBAHN',
    destination: 'Somewhere',
    planned: at(minutes),
    realtime: at(minutes),
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
 * The world the fixture describes, as the tests read it.
 *
 * One board row leaves at 08:10 on U1 and reaches the interchange at 08:20. The
 * walk to the onward platform is 140 metres, which the planner itself bills at
 * eight minutes and this package bills at two. Three X9 departures leave that
 * platform, at 08:21, 08:23 and 08:28, arriving at 08:38, 08:40 and 08:45.
 */
const X9_TIGHT_ARRIVAL = at(38);
const X9_BEST_ARRIVAL = at(40);
const X9_SLOW_ARRIVAL = at(45);

async function planFixtureBoard(rows: Departure[], earlyBufferMinutes?: number) {
  const body = await fixture<unknown>('transitous-plan.json');
  // One page of substance, then nothing: the fixture carries a cursor, and a
  // second page of the same itineraries would only re-assert the first.
  const { fetchImpl, urls } = mockFetch((_url, call) => (call === 0 ? body : { itineraries: [], nextPageCursor: '' }));
  const planned = await planBoard({
    stop: HOME,
    destination: DESTINATION,
    rows,
    startMs: FIXTURE_NOW,
    baseUrl: BASE,
    fetchImpl,
    ...(earlyBufferMinutes === undefined ? {} : { earlyBufferMinutes }),
  });
  return { planned, urls };
}

beforeEach(() => {
  // The cache is keyed on origin, destination and start minute, all of which
  // these cases share; without this one case would answer another.
  clearPlanCache();
});

describe('stop identity', () => {
  test('a parent claims its own platforms and nothing that merely starts the same', () => {
    expect(sameStopArea('de:00000:1:51:51', HOME)).toBe(true);
    expect(sameStopArea(`de-DELFI_${HOME}`, HOME)).toBe(true);
    expect(sameStopArea(HOME, 'de:00000:1:51:51')).toBe(true);
    expect(sameStopArea('de:00000:10:1:1', HOME)).toBe(false);
    expect(sameStopArea('de:00000:7:1:1', HOME)).toBe(false);
  });
});

describe('matching itineraries to rows', () => {
  test('matches on the line and the departure minute', async () => {
    const { planned } = await planFixtureBoard([row('U1', 10), row('U2', 5), row('U1', 40)]);
    expect(planned).toHaveLength(3);
    expect(planned[0]?.options.length).toBeGreaterThan(0);
    expect(planned[1]?.options.length).toBeGreaterThan(0);
    // The same line, twenty-five minutes later than anything the planner
    // returned: no itinerary begins with it.
    expect(planned[2]?.options).toEqual([]);
    expect(planned[2]?.best).toBeNull();
  });

  test('a delayed row matches on its timetabled minute', async () => {
    // The board and the planner carry separate live feeds. A row the board
    // believes is two minutes late still belongs to the itinerary leaving at
    // its timetabled minute, and matching on the expected minute alone would
    // silently empty every delayed row.
    const late = row('U1', 10);
    late.realtime = at(12);
    late.delayMin = 2;
    const { planned } = await planFixtureBoard([late]);
    expect(planned[0]?.options.length).toBeGreaterThan(0);
    expect(planned[0]?.best?.legs[0]?.departure).toBe(at(10));
  });

  test('a cancelled departure is planned but never recommended', async () => {
    const withdrawn = row('U1', 10);
    withdrawn.cancelled = true;
    const { planned } = await planFixtureBoard([withdrawn]);
    // What the timetable said is still worth carrying; the endorsement is not.
    expect(planned[0]?.options.length).toBeGreaterThan(0);
    expect(planned[0]?.best).toBeNull();
  });

  test('a spaced line label matches an unspaced one', async () => {
    const { planned } = await planFixtureBoard([row('U 1', 10)]);
    expect(planned[0]?.options.length).toBeGreaterThan(0);
  });

  test('a leading walking leg is skipped over to find the first transit leg', async () => {
    // The 08:10 itinerary opens with a walk onto the platform. Finding its first
    // transit leg is the only way that row is ever planned.
    const { planned } = await planFixtureBoard([row('U1', 10)]);
    const first = planned[0]?.options[0]?.legs[0];
    expect(first?.line).toBe('U1');
    expect(first?.mode).toBe('UBAHN');
    expect(first?.departure).toBe(at(10));
  });

  test('an itinerary whose first transit leg starts elsewhere is ignored', async () => {
    // The fixture carries a U1 leaving at the same minute from another stop
    // entirely, alighting at a stop nothing else uses.
    const { planned } = await planFixtureBoard([row('U1', 10)]);
    const exits = (planned[0]?.options ?? []).map((option) => option.exitStop);
    expect(exits.some((stop) => stop.startsWith('de:00000:8'))).toBe(false);
    for (const option of planned[0]?.options ?? []) expect(option.exitStopName).toBe('Interchange');
  });
});

describe('exit connections', () => {
  test('the transfer walk comes from the distance, not from the planner duration', async () => {
    // 140 metres at the package's pace is two minutes, so a vehicle arriving at
    // 08:20 makes an 08:23 departure comfortably. The planner bills the same
    // walk at eight minutes; believing it would put the earliest feasible
    // change at 08:28 and drop both earlier options, buffer included.
    expect(Math.round(140 / WALK_METRES_PER_MINUTE)).toBe(2);
    const { planned } = await planFixtureBoard([row('U1', 10)]);
    const arrivals = (planned[0]?.options ?? []).map((option) => option.arrival);
    expect(arrivals).toEqual([X9_TIGHT_ARRIVAL, X9_BEST_ARRIVAL, X9_SLOW_ARRIVAL]);
  });

  test('an option inside the early buffer is tight and is never the best one', async () => {
    const { planned } = await planFixtureBoard([row('U1', 10)]);
    const options = planned[0]?.options ?? [];
    const tight = options.find((option) => option.arrival === X9_TIGHT_ARRIVAL) as RouteOption;
    // 08:21 is one minute before the feasible 08:22, so it only comes off if the
    // train runs early or the change is quicker than the pace assumes.
    expect(tight.tight).toBe(true);
    expect(options.filter((option) => option.tight)).toHaveLength(1);
    expect(planned[0]?.best?.arrival).toBe(X9_BEST_ARRIVAL);
    expect(planned[0]?.best?.tight).toBe(false);
  });

  test('a zero buffer drops the tight option outright', async () => {
    const { planned } = await planFixtureBoard([row('U1', 10)], 0);
    const options = planned[0]?.options ?? [];
    expect(options.map((option) => option.arrival)).toEqual([X9_BEST_ARRIVAL, X9_SLOW_ARRIVAL]);
    expect(options.some((option) => option.tight)).toBe(false);
  });

  test('an option names its exit stop and counts its changes', async () => {
    const { planned } = await planFixtureBoard([row('U1', 10)]);
    const best = planned[0]?.best as RouteOption;
    expect(best.exitStop).toBe('de:00000:9:1:1');
    expect(best.exitStopName).toBe('Interchange');
    expect(best.transfers).toBe(1);
    expect(best.legs.map((leg) => leg.line)).toEqual(['U1', 'X9']);
    expect(best.legs[1]?.departure).toBe(at(23));
  });
});

describe('cursor paging', () => {
  /** A page whose only itinerary leaves at 08:10 and which always offers another. */
  function endlessPages() {
    return mockFetch((_url, call) => ({
      itineraries: [
        {
          startTime: '2026-01-01T08:10:00Z',
          endTime: '2026-01-01T08:30:00Z',
          legs: [
            {
              mode: 'SUBWAY',
              tripId: `trip-${call}`,
              routeShortName: 'U1',
              startTime: '2026-01-01T08:10:00Z',
              endTime: '2026-01-01T08:20:00Z',
              from: { name: 'Home', stopId: 'de-DELFI_de:00000:1:51:51' },
              to: { name: 'Interchange', stopId: 'de-DELFI_de:00000:9:1:1' },
            },
          ],
        },
      ],
      nextPageCursor: `LATER|${call}`,
    }));
  }

  test('stops at the cap when coverage is never reached', async () => {
    const { fetchImpl, urls } = endlessPages();
    // The last row leaves ninety minutes out and no page ever gets near it.
    await planBoard({
      stop: HOME,
      destination: DESTINATION,
      rows: [row('U1', 10), row('U1', 90)],
      startMs: FIXTURE_NOW,
      baseUrl: BASE,
      fetchImpl,
    });
    expect(urls).toHaveLength(PLAN_MAX_PAGES);
    expect(urls[0]).toContain('time=');
    expect(urls[0]).not.toContain('pageCursor=');
    for (const url of urls.slice(1)) expect(url).toContain('pageCursor=');
  });

  test('stops as soon as the itineraries cover the last row', async () => {
    const { fetchImpl, urls } = endlessPages();
    await planBoard({
      stop: HOME,
      destination: DESTINATION,
      rows: [row('U1', 10)],
      startMs: FIXTURE_NOW,
      baseUrl: BASE,
      fetchImpl,
    });
    expect(urls).toHaveLength(1);
  });

  test('coverage ignores itineraries that board somewhere else', async () => {
    // Same page, but the one itinerary starts at another stop, so it can cover
    // nothing on this board and the walk must go on to the cap.
    const { fetchImpl, urls } = mockFetch((_url, call) => ({
      itineraries: [
        {
          startTime: '2026-01-01T08:10:00Z',
          endTime: '2026-01-01T08:30:00Z',
          legs: [
            {
              mode: 'SUBWAY',
              tripId: `trip-${call}`,
              routeShortName: 'U1',
              startTime: '2026-01-01T08:10:00Z',
              endTime: '2026-01-01T08:20:00Z',
              from: { name: 'Other Stop', stopId: 'de-DELFI_de:00000:7:1:1' },
              to: { name: 'Wrong Exit', stopId: 'de-DELFI_de:00000:8:1:1' },
            },
          ],
        },
      ],
      nextPageCursor: `LATER|${call}`,
    }));
    await planBoard({
      stop: HOME,
      destination: DESTINATION,
      rows: [row('U1', 10)],
      startMs: FIXTURE_NOW,
      baseUrl: BASE,
      fetchImpl,
    });
    expect(urls).toHaveLength(PLAN_MAX_PAGES);
  });
});

describe('the plan cache', () => {
  test('a second plan in the same minute asks for nothing', async () => {
    const body = await fixture<unknown>('transitous-plan.json');
    const { fetchImpl, urls } = mockFetch((_url, call) => (call === 0 ? body : { itineraries: [], nextPageCursor: '' }));
    const call = () =>
      planBoard({
        stop: HOME,
        destination: DESTINATION,
        rows: [row('U1', 10)],
        startMs: FIXTURE_NOW,
        baseUrl: BASE,
        fetchImpl,
      });
    const first = await call();
    const before = urls.length;
    const second = await call();
    expect(urls).toHaveLength(before);
    expect(second[0]?.best?.arrival).toBe(first[0]?.best?.arrival);
  });
});
