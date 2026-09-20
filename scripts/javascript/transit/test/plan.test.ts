import { beforeEach, describe, expect, test } from 'bun:test';
import {
  clearPlanCache,
  planBoard,
  PLAN_MAX_PAGES,
  sameStopArea,
  WALK_METRES_PER_MINUTE,
  transitLegs,
  type PlanTarget,
  type RouteOption,
} from '../src/plan.ts';
import { clearOriginCache, UnresolvableOriginError } from '../src/origin.ts';
import type { Departure } from '../src/model.ts';
import { FIXTURE_NOW, fixture, mockFetch } from './helpers.ts';

const BASE = 'https://example.invalid/api';

/** The board's parent stop; every identifier in this file is made up. */
const HOME = 'de:00000:1';
const DESTINATION: PlanTarget[] = [{ place: { lat: 0, lon: 0 }, name: 'Destination', walkMinutes: null }];

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

/**
 * A fetch that answers the origin probe and then serves plan pages in order.
 *
 * `planBoard` resolves the stop identifier before it plans anything, and that
 * resolution is one `/stoptimes` request. Answering it here keeps the page
 * numbering in the plan handler about pages rather than about requests, and
 * `planUrls` keeps the paging assertions about paging.
 */
function planMock(pages: (page: number) => unknown) {
  let page = 0;
  const { fetchImpl, urls } = mockFetch((url) => {
    // The probe asks whether the stop exists; one row is enough to say yes.
    if (url.includes('/stoptimes')) return { stopTimes: [{}] };
    const body = pages(page);
    page += 1;
    return body;
  });
  return { fetchImpl, urls, planUrls: (): string[] => urls.filter((url) => url.includes('/plan')) };
}

async function planFixtureBoard(rows: Departure[], earlyBufferMinutes?: number) {
  const body = await fixture<unknown>('transitous-plan.json');
  // One page of substance, then nothing: the fixture carries a cursor, and a
  // second page of the same itineraries would only re-assert the first.
  const { fetchImpl, urls } = planMock((page) => (page === 0 ? body : { itineraries: [], nextPageCursor: '' }));
  const planned = await planBoard({
    stop: HOME,
    targets: DESTINATION,
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
  // these cases share; without this one case would answer another. The origin
  // resolution is remembered per stop for the life of the process, so it needs
  // clearing for the same reason: every case here uses the same stop.
  clearPlanCache();
  clearOriginCache();
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
    // One option, because all three of the fixture's rides are the same route:
    // same exit, same onward line, three departures. Which one survives is the
    // assertion that matters here. Believing the planner's eight minute walk
    // would put the earliest feasible change at 08:28 and elect the slow ride;
    // measuring the walk from the distance elects the 08:23 one.
    expect(arrivals).toEqual([X9_BEST_ARRIVAL]);
  });

  test('a gamble never represents a route a comfortable ride also covers', async () => {
    const { planned } = await planFixtureBoard([row('U1', 10)]);
    const options = planned[0]?.options ?? [];
    // 08:21 is one minute before the feasible 08:22, so it only comes off if the
    // train runs early or the change is quicker than the pace assumes. It is the
    // earliest arrival of its route and it is still not the one shown: one ride
    // stands for a route, and a row whose only representative could never be
    // recommended is a row with no journey on it.
    expect(options.some((option) => option.tight)).toBe(false);
    expect(options.some((option) => option.arrival === X9_TIGHT_ARRIVAL)).toBe(false);
    expect(planned[0]?.best?.arrival).toBe(X9_BEST_ARRIVAL);
    expect(planned[0]?.best?.tight).toBe(false);
  });

  test('a zero buffer drops the tight option outright', async () => {
    const { planned } = await planFixtureBoard([row('U1', 10)], 0);
    const options = planned[0]?.options ?? [];
    expect(options.map((option) => option.arrival)).toEqual([X9_BEST_ARRIVAL]);
    expect(options.some((option) => option.tight)).toBe(false);
  });

  test('an option names its exit stop and counts its changes', async () => {
    const { planned } = await planFixtureBoard([row('U1', 10)]);
    const best = planned[0]?.best as RouteOption;
    expect(best.exitStop).toBe('de:00000:9:1:1');
    expect(best.exitStopName).toBe('Interchange');
    expect(best.transfers).toBe(1);
    expect(transitLegs(best).map((leg) => leg.line)).toEqual(['U1', 'X9']);
    // The walks are legs too now: the change, and the last stretch to the door.
    expect(best.legs.map((leg) => leg.kind)).toEqual(['transit', 'walk', 'transit', 'walk']);
    expect(transitLegs(best)[1]?.departure).toBe(at(23));
  });
});

describe('cursor paging', () => {
  /** A page whose only itinerary leaves at 08:10 and which always offers another. */
  function endlessPages() {
    return planMock((call) => ({
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
    const { fetchImpl, planUrls } = endlessPages();
    // The last row leaves ninety minutes out and no page ever gets near it.
    await planBoard({
      stop: HOME,
      targets: DESTINATION,
      rows: [row('U1', 10), row('U1', 90)],
      startMs: FIXTURE_NOW,
      baseUrl: BASE,
      fetchImpl,
    });
    const pages = planUrls();
    expect(pages).toHaveLength(PLAN_MAX_PAGES);
    expect(pages[0]).toContain('time=');
    expect(pages[0]).not.toContain('pageCursor=');
    for (const url of pages.slice(1)) expect(url).toContain('pageCursor=');
  });

  test('stops as soon as the itineraries cover the last row', async () => {
    const { fetchImpl, planUrls } = endlessPages();
    await planBoard({
      stop: HOME,
      targets: DESTINATION,
      rows: [row('U1', 10)],
      startMs: FIXTURE_NOW,
      baseUrl: BASE,
      fetchImpl,
    });
    expect(planUrls()).toHaveLength(1);
  });

  test('coverage ignores itineraries that board somewhere else', async () => {
    // Same page, but the one itinerary starts at another stop, so it can cover
    // nothing on this board and the walk must go on to the cap.
    const { fetchImpl, planUrls } = planMock((call) => ({
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
      targets: DESTINATION,
      rows: [row('U1', 10)],
      startMs: FIXTURE_NOW,
      baseUrl: BASE,
      fetchImpl,
    });
    expect(planUrls()).toHaveLength(PLAN_MAX_PAGES);
  });
});

describe('exits along the way', () => {
  // The fixture: one line running at 08:10 and 08:20 from the board's stop,
  // calling at an interchange five minutes out and ending ten minutes later.
  // The planner's own itinerary for the 08:10 vehicle rides to the end and
  // changes onto something that arrives at 10:00; its itinerary for the 08:20
  // vehicle alights at the interchange and arrives at 08:50. A third line
  // calls only at a stop nobody was ever observed leaving.
  const MID_ARRIVAL = at(50);
  const FAR_ARRIVAL = at(120);

  async function planExits(rows: Departure[]) {
    const body = await fixture<unknown>('transitous-plan-exits.json');
    const { fetchImpl } = planMock((page) => (page === 0 ? body : { itineraries: [], nextPageCursor: '' }));
    return planBoard({ stop: HOME, targets: DESTINATION, rows, startMs: FIXTURE_NOW, baseUrl: BASE, fetchImpl });
  }

  test('an earlier departure is told to get off where a later one was', async () => {
    const planned = await planExits([row('T1', 10), row('T1', 20)]);
    const early = planned[0]?.best;
    const later = planned[1]?.best;
    // Both vehicles pass the interchange, so both get the same advice, and the
    // earlier one gets there earlier. Without intermediate stops the earlier
    // row could only ever be offered the one itinerary that began with it,
    // which is the one arriving over an hour later.
    expect(early?.exitStopName).toBe('Mid');
    expect(early?.arrival).toBe(MID_ARRIVAL);
    expect(later?.exitStopName).toBe('Mid');
    expect(early?.legs[0]?.to).toBe('Mid');
    // The ride is truncated at the exit, so the first leg ends when the rider
    // actually leaves the vehicle rather than where the planner's own
    // itinerary happened to end.
    expect(early?.legs[0]?.arrival).toBe(at(15));
  });

  test('a row whose calling points lead nowhere keeps its own itinerary', async () => {
    const planned = await planExits([row('T2', 12)]);
    expect(planned[0]?.best?.exitStopName).toBe('Far');
    expect(planned[0]?.best?.arrival).toBe(FAR_ARRIVAL);
  });

  test('the same onward journey from two exits is offered once, from the later one', async () => {
    const planned = await planExits([row('T1', 10)]);
    const options = planned[0]?.options ?? [];
    const keys = options.map((option) => option.legs.map((leg) => `${leg.line}@${leg.departure}`).join('>'));
    expect(new Set(keys).size).toBe(keys.length);
  });
});

describe('transit modes', () => {
  test('long-distance rail is left out of the request by default', async () => {
    const { urls } = await planFixtureBoard([row('U1', 10)]);
    const plan = urls.find((url) => url.includes('/plan')) ?? '';
    const modes = decodeURIComponent(new URL(plan).searchParams.get('transitModes') ?? '').split(',');
    // The planner routes over the whole national timetable and would otherwise
    // put an inter-city train in the middle of a commute, which is a fine
    // journey and not one a local ticket covers.
    expect(modes).not.toContain('HIGHSPEED_RAIL');
    expect(modes).not.toContain('LONG_DISTANCE');
    expect(modes).not.toContain('NIGHT_RAIL');
    expect(modes).not.toContain('COACH');
    expect(modes).toContain('SUBURBAN');
    expect(modes).toContain('REGIONAL_RAIL');
  });

  test('a configured mode list is sent instead, on every page', async () => {
    const { fetchImpl, planUrls } = planMock(() => ({
      itineraries: [],
      nextPageCursor: '',
    }));
    await planBoard({
      stop: HOME,
      targets: DESTINATION,
      rows: [row('U1', 10)],
      startMs: FIXTURE_NOW,
      baseUrl: BASE,
      planModes: ['TRANSIT'],
      fetchImpl,
    });
    for (const url of planUrls()) {
      expect(decodeURIComponent(new URL(url).searchParams.get('transitModes') ?? '')).toBe('TRANSIT');
    }
  });

  test('two mode lists are two searches, and one does not answer the other', async () => {
    const body = await fixture<unknown>('transitous-plan.json');
    const { fetchImpl, planUrls } = planMock((page) => (page === 0 ? body : { itineraries: [], nextPageCursor: '' }));
    const call = (planModes: string[]) =>
      planBoard({ stop: HOME, targets: DESTINATION, rows: [row('U1', 10)], startMs: FIXTURE_NOW, baseUrl: BASE, planModes, fetchImpl });
    await call(['SUBWAY']);
    const before = planUrls().length;
    await call(['SUBWAY', 'TRAM']);
    expect(planUrls().length).toBeGreaterThan(before);
  });
});

describe('the plan cache', () => {
  test('a second plan in the same minute asks for nothing', async () => {
    const body = await fixture<unknown>('transitous-plan.json');
    const { fetchImpl, urls } = planMock((page) => (page === 0 ? body : { itineraries: [], nextPageCursor: '' }));
    const call = () =>
      planBoard({
        stop: HOME,
        targets: DESTINATION,
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

  test('a stop nothing in the chain can resolve is walked once, not once a refresh', async () => {
    // Everything 404s: the parent is not a stop, there are no platform ids on
    // these rows, and the station lookup has nothing either.
    const { fetchImpl, urls } = mockFetch(
      () => new Response(JSON.stringify({ error: 'no radius' }), { status: 404, headers: { 'content-type': 'application/json' } }),
    );
    const call = (offsetMinutes: number) =>
      planBoard({
        stop: HOME,
        targets: DESTINATION,
        rows: [row('U1', 10)],
        // A different minute each time, so the itinerary cache cannot be what
        // suppresses the second attempt.
        startMs: FIXTURE_NOW + offsetMinutes * 60_000,
        baseUrl: BASE,
        fetchImpl,
      });
    await expect(call(0)).rejects.toBeInstanceOf(UnresolvableOriginError);
    const before = urls.length;
    expect(before).toBeGreaterThan(0);
    await expect(call(5)).rejects.toBeInstanceOf(UnresolvableOriginError);
    expect(urls).toHaveLength(before);
  });

  test('a resolved origin the planner then rejects is asked about once', async () => {
    // The stop resolves, so the chain is happy; it is the plan itself that
    // 404s, which is the case the negative memo on the resolved origin covers.
    const { fetchImpl, urls } = mockFetch((url) =>
      url.includes('/stoptimes')
        ? { stopTimes: [{}] }
        : new Response(JSON.stringify({ error: 'nope' }), { status: 404, headers: { 'content-type': 'application/json' } }),
    );
    const call = (offsetMinutes: number) =>
      planBoard({
        stop: HOME,
        targets: DESTINATION,
        rows: [row('U1', 10)],
        startMs: FIXTURE_NOW + offsetMinutes * 60_000,
        baseUrl: BASE,
        fetchImpl,
      });
    await expect(call(0)).rejects.toThrow(/404/);
    const before = urls.length;
    await expect(call(5)).rejects.toThrow(/404/);
    expect(urls).toHaveLength(before);
  });
});

describe('planning to the destination’s own stops', () => {
  /** The near station, six minutes from the door; the place is fourteen. */
  const NEAR = 'de:00000:5';
  const PLACE_TARGET: PlanTarget = { place: { lat: 0, lon: 0 }, name: 'Home', walkMinutes: null };
  const STOP_TARGET: PlanTarget = { place: { id: NEAR }, name: 'Home', walkMinutes: 6 };

  async function planTo(targets: PlanTarget[], walkWeight?: number) {
    const body = await fixture<{ place: unknown; stop: unknown }>('transitous-plan-targets.json');
    const { fetchImpl } = mockFetch((url) => {
      if (url.includes('/stoptimes')) return { stopTimes: [{}] };
      return url.includes(encodeURIComponent(NEAR)) ? body.stop : body.place;
    });
    return planBoard({
      stop: HOME,
      targets,
      rows: [row('T1', 10)],
      startMs: FIXTURE_NOW,
      baseUrl: BASE,
      fetchImpl,
      ...(walkWeight === undefined ? {} : { walkWeight }),
    });
  }

  /** Where the last vehicle of a journey puts the rider down. */
  function lastStation(option: RouteOption): string {
    const legs = transitLegs(option);
    return legs[legs.length - 1]?.to ?? '';
  }

  test('the near station is not offered at all when only the place is asked about', async () => {
    const planned = await planTo([PLACE_TARGET]);
    expect((planned[0]?.options ?? []).map(lastStation)).not.toContain('Near Station');
  });

  test('asking about the stop by name makes that journey exist', async () => {
    const planned = await planTo([PLACE_TARGET, STOP_TARGET]);
    expect((planned[0]?.options ?? []).map(lastStation)).toContain('Near Station');
  });

  test('the shorter walk wins when a walked minute costs two ridden ones', async () => {
    const planned = await planTo([PLACE_TARGET, STOP_TARGET], 2);
    const best = planned[0]?.best as RouteOption;
    expect(lastStation(best)).toBe('Near Station');
    expect(best.arrival).toBe(at(56));
    expect(Math.round(best.walkMinutes)).toBe(8);
  });

  test('the earlier arrival wins when walking is free', async () => {
    const planned = await planTo([PLACE_TARGET, STOP_TARGET], 1);
    const best = planned[0]?.best as RouteOption;
    expect(lastStation(best)).toBe('Far Station');
    expect(best.arrival).toBe(at(52));
    expect(Math.round(best.walkMinutes)).toBe(16);
  });

  test('a stop target’s final walk is the configured one, as its own leg', async () => {
    const planned = await planTo([STOP_TARGET]);
    const best = planned[0]?.best as RouteOption;
    const last = best.legs[best.legs.length - 1];
    expect(last?.kind).toBe('walk');
    expect(last?.to).toBe('Home');
    expect((last as { arrival: number }).arrival - (last as { departure: number }).departure).toBe(6 * 60_000);
  });
});

describe('a target that cannot be where it claims to be', () => {
  /** The doorstep the walks in these cases are measured from. */
  const DOOR: PlanTarget = { place: { lat: 0, lon: 0 }, name: 'Home', walkMinutes: null };
  const ACROSS_TOWN = 'de:00000:31';
  const DOWN_THE_ROAD = 'de:00000:32';
  /** One degree of latitude is 111_320 metres, so these are distances. */
  const north = (metres: number) => ({ latitude: metres / 111_320, longitude: 0 });

  async function planWith(stop: string, metres: number, walkMinutes: number) {
    const body = await fixture<{ place: unknown; stop: unknown }>('transitous-plan-targets.json');
    const { fetchImpl, urls } = mockFetch((url) => {
      if (url.includes('/stations/')) return north(metres);
      if (url.includes('/stoptimes')) return { stopTimes: [{}] };
      return url.includes(encodeURIComponent(stop)) ? body.stop : body.place;
    });
    const planned = await planBoard({
      stop: HOME,
      targets: [DOOR, { place: { id: stop }, name: 'Home', walkMinutes }],
      rows: [row('T1', 10)],
      startMs: FIXTURE_NOW,
      baseUrl: BASE,
      fetchImpl,
    });
    const asked = urls.filter((url) => url.includes('/plan') && url.includes(encodeURIComponent(stop)));
    return { planned, asked };
  }

  test('a stop three kilometres from the door is dropped, and said so once', async () => {
    const warnings: string[] = [];
    const warn = console.warn;
    console.warn = (...args: unknown[]) => void warnings.push(args.join(' '));
    try {
      // Three minutes on foot, says the configuration; three kilometres, says
      // the backend. This is the misplaced-target fault in miniature.
      const { planned, asked } = await planWith(ACROSS_TOWN, 3000, 3);
      expect(asked).toHaveLength(0);
      // The doorstep target is untouched, so the board still has its journeys.
      expect(planned[0]?.options.length).toBeGreaterThan(0);
    } finally {
      console.warn = warn;
    }
    expect(warnings.join('\n')).toContain(ACROSS_TOWN);
  });

  test('a stop four hundred metres from the door is asked about as usual', async () => {
    const { asked } = await planWith(DOWN_THE_ROAD, 400, 5);
    expect(asked.length).toBeGreaterThan(0);
  });
});

describe('the search window', () => {
  test('one wide window replaces the cursor walk', async () => {
    const body = await fixture<unknown>('transitous-plan.json');
    const { fetchImpl, planUrls } = planMock(() => body);
    await planBoard({ stop: HOME, targets: DESTINATION, rows: [row('U1', 10)], startMs: FIXTURE_NOW, baseUrl: BASE, fetchImpl });
    const urls = planUrls();
    expect(urls).toHaveLength(1);
    // The board's last row is ten minutes out, under the floor, so the floor is
    // what is asked for rather than six hundred seconds of nothing.
    expect(urls[0]).toContain('searchWindow=900');
  });

  test('the window is the span the board actually covers', async () => {
    const body = await fixture<unknown>('transitous-plan.json');
    const { fetchImpl, planUrls } = planMock(() => body);
    await planBoard({ stop: HOME, targets: DESTINATION, rows: [row('U1', 10), row('U1', 130)], startMs: FIXTURE_NOW, baseUrl: BASE, fetchImpl });
    expect(planUrls()[0]).toContain('searchWindow=7800');
  });
});

describe('a destination stop the aggregator carries only as a position', () => {
  const NEAR = 'de:00000:5';

  /**
   * The aggregator knows the board's stop and does not know the destination
   * stop, so the chain falls back to that station's coordinate and the planner
   * alights wherever it likes and walks from there.
   */
  function fallbackMock(body: unknown) {
    return mockFetch((url) => {
      if (url.includes('/stoptimes')) {
        const id = decodeURIComponent(new URL(url).searchParams.get('stopId') ?? '');
        return id.includes(NEAR)
          ? new Response(JSON.stringify({ error: 'stop_found=false' }), { status: 404 })
          : { stopTimes: [{}] };
      }
      if (url.includes('/stations/')) return { globalId: NEAR, latitude: 1, longitude: 2 };
      if (url.includes('/reverse-geocode')) return [];
      return body;
    });
  }

  test('the configured walk is dropped, because it is measured from somewhere else', async () => {
    const body = await fixture<{ place: unknown; stop: unknown }>('transitous-plan-targets.json');
    const { fetchImpl, urls } = fallbackMock(body.place);
    const planned = await planBoard({
      stop: HOME,
      // Claiming the destination is at the door, which is true of the stop and
      // not of the position the planner ends up using.
      targets: [{ place: { id: NEAR }, name: 'Home', walkMinutes: 0 }],
      rows: [row('T1', 10)],
      startMs: FIXTURE_NOW,
      baseUrl: BASE,
      fetchImpl,
    });
    expect(urls.some((url) => url.includes('toPlace=1%2C2'))).toBe(true);
    const best = planned[0]?.best as RouteOption;
    // The fixture's own final street leg is fourteen minutes; a walk of zero
    // here would be the bug this case exists for.
    expect(best.arrival).toBe(at(52));
    expect(Math.round(best.walkMinutes)).toBe(16);
  });
});
