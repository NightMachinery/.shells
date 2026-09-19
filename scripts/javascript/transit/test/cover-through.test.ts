import { beforeEach, describe, expect, test } from 'bun:test';
import { clearPlanCache, planBoard, type PlanTarget } from '../src/plan.ts';
import { clearOriginCache } from '../src/origin.ts';
import type { Departure } from '../src/model.ts';
import { FIXTURE_NOW, mockFetch } from './helpers.ts';

// The horizon-extension fix (src/page/data.ts, src/page/commute.ts, src/cli.ts)
// fetches and plans further than a board renders, so a row near the end of the
// visible horizon still has something past it to find. This file exercises the
// piece of that fix that lives in `planBoard` itself: `coverThroughMs`, the
// option that tells the search how far it has to reach when that is more than
// "the latest row in `rows`".

const BASE = 'https://example.invalid/api';

/** The board's parent stop; every identifier in this file is made up. */
const HOME = 'de:00000:1';
const INTERCHANGE_ID = 'de-DELFI_de:00000:9:1:1';
const DESTINATION: PlanTarget[] = [{ place: { lat: 0, lon: 0 }, name: 'Destination', walkMinutes: null }];

/** Minutes past the instant the fixtures are written around. */
function at(minutes: number): number {
  return FIXTURE_NOW + minutes * 60_000;
}

function iso(ms: number): string {
  return new Date(ms).toISOString();
}

/** A stand-in horizon: the picker's edge, five minutes after this row leaves. */
const HORIZON_MINUTES = 60;
/** Comfortably inside a 120-minute extension past that horizon. */
const ONWARD_MINUTES = HORIZON_MINUTES + 40;

function row(): Departure {
  return {
    line: 'U1',
    mode: 'UBAHN',
    destination: 'Somewhere',
    planned: at(HORIZON_MINUTES - 5),
    realtime: at(HORIZON_MINUTES - 5),
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

function firstLeg(): unknown {
  return {
    mode: 'SUBWAY',
    tripId: 'trip-first',
    routeShortName: 'U1',
    startTime: iso(at(HORIZON_MINUTES - 5)),
    endTime: iso(at(HORIZON_MINUTES - 2)),
    scheduledStartTime: iso(at(HORIZON_MINUTES - 5)),
    from: { name: 'Home', stopId: 'de-DELFI_de:00000:1:51:51' },
    to: { name: 'Interchange', stopId: INTERCHANGE_ID },
  };
}

function onwardLeg(): unknown {
  return {
    mode: 'SUBWAY',
    tripId: 'trip-onward',
    routeShortName: 'X9',
    startTime: iso(at(ONWARD_MINUTES)),
    endTime: iso(at(ONWARD_MINUTES + 5)),
    from: { name: 'Interchange', stopId: INTERCHANGE_ID },
    to: { name: 'Far Stop', stopId: 'de-DELFI_de:00000:20:1:1' },
  };
}

/**
 * What the real aggregator does with a narrow `searchWindow`: it never hands
 * back a change scheduled outside the window it was asked to search, so the
 * itinerary it returns stops after the first leg. Asked with a window wide
 * enough to reach the onward leg, both legs come back. The threshold sits
 * strictly between what an un-extended cover-through produces (about fifty-
 * five minutes of window) and what the extended one does (about three hours),
 * so which body a call gets back is decided entirely by `coverThroughMs`.
 */
const SEARCH_WINDOW_THRESHOLD_SECONDS = 90 * 60;

const SHORT_ITINERARY = {
  itineraries: [{ startTime: iso(at(HORIZON_MINUTES - 5)), endTime: iso(at(HORIZON_MINUTES - 2)), legs: [firstLeg()] }],
  nextPageCursor: '',
};

const FULL_ITINERARY = {
  itineraries: [
    { startTime: iso(at(HORIZON_MINUTES - 5)), endTime: iso(at(ONWARD_MINUTES + 5)), legs: [firstLeg(), onwardLeg()] },
  ],
  nextPageCursor: '',
};

function windowedPlanMock() {
  return mockFetch((url) => {
    if (url.includes('/stoptimes')) return { stopTimes: [{}] };
    const requestedSeconds = Number(new URL(url).searchParams.get('searchWindow') ?? '0');
    return requestedSeconds >= SEARCH_WINDOW_THRESHOLD_SECONDS ? FULL_ITINERARY : SHORT_ITINERARY;
  });
}

beforeEach(() => {
  // Same reason every other file in this suite clears these: the origin
  // resolution and the itinerary cache are both keyed on things every case
  // here shares, so one case would otherwise answer the next.
  clearPlanCache();
  clearOriginCache();
});

describe('coverThroughMs reaches a search past the rows it was given', () => {
  test('a departure just inside the horizon is planned an onward leg beyond it', async () => {
    const { fetchImpl } = windowedPlanMock();
    const planned = await planBoard({
      stop: HOME,
      targets: DESTINATION,
      rows: [row()],
      startMs: FIXTURE_NOW,
      baseUrl: BASE,
      fetchImpl,
      // What the page's and the CLI's callers now pass once they fetch and plan
      // past their own horizon; see src/page/commute.ts and src/cli.ts.
      coverThroughMs: at(HORIZON_MINUTES + 120),
    });
    const legs = (planned[0]?.options ?? []).flatMap((option) => option.legs);
    const onward = legs.find((leg) => leg.line === 'X9');
    expect(onward?.departure).toBe(at(ONWARD_MINUTES));
  });

  test('without it, the same row is planned only as far as its own window', async () => {
    // The control case: nothing here changed except leaving `coverThroughMs`
    // out, which is exactly the bug this option exists to fix. If this ever
    // starts finding the onward leg too, the mock above has stopped doing its
    // job of distinguishing the two searches.
    const { fetchImpl } = windowedPlanMock();
    const planned = await planBoard({
      stop: HOME,
      targets: DESTINATION,
      rows: [row()],
      startMs: FIXTURE_NOW,
      baseUrl: BASE,
      fetchImpl,
    });
    const legs = (planned[0]?.options ?? []).flatMap((option) => option.legs);
    expect(legs.find((leg) => leg.line === 'X9')).toBeUndefined();
  });
});
