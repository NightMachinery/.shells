import { beforeEach, describe, expect, test } from 'bun:test';
import { clearPlanCache, planBoard, type PlanTarget } from '../src/plan.ts';
import { clearOriginCache } from '../src/origin.ts';
import type { Departure } from '../src/model.ts';
import { mockFetch } from './helpers.ts';

// Measured on a real station, which is why this file exists. The departure board
// and the journey planner do not spell a platform the same way there: the board
// says platform 6 and the planner says track 86 for the same piece of concrete,
// and says 6 for it on the next train through. Rows were held to that spelling
// and refused their journeys, which on the busiest board on the page meant most
// of them. The headsign is what actually separates two trains of one line at one
// minute, and it is the thing the platform was brought in to settle.

const BASE = 'https://example.invalid/api';
const STOP = 'de:00000:10';
const DEPARTURE = '2026-01-01T08:00:00Z';
const TARGET: PlanTarget[] = [{ place: { lat: 0, lon: 0 }, name: 'Maxmonument', walkMinutes: null }];

function row(overrides: Partial<Departure> = {}): Departure {
  return {
    line: 'S3',
    mode: 'SBAHN',
    destination: 'Nordweg',
    planned: Date.parse(DEPARTURE),
    realtime: Date.parse(DEPARTURE),
    delayMin: 0,
    cancelled: false,
    sev: false,
    platform: '6',
    direction: 'H',
    backend: 'synthetic',
    stop: STOP,
    realtimeKnown: true,
    ...overrides,
  };
}

function itinerary(track: string, headsign: string) {
  return {
    startTime: DEPARTURE,
    endTime: '2026-01-01T08:20:00Z',
    legs: [
      {
        mode: 'METRO',
        tripId: 'trip-1',
        routeShortName: 'S3',
        headsign,
        agencyName: 'Synthetic Rail',
        startTime: DEPARTURE,
        scheduledStartTime: DEPARTURE,
        endTime: '2026-01-01T08:20:00Z',
        from: { name: 'Umstiegspunkt', stopId: STOP, track },
        to: { name: 'Maxmonument', stopId: 'de:00000:99' },
      },
    ],
  };
}

async function plan(rows: Departure[], track: string, headsign: string) {
  const { fetchImpl } = mockFetch((url) => {
    if (url.includes('/stoptimes')) return { stopTimes: [{}] };
    return { itineraries: [itinerary(track, headsign)], nextPageCursor: '' };
  });
  return planBoard({
    stop: STOP,
    targets: TARGET,
    rows,
    startMs: Date.parse('2026-01-01T07:50:00Z'),
    baseUrl: BASE,
    fetchImpl,
  });
}

beforeEach(() => {
  clearPlanCache();
  clearOriginCache();
});

describe('matching a row to the vehicle an itinerary starts with', () => {
  test('a platform both feeds spell the same way still matches', async () => {
    const planned = await plan([row()], '6', 'Nordweg');
    expect(planned[0]?.options).toHaveLength(1);
    expect(planned[0]?.miss).toBeUndefined();
  });

  test('two spellings of one platform do not cost the row its journey', async () => {
    const planned = await plan([row()], '86', 'Nordweg');
    expect(planned[0]?.options).toHaveLength(1);
  });

  test('the same line at the same minute going the other way is still refused', async () => {
    // The collision the platform check was added for: one parent identifier,
    // both directions, one line, one minute. The platforms disagree and so do
    // the destinations, and the second of those is the one that decides.
    const planned = await plan([row({ destination: 'Nordweg' })], '2', 'Südfeld');
    expect(planned[0]?.options).toHaveLength(0);
    expect(planned[0]?.miss).toBe('a journey for this line and minute was offered and did not match this row');
  });

  test('a row nobody planned for says so differently', async () => {
    const planned = await plan([row({ line: 'S7' })], '6', 'Nordweg');
    expect(planned[0]?.options).toHaveLength(0);
    expect(planned[0]?.miss).toBe('no itinerary the planner returned starts with this departure');
  });

  test('a district in brackets is not a different destination', async () => {
    const planned = await plan([row({ destination: 'Nordweg (Obb)' })], '86', 'Nordweg');
    expect(planned[0]?.options).toHaveLength(1);
  });
});
