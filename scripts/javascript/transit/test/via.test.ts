import { beforeEach, describe, expect, test } from 'bun:test';
import { applyVia } from '../src/via.ts';
import { callsAfter, callsAtAfter, clearTripCache, parseTrip, stationOf } from '../src/trip.ts';
import type { Departure } from '../src/model.ts';
import { mockFetch } from './helpers.ts';

// The board this exists for asks for departures towards the city from a station
// where both kinds of line meet. A direction letter belongs to a line rather
// than to a compass: at that station the same letter marked the rapid-transit
// services running in towards the centre and the regional services running out
// of it, because the centre is where one set of lines starts and where the other
// set ends. Everything below uses invented ids and names in the same shape as
// the real ones.

const STOP = 'de:00000:10';
const CITY = 'de:00000:100';
const OUTSIDE = 'de:00000:700';
const BASE = 'https://example.invalid/api';

const AT = Date.parse('2026-01-01T08:00:00Z');

function row(overrides: Partial<Departure> = {}): Departure {
  return {
    line: 'S3',
    mode: 'SBAHN',
    destination: 'Nordweg',
    planned: AT,
    realtime: AT,
    delayMin: 0,
    cancelled: false,
    sev: false,
    platform: '6',
    direction: 'H',
    backend: 'mvg',
    stop: STOP,
    realtimeKnown: true,
    ...overrides,
  };
}

/** A trip answer in the shape the aggregator publishes: one leg, two ends. */
function trip(calls: Array<[string, string, string]>) {
  const places = calls.map(([stopId, name, time]) => ({
    stopId: `de-DELFI_${stopId}`,
    name,
    arrival: time,
    departure: time,
    scheduledArrival: time,
    scheduledDeparture: time,
  }));
  return {
    legs: [
      {
        from: places[0],
        intermediateStops: places.slice(1, -1),
        to: places[places.length - 1],
      },
    ],
  };
}

const THROUGH_THE_CITY = trip([
  [`${STOP}:6:6`, 'Umstiegspunkt', '2026-01-01T08:00:00Z'],
  [`${CITY}:11:11`, 'Hauptplatz', '2026-01-01T08:12:00Z'],
  ['de:00000:300:1:1', 'Nordweg', '2026-01-01T08:30:00Z'],
]);

const AWAY_FROM_THE_CITY = trip([
  [`${STOP}:4:4`, 'Umstiegspunkt', '2026-01-01T08:00:00Z'],
  ['de:00000:500:1:1', 'Feldkirchen', '2026-01-01T08:25:00Z'],
  [`${OUTSIDE}:1:1`, 'Weitental', '2026-01-01T09:10:00Z'],
]);

function server(bodies: Record<string, unknown>, onTrip?: (tripId: string) => unknown) {
  return mockFetch((url) => {
    const match = url.match(/tripId=([^&]+)/);
    if (match !== null) {
      const tripId = decodeURIComponent(match[1] as string);
      const custom = onTrip?.(tripId);
      if (custom !== undefined) return custom;
      return bodies[tripId] ?? { legs: [] };
    }
    return { stopTimes: [] };
  });
}

beforeEach(() => {
  clearTripCache();
});

describe('reading a stop identifier', () => {
  test('a platform belongs to its station', () => {
    expect(stationOf('de-DELFI_de:00000:10:45:86')).toBe('de:00000:10');
    expect(stationOf('de:00000:10')).toBe('de:00000:10');
  });
});

describe('reading a trip', () => {
  test('the two ends and the stops between them are one list', () => {
    const calls = parseTrip(THROUGH_THE_CITY);
    expect(calls.map((call) => call.name)).toEqual(['Umstiegspunkt', 'Hauptplatz', 'Nordweg']);
    expect(calls[1]?.stopId).toBe(`${CITY}:11:11`);
  });

  test('a call the vehicle already made does not count', () => {
    const calls = parseTrip(THROUGH_THE_CITY);
    expect(callsAtAfter(calls, [CITY], AT)).toBe(true);
    // The same trip asked about from an hour later: the call at the city is
    // behind the rider, which is exactly the case a line that runs through the
    // centre and back out again produces.
    expect(callsAtAfter(calls, [CITY], AT + 60 * 60_000)).toBe(false);
  });

  test('what is still ahead does not include where the reader is standing', () => {
    const ahead = callsAfter(parseTrip(THROUGH_THE_CITY), STOP, AT);
    expect(ahead.map((call) => call.name)).toEqual(['Hauptplatz', 'Nordweg']);
  });
});

describe('filtering a board by a place the vehicle must call at', () => {
  test('a row whose vehicle calls there is kept and marked verified', async () => {
    const { fetchImpl } = server({ 'trip-through': THROUGH_THE_CITY });
    const kept = await applyVia([row({ tripId: 'trip-through' })], { via: [CITY], baseUrl: BASE, fetchImpl });
    expect(kept).toHaveLength(1);
    expect(kept[0]?.viaUnverified).toBeUndefined();
  });

  test('a station published under two identifiers counts as one place', async () => {
    // The real shape of this: a main station whose underground rapid-transit
    // hall is its own station in the data. The long-distance trains call at one
    // identifier and the local ones at another, under one name on every sign in
    // the building, and a filter that knew only one of them threw away exactly
    // the services the board existed for.
    const LOCAL_HALL = 'de:00000:6';
    const throughTheHall = trip([
      [`${STOP}:45:86`, 'Umstiegspunkt', '2026-01-01T08:00:00Z'],
      [`${LOCAL_HALL}:40:81`, 'Hauptplatz', '2026-01-01T08:11:00Z'],
      ['de:00000:300:1:1', 'Nordweg', '2026-01-01T08:30:00Z'],
    ]);
    const { fetchImpl } = server({ 'trip-local': throughTheHall });
    const kept = await applyVia([row({ tripId: 'trip-local' })], { via: [CITY, LOCAL_HALL], baseUrl: BASE, fetchImpl });
    expect(kept).toHaveLength(1);
    const missed = await applyVia([row({ tripId: 'trip-local' })], { via: [CITY], baseUrl: BASE, fetchImpl });
    expect(missed).toHaveLength(0);
  });

  test('a row whose vehicle goes the other way is dropped, whatever its letter says', async () => {
    const { fetchImpl } = server({ 'trip-away': AWAY_FROM_THE_CITY });
    // Direction H, like every row on the board it came from, and heading away.
    const kept = await applyVia([row({ line: 'RB 6', tripId: 'trip-away', direction: 'H' })], {
      via: [CITY],
      baseUrl: BASE,
      fetchImpl,
    });
    expect(kept).toHaveLength(0);
  });

  test('a row from a backend with no trip identifier borrows one from the aggregator', async () => {
    const { fetchImpl } = server({ 'trip-through': THROUGH_THE_CITY });
    const aggregatorRows = async (): Promise<Departure[]> => [row({ backend: 'transitous', tripId: 'trip-through' })];
    const kept = await applyVia([row()], { via: [CITY], baseUrl: BASE, fetchImpl, aggregatorRows });
    expect(kept).toHaveLength(1);
    expect(kept[0]?.tripId).toBe('trip-through');
    expect(kept[0]?.viaUnverified).toBeUndefined();
  });

  test('a row nothing can identify is kept and says it could not be checked', async () => {
    const { fetchImpl } = server({});
    const aggregatorRows = async (): Promise<Departure[]> => [];
    const kept = await applyVia([row()], { via: [CITY], baseUrl: BASE, fetchImpl, aggregatorRows });
    expect(kept).toHaveLength(1);
    expect(kept[0]?.viaUnverified).toBe(true);
  });

  test('a trip lookup that fails leaves the row on the board, marked', async () => {
    const { fetchImpl } = mockFetch((url) => {
      if (url.includes('tripId=')) return new Response('nope', { status: 500 });
      return { stopTimes: [] };
    });
    const kept = await applyVia([row({ tripId: 'trip-through' })], { via: [CITY], baseUrl: BASE, fetchImpl });
    expect(kept).toHaveLength(1);
    expect(kept[0]?.viaUnverified).toBe(true);
  });

  test('one lookup per vehicle, however many rows ask', async () => {
    const { fetchImpl, urls } = server({ 'trip-through': THROUGH_THE_CITY });
    const rows = [row({ tripId: 'trip-through' }), row({ tripId: 'trip-through', planned: AT + 60_000 })];
    const kept = await applyVia(rows, { via: [CITY], baseUrl: BASE, fetchImpl });
    expect(kept).toHaveLength(2);
    expect(urls.filter((url) => url.includes('tripId=')).length).toBe(1);
  });
});
