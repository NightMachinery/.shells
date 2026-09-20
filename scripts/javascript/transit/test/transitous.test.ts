import { beforeEach, describe, expect, test } from 'bun:test';
import {
  CITY_BUS_AGENCIES,
  createTransitousBackend,
  DELFI_ID_PREFIX,
  directionFromDirectionId,
  isCityBusAgency,
  normaliseLineName,
  toAggregatorId,
} from '../src/backends/transitous.ts';
import { clearOriginCache } from '../src/origin.ts';
import { FIXTURE_WINDOW, fixture, mockFetch } from './helpers.ts';

const SYNTHETIC_STOP = 'de:00000:1';
const BASE = 'https://example.invalid/api';

async function pages(): Promise<[unknown, unknown]> {
  return [
    await fixture<unknown>('transitous-stoptimes-page1.json'),
    await fixture<unknown>('transitous-stoptimes-page2.json'),
  ];
}

describe('aggregator identifiers', () => {
  test('prefixes a bare national id and leaves a prefixed one alone', () => {
    expect(toAggregatorId(SYNTHETIC_STOP)).toBe(`${DELFI_ID_PREFIX}${SYNTHETIC_STOP}`);
    expect(toAggregatorId(`${DELFI_ID_PREFIX}${SYNTHETIC_STOP}`)).toBe(`${DELFI_ID_PREFIX}${SYNTHETIC_STOP}`);
  });
});

describe('aggregator line names', () => {
  test('strips the trailing train number', () => {
    expect(normaliseLineName('RE72 (78949)')).toBe('RE72');
    expect(normaliseLineName('S8')).toBe('S8');
    expect(normaliseLineName(undefined)).toBe('');
  });
});

describe('aggregator direction flag', () => {
  // This mapping is asserted here so it cannot drift silently. It has not been
  // checked against the primary backend on the same stop; see
  // TRANSITOUS_DIRECTION_TRUSTED in the backend.
  test('maps the outbound flag onto the two letters', () => {
    expect(directionFromDirectionId(0)).toBe('H');
    expect(directionFromDirectionId(1)).toBe('R');
    expect(directionFromDirectionId('0')).toBe('H');
    expect(directionFromDirectionId(undefined)).toBeNull();
    expect(directionFromDirectionId(7)).toBeNull();
  });
});

describe('the urban-versus-regional bus heuristic', () => {
  test('recognises the city operator and nothing else', () => {
    expect(isCityBusAgency('MVG')).toBe(true);
    for (const known of CITY_BUS_AGENCIES) expect(isCityBusAgency(known)).toBe(true);
    expect(isCityBusAgency('Example Regional Coaches')).toBe(false);
    expect(isCityBusAgency(null)).toBe(false);
    expect(isCityBusAgency('')).toBe(false);
  });

  test('the regional brand is not mistaken for the city operator', () => {
    // It shares a prefix with the city operator's initials, so a loose match
    // would label every regional bus urban.
    expect(isCityBusAgency('MVV-Regionalbus')).toBe(false);
  });
});

describe('aggregator normalisation', () => {
  test('maps every category, parses UTC times and keeps the delay', async () => {
    const [page1, page2] = await pages();
    const { fetchImpl } = mockFetch((_url, call) => (call === 0 ? page1 : page2));
    const backend = createTransitousBackend({ fetchImpl, baseUrl: BASE });

    const rows = await backend.departures(SYNTHETIC_STOP, FIXTURE_WINDOW);
    const byLine = new Map(rows.map((row) => [row.line, row]));

    expect(byLine.get('RE72')?.mode).toBe('BAHN');
    expect(byLine.get('S8')?.mode).toBe('SBAHN');
    expect(byLine.get('U6')?.mode).toBe('UBAHN');
    // Same feed category, split by operating agency.
    expect(byLine.get('100')?.mode).toBe('BUS');
    expect(byLine.get('900')?.mode).toBe('REGIONAL_BUS');

    const regional = byLine.get('RE72');
    expect(regional?.planned).toBe(Date.parse('2026-01-01T08:00:00Z'));
    expect(regional?.realtime).toBe(Date.parse('2026-01-01T08:02:00Z'));
    expect(regional?.delayMin).toBe(2);
    expect(regional?.direction).toBe('R');
    expect(regional?.platform).toBe('8');
    expect(regional?.color).toBe('#AA3311');
    expect(byLine.get('S8')?.direction).toBe('H');
    expect(byLine.get('U6')?.platform).toBeNull();
    // The earlier of the two rows on this line carries no live data.
    const firstS8 = rows.find((row) => row.line === 'S8');
    expect(firstS8?.realtimeKnown).toBe(false);
  });

  test('follows the next-page cursor once and then stops', async () => {
    const [page1, page2] = await pages();
    const { fetchImpl, urls } = mockFetch((_url, call) => (call === 0 ? page1 : page2));
    const backend = createTransitousBackend({ fetchImpl, baseUrl: BASE });

    await backend.departures(SYNTHETIC_STOP, FIXTURE_WINDOW);

    expect(urls).toHaveLength(2);
    const first = new URL(urls[0] as string);
    expect(first.searchParams.get('stopId')).toBe(`${DELFI_ID_PREFIX}${SYNTHETIC_STOP}`);
    expect(Number(first.searchParams.get('n'))).toBeGreaterThanOrEqual(50);
    expect(first.searchParams.get('time')).toBeTruthy();
    // The second request pages, it does not re-anchor on a time.
    const second = new URL(urls[1] as string);
    expect(second.searchParams.get('pageCursor')).toBe('SYNTHETIC-CURSOR-2');
    expect(second.searchParams.get('time')).toBeNull();
  });
});

describe('a per-call transport-type narrowing', () => {
  test('narrows what is kept, since the endpoint takes no category parameter', async () => {
    const [page1, page2] = await pages();
    const { fetchImpl } = mockFetch((_url, call) => (call === 0 ? page1 : page2));
    const backend = createTransitousBackend({ fetchImpl, baseUrl: BASE });

    const rows = await backend.departures(SYNTHETIC_STOP, FIXTURE_WINDOW, { transportTypes: ['UBAHN'] });

    expect(rows.length).toBeGreaterThan(0);
    expect(new Set(rows.map((row) => row.mode))).toEqual(new Set(['UBAHN']));
  });

  test('cannot widen the configured set', async () => {
    const [page1, page2] = await pages();
    const { fetchImpl } = mockFetch((_url, call) => (call === 0 ? page1 : page2));
    const backend = createTransitousBackend({ fetchImpl, baseUrl: BASE, transportTypes: ['UBAHN'] });

    const rows = await backend.departures(SYNTHETIC_STOP, FIXTURE_WINDOW, { transportTypes: ['UBAHN', 'BAHN'] });

    expect(new Set(rows.map((row) => row.mode))).toEqual(new Set(['UBAHN']));
  });

  test('that intersects to nothing asks for nothing at all', async () => {
    const { fetchImpl, urls } = mockFetch(() => ({ stopTimes: [] }));
    const backend = createTransitousBackend({ fetchImpl, baseUrl: BASE, transportTypes: ['UBAHN'] });

    const rows = await backend.departures(SYNTHETIC_STOP, FIXTURE_WINDOW, { transportTypes: ['TRAM'] });

    expect(rows).toEqual([]);
    expect(urls).toHaveLength(0);
  });
});

describe('a parent identifier this feed has never heard of', () => {
  const PLATFORM = 'de:00000:1:2:2';

  // The resolution is memoised per stop for the life of the process, and these
  // cases resolve the same stop two different ways on purpose.
  beforeEach(() => {
    clearOriginCache();
  });

  /**
   * The aggregator answers 404 for the parent and serves the platform.
   *
   * This is not a contrived shape. It is what a real station does when the
   * national feed carries its platforms and not the stop above them, and the
   * whole point of the origin chain is to find them; the 404 used to throw
   * before the chain was ever consulted.
   */
  function feed(onPlatform: unknown) {
    return mockFetch((url) => {
      if (url.includes(encodeURIComponent(PLATFORM))) return onPlatform;
      // The chain probes a candidate with `stoptimes` before it trusts it, and
      // that probe is answered by the same rule as the fetch.
      return new Response(JSON.stringify({ error: 'no such stop' }), {
        status: 404,
        headers: { 'content-type': 'application/json' },
      });
    });
  }

  test('is asked about through its platforms rather than thrown', async () => {
    const [page1] = await pages();
    const { fetchImpl, urls } = feed(page1);
    const backend = createTransitousBackend({ fetchImpl, baseUrl: BASE });

    const rows = await backend.departures(SYNTHETIC_STOP, FIXTURE_WINDOW, { platformIds: [PLATFORM] });

    expect(rows.length).toBeGreaterThan(0);
    expect(urls.some((url) => url.includes(encodeURIComponent(PLATFORM)))).toBe(true);
  });

  test('a rate limit is still a failure, because an empty board would be a lie', async () => {
    const { fetchImpl } = mockFetch(
      () => new Response(JSON.stringify({ error: 'slow down' }), { status: 429, headers: { 'content-type': 'application/json' } }),
    );
    const backend = createTransitousBackend({ fetchImpl, baseUrl: BASE });

    await expect(backend.departures(SYNTHETIC_STOP, FIXTURE_WINDOW)).rejects.toThrow(/429/);
  });
});
