import { describe, expect, test } from 'bun:test';
import { createMvgBackend, directionFromLineId, MVG_PAGE_LIMIT, stripMarkup } from '../src/backends/mvg.ts';
import { ALL_MODES, type Mode } from '../src/model.ts';
import { FIXTURE_NOW, FIXTURE_WINDOW, fixture, mockFetch } from './helpers.ts';

const clock = () => FIXTURE_NOW;

const SYNTHETIC_STOP = 'de:00000:1';

describe('primary backend requests', () => {
  test('always names every transport type, because the default response omits some', async () => {
    const rows = await fixture<unknown[]>('mvg-departures.json');
    const { fetchImpl, urls } = mockFetch(() => rows);
    const backend = createMvgBackend({ fetchImpl, baseUrl: 'https://example.invalid/api', now: clock });

    await backend.departures(SYNTHETIC_STOP, FIXTURE_WINDOW);

    expect(urls.length).toBeGreaterThan(0);
    for (const entry of urls) {
      const url = new URL(entry);
      const requested = (url.searchParams.get('transportTypes') ?? '').split(',');
      // The regression this guards: with no explicit list the API silently
      // drops regional rail and regional buses. Every page has to carry the
      // list, not just the first.
      for (const mode of ALL_MODES) expect(requested).toContain(mode);
      expect(url.searchParams.get('limit')).toBe(String(MVG_PAGE_LIMIT));
    }
  });

  test('sorts rows the API returns out of order', async () => {
    const rows = await fixture<unknown[]>('mvg-departures.json');
    const { fetchImpl } = mockFetch(() => rows);
    const backend = createMvgBackend({ fetchImpl, baseUrl: 'https://example.invalid/api', now: clock });

    const departures = await backend.departures(SYNTHETIC_STOP, FIXTURE_WINDOW);

    const times = departures.map((row) => row.realtime);
    expect(times).toEqual([...times].sort((a, b) => a - b));
    // The fixture puts the latest row first on purpose.
    expect(departures[0]?.line).toBe('S8');
    expect(departures[departures.length - 1]?.line).toBe('U6');
  });

  test('normalises a row completely', async () => {
    const rows = await fixture<unknown[]>('mvg-departures.json');
    const { fetchImpl } = mockFetch(() => rows);
    const backend = createMvgBackend({ fetchImpl, baseUrl: 'https://example.invalid/api', now: clock });

    const departures = await backend.departures(SYNTHETIC_STOP, FIXTURE_WINDOW);
    const sbahn = departures.find((row) => row.line === 'S8');
    expect(sbahn).toBeDefined();
    expect(sbahn?.mode).toBe('SBAHN');
    expect(sbahn?.delayMin).toBe(2);
    expect(sbahn?.realtimeKnown).toBe(true);
    expect(sbahn?.platform).toBeNull(); // the fixture's empty string
    expect(sbahn?.direction).toBe('H');
    expect(sbahn?.backend).toBe('mvg');
    expect(sbahn?.stop).toBe(SYNTHETIC_STOP);

    const bus = departures.find((row) => row.line === '900');
    expect(bus?.cancelled).toBe(true);
    expect(bus?.sev).toBe(true);
    expect(bus?.mode).toBe('REGIONAL_BUS');
  });

  test('keeps only the requested categories', async () => {
    const rows = await fixture<unknown[]>('mvg-departures.json');
    const { fetchImpl } = mockFetch(() => rows);
    const backend = createMvgBackend({
      fetchImpl,
      baseUrl: 'https://example.invalid/api',
      transportTypes: ['SBAHN'],
      now: clock,
    });

    const departures = await backend.departures(SYNTHETIC_STOP, FIXTURE_WINDOW);
    expect(departures.map((row) => row.mode)).toEqual(['SBAHN']);
  });
});

describe('direction letter extraction', () => {
  test('takes the fourth field of the line identifier', () => {
    expect(directionFromLineId('swm:02:S8:H:001')).toBe('H');
    expect(directionFromLineId('swm:03:RE72:R:001')).toBe('R');
  });

  test('yields null for anything malformed rather than guessing', () => {
    expect(directionFromLineId('swm:02')).toBeNull();
    expect(directionFromLineId('swm:02:S8:X:001')).toBeNull();
    expect(directionFromLineId(undefined)).toBeNull();
    expect(directionFromLineId(42)).toBeNull();
  });
});

describe('primary backend paging', () => {
  test('stops when the offset stops advancing, not when a page comes back short', async () => {
    const rows = await fixture<unknown[]>('mvg-departures.json');
    const { fetchImpl, urls } = mockFetch(() => rows);
    const backend = createMvgBackend({ fetchImpl, baseUrl: 'https://example.invalid/api', now: clock });

    await backend.departures(SYNTHETIC_STOP, FIXTURE_WINDOW);

    // Four rows is far short of a page and proves nothing about the stop, so
    // the walk continues. The second page repeats the same newest row, the
    // offset will not move, and that is what ends it. One extra request is the
    // whole price of not truncating a busy stop.
    expect(urls).toHaveLength(2);
    const offsets = urls.map((url) => Number(new URL(url).searchParams.get('offsetInMinutes')));
    expect(offsets[1]).toBeGreaterThan(offsets[0] as number);
  });

  test('asks for another page after a full one, then stops', async () => {
    const rows = await fixture<Array<Record<string, unknown>>>('mvg-departures.json');
    const template = rows[1] as Record<string, unknown>;
    // A full page, spread over the window so paging has somewhere to advance to.
    const fullPage = Array.from({ length: MVG_PAGE_LIMIT }, (_unused, index) => ({
      ...template,
      lineId: `swm:02:S8:H:${index}`,
      plannedDepartureTime: (template.plannedDepartureTime as number) + index * 60_000,
      realtimeDepartureTime: (template.realtimeDepartureTime as number) + index * 60_000,
    }));

    const { fetchImpl, urls } = mockFetch((_url, call) => (call === 0 ? fullPage : rows));
    const backend = createMvgBackend({ fetchImpl, baseUrl: 'https://example.invalid/api', now: clock });

    await backend.departures(SYNTHETIC_STOP, FIXTURE_WINDOW);
    expect(urls).toHaveLength(2);
    const first = new URL(urls[0] as string).searchParams.get('offsetInMinutes');
    const second = new URL(urls[1] as string).searchParams.get('offsetInMinutes');
    expect(Number(second)).toBeGreaterThan(Number(first));
  });
});

/**
 * A stop busy enough that no single page reaches a long horizon: a departure
 * every `BUSY_HEADWAY_MS`, so a full page of `MVG_PAGE_LIMIT` rows spans a
 * little over forty minutes and a three-hour board needs five of them.
 *
 * The headway is deliberately not a whole number of minutes, so a page ends
 * part-way through one. That is exactly where an offset rounded up to the next
 * minute would step over the rows the row cap had just cut off.
 */
const BUSY_HEADWAY_MS = 25_000;
const BUSY_HORIZON_MINUTES = 180;
const BUSY_WINDOW = { fromMs: FIXTURE_NOW, toMs: FIXTURE_NOW + BUSY_HORIZON_MINUTES * 60_000 };

const BUSY_LANES = [
  { label: 'S1', transportType: 'SBAHN', destination: 'Synthetic North', lineId: 'swm:02:S1:H:001' },
  { label: 'S2', transportType: 'SBAHN', destination: 'Synthetic East', lineId: 'swm:02:S2:R:001' },
  { label: 'U3', transportType: 'UBAHN', destination: 'Synthetic South', lineId: 'swm:01:U3:H:001' },
  { label: '53', transportType: 'BUS', destination: 'Synthetic West', lineId: 'swm:04:53:R:001' },
];

/**
 * Five hours of timetable, comfortably past any window a test here asks for, so
 * there is always something beyond the horizon left to drop. Built in code
 * rather than checked in, because seven hundred rows of JSON is a file nobody
 * would ever read.
 *
 * The line identifier repeats per lane, as a real one does; what makes a row
 * unique is its planned time, which is what the backend's dedupe key relies on.
 */
const BUSY_TIMETABLE = Array.from({ length: 720 }, (_unused, index) => {
  const lane = BUSY_LANES[index % BUSY_LANES.length] as (typeof BUSY_LANES)[number];
  const time = FIXTURE_NOW + index * BUSY_HEADWAY_MS;
  return {
    ...lane,
    plannedDepartureTime: time,
    realtimeDepartureTime: time,
    delayInMinutes: 0,
    cancelled: false,
    sev: false,
    platform: '1',
    realtime: true,
  };
});

/**
 * The API's own order of operations: take the first `limit` rows at or after the
 * offset, and *then* drop the categories nobody asked for. A page can come back
 * a quarter full with hours of timetable still behind it, which is the whole
 * reason the paging loop may not count rows.
 */
function busyStation(): ReturnType<typeof mockFetch> {
  return mockFetch((url) => {
    const params = new URL(url).searchParams;
    const from = FIXTURE_NOW + Number(params.get('offsetInMinutes')) * 60_000;
    const wanted = new Set((params.get('transportTypes') ?? '').split(','));
    return BUSY_TIMETABLE.filter((row) => row.realtimeDepartureTime >= from)
      .slice(0, Number(params.get('limit')))
      .filter((row) => wanted.has(row.transportType));
  });
}

/** Every departure of those categories inside the window, in timetable order. */
function busyExpected(modes: readonly Mode[]): number[] {
  const wanted = new Set<string>(modes);
  return BUSY_TIMETABLE.filter(
    (row) => wanted.has(row.transportType) && row.realtimeDepartureTime <= BUSY_WINDOW.toMs,
  ).map((row) => row.realtimeDepartureTime);
}

function offsetsOf(urls: readonly string[]): number[] {
  return urls.map((url) => Number(new URL(url).searchParams.get('offsetInMinutes')));
}

describe('paging a busy station to the horizon', () => {
  test('walks several pages, each from a strictly later offset', async () => {
    const { fetchImpl, urls } = busyStation();
    const backend = createMvgBackend({ fetchImpl, baseUrl: 'https://example.invalid/api', now: clock });

    await backend.departures(SYNTHETIC_STOP, BUSY_WINDOW);

    expect(urls.length).toBeGreaterThan(2);
    const offsets = offsetsOf(urls);
    for (let i = 1; i < offsets.length; i += 1) expect(offsets[i]).toBeGreaterThan(offsets[i - 1] as number);
  });

  test('covers the whole horizon with no gap at a page boundary', async () => {
    const { fetchImpl } = busyStation();
    const backend = createMvgBackend({ fetchImpl, baseUrl: 'https://example.invalid/api', now: clock });

    const departures = await backend.departures(SYNTHETIC_STOP, BUSY_WINDOW);

    // Equality against the timetable, not a count: a gap where one page ends
    // and the next begins is invisible to a count and obvious here.
    expect(departures.map((row) => row.realtime)).toEqual(busyExpected(ALL_MODES));
  });

  test('returns no row twice although the pages overlap on purpose', async () => {
    const { fetchImpl } = busyStation();
    const backend = createMvgBackend({ fetchImpl, baseUrl: 'https://example.invalid/api', now: clock });

    const departures = await backend.departures(SYNTHETIC_STOP, BUSY_WINDOW);

    const triples = departures.map((row) => `${row.line}|${row.planned}|${row.destination}`);
    expect(new Set(triples).size).toBe(triples.length);
  });

  test('drops the rows past the horizon that the last page brought back', async () => {
    const { fetchImpl } = busyStation();
    const backend = createMvgBackend({ fetchImpl, baseUrl: 'https://example.invalid/api', now: clock });

    const departures = await backend.departures(SYNTHETIC_STOP, BUSY_WINDOW);

    // The stop really does have more to give, so this is a live assertion and
    // not a vacuous one.
    expect(BUSY_TIMETABLE.some((row) => row.realtimeDepartureTime > BUSY_WINDOW.toMs)).toBe(true);
    for (const row of departures) {
      expect(row.realtime).toBeGreaterThanOrEqual(BUSY_WINDOW.fromMs);
      expect(row.realtime).toBeLessThanOrEqual(BUSY_WINDOW.toMs);
    }
  });

  test('keeps walking after a short page, because the cap precedes the category filter', async () => {
    const { fetchImpl, urls } = busyStation();
    const backend = createMvgBackend({ fetchImpl, baseUrl: 'https://example.invalid/api', now: clock });

    const departures = await backend.departures(SYNTHETIC_STOP, BUSY_WINDOW, { transportTypes: ['UBAHN'] });

    // One lane in four, so every page comes back at a quarter of the limit and
    // a row count would have ended the walk at the first one.
    const pages = urls.length;
    expect(pages).toBeGreaterThan(2);
    expect(departures.map((row) => row.realtime)).toEqual(busyExpected(['UBAHN']));
  });

  test('reports one structured progress event per request', async () => {
    const seen: Array<{ backend: string; stop: string; page: number; rows: number }> = [];
    const { fetchImpl, urls } = busyStation();
    const backend = createMvgBackend({
      fetchImpl,
      baseUrl: 'https://example.invalid/api',
      now: clock,
      onProgress: (event) => seen.push(event),
    });

    await backend.departures(SYNTHETIC_STOP, BUSY_WINDOW);

    expect(seen).toHaveLength(urls.length);
    expect(seen.map((event) => event.page)).toEqual(urls.map((_unused, index) => index + 1));
    for (const event of seen) {
      expect(event.backend).toBe('mvg');
      expect(event.stop).toBe(SYNTHETIC_STOP);
      expect(event.rows).toBeGreaterThan(0);
    }
  });
});

describe('a per-call transport-type narrowing', () => {
  test('reaches the query string, and the list stays explicit', async () => {
    const { fetchImpl, urls } = busyStation();
    const backend = createMvgBackend({ fetchImpl, baseUrl: 'https://example.invalid/api', now: clock });

    await backend.departures(SYNTHETIC_STOP, BUSY_WINDOW, { transportTypes: ['UBAHN', 'SBAHN'] });

    expect(urls.length).toBeGreaterThan(0);
    for (const url of urls) {
      expect(new URL(url).searchParams.get('transportTypes')).toBe('UBAHN,SBAHN');
    }
  });

  test('can only shrink the configured set, never widen it', async () => {
    const { fetchImpl, urls } = busyStation();
    const backend = createMvgBackend({
      fetchImpl,
      baseUrl: 'https://example.invalid/api',
      transportTypes: ['UBAHN'],
      now: clock,
    });

    const departures = await backend.departures(SYNTHETIC_STOP, BUSY_WINDOW, { transportTypes: ['UBAHN', 'BAHN'] });

    for (const url of urls) expect(new URL(url).searchParams.get('transportTypes')).toBe('UBAHN');
    expect(new Set(departures.map((row) => row.mode))).toEqual(new Set(['UBAHN']));
  });

  test('that intersects to nothing asks for nothing at all', async () => {
    const { fetchImpl, urls } = busyStation();
    const backend = createMvgBackend({
      fetchImpl,
      baseUrl: 'https://example.invalid/api',
      transportTypes: ['UBAHN'],
      now: clock,
    });

    const departures = await backend.departures(SYNTHETIC_STOP, BUSY_WINDOW, { transportTypes: ['TRAM'] });

    // An empty list in the query string would read as no list at all, which is
    // the silent-drop behaviour the explicit list exists to avoid.
    expect(departures).toEqual([]);
    expect(urls).toHaveLength(0);
  });
});

describe('service message text', () => {
  test('markup becomes whitespace and entities are decoded', () => {
    expect(stripMarkup('<p>Line&nbsp;A &amp; B</p><br/>resumed')).toBe('Line A & B resumed');
    expect(stripMarkup('plain')).toBe('plain');
    expect(stripMarkup('')).toBe('');
  });
});
