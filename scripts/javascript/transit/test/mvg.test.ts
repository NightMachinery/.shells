import { describe, expect, test } from 'bun:test';
import { createMvgBackend, directionFromLineId, MVG_PAGE_LIMIT, stripMarkup } from '../src/backends/mvg.ts';
import { ALL_MODES } from '../src/model.ts';
import { FIXTURE_NOW, FIXTURE_WINDOW, fixture, mockFetch } from './helpers.ts';

const clock = () => FIXTURE_NOW;

const SYNTHETIC_STOP = 'de:00000:1';

describe('primary backend requests', () => {
  test('always names every transport type, because the default response omits some', async () => {
    const rows = await fixture<unknown[]>('mvg-departures.json');
    const { fetchImpl, urls } = mockFetch(() => rows);
    const backend = createMvgBackend({ fetchImpl, baseUrl: 'https://example.invalid/api', now: clock });

    await backend.departures(SYNTHETIC_STOP, FIXTURE_WINDOW);

    expect(urls).toHaveLength(1);
    const url = new URL(urls[0] as string);
    const requested = (url.searchParams.get('transportTypes') ?? '').split(',');
    // The regression this guards: with no explicit list the API silently drops
    // regional rail and regional buses.
    for (const mode of ALL_MODES) expect(requested).toContain(mode);
    expect(url.searchParams.get('limit')).toBe(String(MVG_PAGE_LIMIT));
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
  test('stops after a short page', async () => {
    const rows = await fixture<unknown[]>('mvg-departures.json');
    const { fetchImpl, urls } = mockFetch(() => rows);
    const backend = createMvgBackend({ fetchImpl, baseUrl: 'https://example.invalid/api', now: clock });

    await backend.departures(SYNTHETIC_STOP, FIXTURE_WINDOW);
    expect(urls).toHaveLength(1);
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

describe('service message text', () => {
  test('markup becomes whitespace and entities are decoded', () => {
    expect(stripMarkup('<p>Line&nbsp;A &amp; B</p><br/>resumed')).toBe('Line A & B resumed');
    expect(stripMarkup('plain')).toBe('plain');
    expect(stripMarkup('')).toBe('');
  });
});
