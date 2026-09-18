import { beforeEach, describe, expect, test } from 'bun:test';
import { clearOriginCache, departureIds, resolveOrigin, UnresolvableOriginError } from '../src/origin.ts';
import { mockFetch } from './helpers.ts';

const BASE = 'https://planner.invalid/api';
const MVG = 'https://primary.invalid/api';

/** The board's parent stop; every identifier in this file is made up. */
const STOP = 'de:00000:1';
const PARENT = 'de-DELFI_de:00000:1';

function notFound(): Response {
  return new Response(JSON.stringify({ error: 'no radius: stop_found=false' }), {
    status: 404,
    headers: { 'content-type': 'application/json' },
  });
}

/**
 * A fetch that knows exactly which identifiers exist.
 *
 * `known` is the set of aggregator identifiers that answer stop times; the
 * station lookup answers with `point` when one is given; the reverse geocode
 * answers with `nearby`.
 */
function service(options: { known: string[]; point?: { lat: number; lon: number }; nearby?: string[] }) {
  return mockFetch((url) => {
    if (url.includes('/stoptimes')) {
      const id = decodeURIComponent(new URL(url).searchParams.get('stopId') ?? '');
      return options.known.includes(id) ? { stopTimes: [{}] } : notFound();
    }
    if (url.includes('/stations/')) {
      return options.point === undefined
        ? notFound()
        : { globalId: STOP, latitude: options.point.lat, longitude: options.point.lon };
    }
    if (url.includes('/reverse-geocode')) {
      return (options.nearby ?? []).map((id) => ({ id, type: 'STOP' }));
    }
    return notFound();
  });
}

beforeEach(() => {
  clearOriginCache();
});

describe('the origin chain', () => {
  test('stops at the parent identifier when the planner has it', async () => {
    const { fetchImpl, urls } = service({ known: [PARENT] });
    const resolved = await resolveOrigin({ stop: STOP, baseUrl: BASE, mvgBaseUrl: MVG, fetchImpl });
    expect(resolved.level).toBe('parent');
    expect(resolved.place).toBe(PARENT);
    // One question asked, and no coordinate lookup behind it.
    expect(urls).toHaveLength(1);
  });

  test('falls to the platforms the primary backend already named', async () => {
    const platform = 'de-DELFI_de:00000:1:2:1';
    const { fetchImpl } = service({ known: [platform] });
    const resolved = await resolveOrigin({
      stop: STOP,
      platformIds: ['de:00000:1:2:1', 'de:00000:1:9:9'],
      baseUrl: BASE,
      mvgBaseUrl: MVG,
      fetchImpl,
    });
    expect(resolved.level).toBe('platform');
    expect(resolved.place).toBe(platform);
    // The platform that does not exist upstream is dropped rather than offered.
    expect(resolved.places).toEqual([platform]);
  });

  test('keeps the platform order it was given, which is most used first', async () => {
    const first = 'de-DELFI_de:00000:1:4:4';
    const second = 'de-DELFI_de:00000:1:2:1';
    const { fetchImpl } = service({ known: [first, second] });
    const resolved = await resolveOrigin({
      stop: STOP,
      platformIds: ['de:00000:1:4:4', 'de:00000:1:2:1'],
      baseUrl: BASE,
      mvgBaseUrl: MVG,
      fetchImpl,
    });
    expect(resolved.place).toBe(first);
    expect(resolved.places).toEqual([first, second]);
  });

  test('falls to the coordinate, and keeps only stops inside the same stop area', async () => {
    const { fetchImpl } = service({
      known: [],
      point: { lat: 1.5, lon: 2.5 },
      nearby: ['de-DELFI_de:00000:1:1:1', 'de-DELFI_de:00000:2:1:1'],
    });
    const resolved = await resolveOrigin({ stop: STOP, baseUrl: BASE, mvgBaseUrl: MVG, fetchImpl });
    expect(resolved.level).toBe('coordinate');
    expect(resolved.place).toBe('1.5,2.5');
    // The neighbouring stop is dropped: reporting its departures would be a
    // worse answer than reporting none.
    expect(resolved.places).toEqual(['1.5,2.5', 'de-DELFI_de:00000:1:1:1']);
    // A coordinate is not something stop times can be asked for.
    expect(departureIds(resolved)).toEqual(['de-DELFI_de:00000:1:1:1']);
  });

  test('gives up loudly when nothing resolves, and walks the chain only once', async () => {
    const { fetchImpl, urls } = service({ known: [] });
    await expect(resolveOrigin({ stop: STOP, baseUrl: BASE, mvgBaseUrl: MVG, fetchImpl })).rejects.toBeInstanceOf(
      UnresolvableOriginError,
    );
    const before = urls.length;
    await expect(resolveOrigin({ stop: STOP, baseUrl: BASE, mvgBaseUrl: MVG, fetchImpl })).rejects.toBeInstanceOf(
      UnresolvableOriginError,
    );
    expect(urls).toHaveLength(before);
  });

  test('a resolved stop is not asked about twice', async () => {
    const { fetchImpl, urls } = service({ known: [PARENT] });
    await resolveOrigin({ stop: STOP, baseUrl: BASE, mvgBaseUrl: MVG, fetchImpl });
    const before = urls.length;
    await resolveOrigin({ stop: STOP, baseUrl: BASE, mvgBaseUrl: MVG, fetchImpl });
    expect(urls).toHaveLength(before);
  });

  test('a cache answers without any request at all', async () => {
    const store = new Map<string, string[]>();
    const cache = {
      get: async (key: string): Promise<string[] | null> => store.get(key) ?? null,
      set: async (key: string, value: string[]): Promise<void> => void store.set(key, value),
    };
    const first = service({ known: [PARENT] });
    await resolveOrigin({ stop: STOP, baseUrl: BASE, mvgBaseUrl: MVG, fetchImpl: first.fetchImpl, cache });
    expect(store.size).toBe(1);

    // A fresh process, and a service that would now answer differently: the
    // cached answer is what comes back, which is the point of caching a fact
    // about the data rather than about the moment.
    clearOriginCache();
    const second = service({ known: [] });
    const resolved = await resolveOrigin({
      stop: STOP,
      baseUrl: BASE,
      mvgBaseUrl: MVG,
      fetchImpl: second.fetchImpl,
      cache,
    });
    expect(resolved.level).toBe('parent');
    expect(second.urls).toHaveLength(0);
  });

  test('a network failure is never mistaken for a missing stop', async () => {
    const { fetchImpl } = mockFetch(() => new Response('upstream is unwell', { status: 503 }));
    await expect(resolveOrigin({ stop: STOP, baseUrl: BASE, mvgBaseUrl: MVG, fetchImpl })).rejects.toThrow(/503/);
    // Nothing was remembered, so the next attempt asks again rather than
    // treating a bad minute as a permanent fact about the configuration.
    const good = service({ known: [PARENT] });
    const resolved = await resolveOrigin({ stop: STOP, baseUrl: BASE, mvgBaseUrl: MVG, fetchImpl: good.fetchImpl });
    expect(resolved.level).toBe('parent');
  });
});
