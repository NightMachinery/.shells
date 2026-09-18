import { describe, expect, test } from 'bun:test';
import { chain } from '../src/backends/chain.ts';
import { createMvgBackend } from '../src/backends/mvg.ts';
import { createTransitousBackend } from '../src/backends/transitous.ts';
import { failingFetch, FIXTURE_NOW, FIXTURE_WINDOW, fixture, mockFetch } from './helpers.ts';

const SYNTHETIC_STOP = 'de:00000:1';

describe('falling back', () => {
  test('the fallback answers when the primary throws, and its rows say so', async () => {
    const page1 = await fixture<unknown>('transitous-stoptimes-page1.json');
    const page2 = await fixture<unknown>('transitous-stoptimes-page2.json');

    const broken = failingFetch('synthetic primary outage');
    const working = mockFetch((_url, call) => (call === 0 ? page1 : page2));

    const primary = createMvgBackend({
      fetchImpl: broken.fetchImpl,
      baseUrl: 'https://example.invalid/primary',
      now: () => FIXTURE_NOW,
    });
    const fallback = createTransitousBackend({
      fetchImpl: working.fetchImpl,
      baseUrl: 'https://example.invalid/fallback',
    });
    const chained = chain(primary, fallback);

    const rows = await chained.departures(SYNTHETIC_STOP, FIXTURE_WINDOW);

    expect(rows.length).toBeGreaterThan(0);
    expect(new Set(rows.map((row) => row.backend))).toEqual(new Set(['transitous']));
    const outcome = chained.outcomes.get(SYNTHETIC_STOP);
    expect(outcome?.backend).toBe('transitous');
    expect(outcome?.error).toContain('synthetic primary outage');
    // No retry: the primary is asked exactly once.
    expect(broken.urls).toHaveLength(1);
  });

  test('the primary is used, and recorded, when it works', async () => {
    const rows = await fixture<unknown[]>('mvg-departures.json');
    const working = mockFetch(() => rows);
    const never = failingFetch('the fallback must not be reached');

    const chained = chain(
      createMvgBackend({ fetchImpl: working.fetchImpl, baseUrl: 'https://example.invalid/primary', now: () => FIXTURE_NOW }),
      createTransitousBackend({ fetchImpl: never.fetchImpl, baseUrl: 'https://example.invalid/fallback' }),
    );

    const result = await chained.departures(SYNTHETIC_STOP, FIXTURE_WINDOW);
    expect(result.every((row) => row.backend === 'mvg')).toBe(true);
    expect(chained.outcomes.get(SYNTHETIC_STOP)?.backend).toBe('mvg');
    expect(chained.outcomes.get(SYNTHETIC_STOP)?.error).toBeUndefined();
    expect(never.urls).toHaveLength(0);
  });
});

describe('passing a per-call narrowing through the chain', () => {
  test('it reaches the primary when the primary answers', async () => {
    const rows = await fixture<unknown[]>('mvg-departures.json');
    const working = mockFetch(() => rows);
    const never = failingFetch('the fallback must not be reached');

    const chained = chain(
      createMvgBackend({ fetchImpl: working.fetchImpl, baseUrl: 'https://example.invalid/primary', now: () => FIXTURE_NOW }),
      createTransitousBackend({ fetchImpl: never.fetchImpl, baseUrl: 'https://example.invalid/fallback' }),
    );

    const result = await chained.departures(SYNTHETIC_STOP, FIXTURE_WINDOW, { transportTypes: ['UBAHN'] });

    expect(working.urls.length).toBeGreaterThan(0);
    for (const url of working.urls) {
      expect(new URL(url).searchParams.get('transportTypes')).toBe('UBAHN');
    }
    expect(new Set(result.map((row) => row.mode))).toEqual(new Set(['UBAHN']));
  });

  test('it reaches the fallback when the primary throws', async () => {
    const page1 = await fixture<unknown>('transitous-stoptimes-page1.json');
    const page2 = await fixture<unknown>('transitous-stoptimes-page2.json');
    const broken = failingFetch('synthetic primary outage');
    const working = mockFetch((_url, call) => (call === 0 ? page1 : page2));

    const chained = chain(
      createMvgBackend({ fetchImpl: broken.fetchImpl, baseUrl: 'https://example.invalid/primary', now: () => FIXTURE_NOW }),
      createTransitousBackend({ fetchImpl: working.fetchImpl, baseUrl: 'https://example.invalid/fallback' }),
    );

    const rows = await chained.departures(SYNTHETIC_STOP, FIXTURE_WINDOW, { transportTypes: ['UBAHN'] });

    expect(rows.length).toBeGreaterThan(0);
    expect(new Set(rows.map((row) => row.mode))).toEqual(new Set(['UBAHN']));
  });
});
