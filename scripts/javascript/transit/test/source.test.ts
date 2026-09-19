import { describe, expect, test } from 'bun:test';
import { createServerSource, createSwitchingSource, type SourceNote } from '../src/page/source.ts';
import { planKey, type BoardRoutes, type PlanProfileOptions, type ProfileRoutes } from '../src/page/commute.ts';
import { WIRE_VERSION, type WireProfileAnswer } from '../src/page/wire.ts';
import type { Board } from '../src/model.ts';
import type { PlannedRow } from '../src/plan.ts';
import type { ExportedConfig, ExportedProfile } from '../src/page/types.ts';

// The two sources are tested against an injected `fetchImpl` and an injected
// `now`, never against the network: `directSource` is exercised only for its
// identity, `kind === 'direct'`, since calling it for real means asking the
// live departure backends.

const NOW = 1_700_000_000_000;
const SYNTHETIC_STOP = 'de:00000:1';
const DEST_A = 'de:00000:9';
const DEST_B = 'de:00000:8';

const CONFIG: ExportedConfig = {
  schema_version: 1,
  defaults: {
    horizon_minutes: 60,
    backend: 'transitous',
    fallback: null,
    transport_types: ['BUS'],
    timezone: 'Europe/Berlin',
    home: null,
  },
  backends: { mvg_base_url: 'https://example.invalid/mvg', transitous_base_url: 'https://example.invalid/transitous' },
  profiles: [],
};

const PROFILE: ExportedProfile = {
  key: 'home',
  title: 'Home',
  boards: [
    {
      title: 'Board A',
      stops: [SYNTHETIC_STOP],
      modes: null,
      lines: null,
      direction: null,
      via: null,
      destinations: null,
      walk_minutes: 5,
      walk_minutes_by_stop: null,
      stop_labels: null,
      commute: true,
      connection: null,
    },
  ],
};

const BOARD: Board = {
  title: 'Board A',
  stops: [SYNTHETIC_STOP],
  backend: 'transitous',
  departures: [],
  walkMinutes: 5,
};

function row(id: string): PlannedRow {
  return { departure: { line: id } as never, options: [], best: { exitStop: id } as never };
}

function wireAnswer(overrides: Partial<WireProfileAnswer> = {}): WireProfileAnswer {
  return {
    v: WIRE_VERSION,
    at: NOW,
    profileKey: PROFILE.key,
    startMs: NOW,
    horizonMinutes: 60,
    backends: ['transitous'],
    boards: [BOARD],
    routes: null,
    destinationKey: '',
    ...overrides,
  };
}

function planOptions(destinationKey: string, boards: Board[] = [BOARD]): PlanProfileOptions {
  return {
    config: CONFIG,
    profileKey: PROFILE.key,
    profile: PROFILE,
    boards,
    destinationKey,
    startMs: NOW,
    horizonMinutes: 60,
  };
}

/**
 * Build an injectable `fetch`, typed exactly like the global rather than the
 * package's own narrower `FetchLike` (see `test/helpers.ts`): `source.ts`
 * takes its `fetchImpl` as `typeof fetch`, so a `FetchLike` mock does not fit
 * it without a cast.
 */
function mockFetch(handler: (url: string, call: number) => Response | unknown): { fetchImpl: typeof fetch; urls: string[] } {
  const urls: string[] = [];
  const fetchImpl: typeof fetch = async (input) => {
    const url = String(input);
    const index = urls.length;
    urls.push(url);
    const body = handler(url, index);
    if (body instanceof Response) return body;
    return new Response(JSON.stringify(body), { status: 200, headers: { 'content-type': 'application/json' } });
  };
  return { fetchImpl, urls };
}

/**
 * `createSwitchingSource`'s injected `fetchImpl` only ever reaches the calls
 * this module makes itself, against the planning server's own endpoints. The
 * `direct` fallback is the page's ordinary behaviour, `fetchProfile` from
 * `./data.ts`, and that asks the real global `fetch` with no way to hand it a
 * substitute through this API; `makeBackends` builds the departure backends
 * with no `fetchImpl` of their own. So for the two cases below that actually
 * walk the fallback all the way through, the global is swapped out for the
 * duration of one call and restored immediately after in a `finally`, which
 * keeps the test off the network without changing `source.ts` itself.
 */
async function throughDirect<T>(fetchImpl: typeof fetch, run: () => Promise<T>): Promise<T> {
  const original = globalThis.fetch;
  globalThis.fetch = fetchImpl;
  try {
    return await run();
  } finally {
    globalThis.fetch = original;
  }
}

describe('createServerSource', () => {
  const base = 'https://example.invalid/api';

  test('fetchProfile asks the server exactly once and reports every board ready', async () => {
    const { fetchImpl, urls } = mockFetch(() => wireAnswer({ boards: [BOARD, BOARD] }));
    const source = createServerSource({ base, fetchImpl });
    const seen: Array<[number, string]> = [];

    const result = await source.fetchProfile({
      config: CONFIG,
      profile: PROFILE,
      startMs: NOW,
      horizonMinutes: 60,
      onStatus: (index, status) => seen.push([index, status.kind]),
    });

    expect(urls).toHaveLength(1);
    expect(urls[0]).toMatch(new RegExp(`^${base}/profile/${PROFILE.key}\\?`));
    expect(result.boards).toHaveLength(2);
    expect(result.backends).toEqual(['transitous']);
    // Every board arrives at once, so a board left unreported would read to
    // the page as still loading; see the comment on this in source.ts.
    expect(seen).toEqual([
      [0, 'ready'],
      [1, 'ready'],
    ]);
  });

  test('reports the answer age from the response header, converted to milliseconds', async () => {
    const notes: SourceNote[] = [];
    const { fetchImpl } = mockFetch(
      () => new Response(JSON.stringify(wireAnswer()), { status: 200, headers: { 'x-answer-age': '3' } }),
    );
    const source = createServerSource({ base, fetchImpl, onNote: (note) => notes.push(note) });

    await source.fetchProfile({ config: CONFIG, profile: PROFILE, startMs: NOW, horizonMinutes: 60, onStatus: () => {} });

    expect(notes).toEqual([{ kind: 'server', ageMs: 3000 }]);
  });

  test('a planProfile asked right after fetchProfile for the same query is answered from the held response', async () => {
    const { fetchImpl, urls } = mockFetch(() =>
      wireAnswer({
        destinationKey: DEST_A,
        routes: [{ index: 0, destinationKey: DEST_A, answered: true, origin: null, rows: [] }],
      }),
    );
    const source = createServerSource({ base, fetchImpl });

    await source.fetchProfile({
      config: CONFIG,
      profile: PROFILE,
      startMs: NOW,
      horizonMinutes: 60,
      plan: { destinationKey: DEST_A },
      onStatus: () => {},
    });
    expect(urls).toHaveLength(1);

    const routes = await source.planProfile(planOptions(DEST_A));

    expect(urls).toHaveLength(1);
    expect(routes?.boards).toBeInstanceOf(Map);
  });

  test('a planProfile for a different destination is a different question, and asks again', async () => {
    const { fetchImpl, urls } = mockFetch(() =>
      wireAnswer({
        destinationKey: DEST_A,
        routes: [{ index: 0, destinationKey: DEST_A, answered: true, origin: null, rows: [] }],
      }),
    );
    const source = createServerSource({ base, fetchImpl });

    await source.fetchProfile({
      config: CONFIG,
      profile: PROFILE,
      startMs: NOW,
      horizonMinutes: 60,
      plan: { destinationKey: DEST_A },
      onStatus: () => {},
    });
    expect(urls).toHaveLength(1);

    await source.planProfile(planOptions(DEST_B));

    expect(urls).toHaveLength(2);
  });

  test('planProfile returns the previous answer unchanged, and asks nothing, when its key has not moved on', async () => {
    const { fetchImpl, urls } = mockFetch(() => wireAnswer());
    const source = createServerSource({ base, fetchImpl });
    const options = planOptions(DEST_A);
    const previous: ProfileRoutes = { boards: new Map(), at: NOW, destinationKey: DEST_A, stale: false, key: planKey(options) };

    const routes = await source.planProfile({ ...options, previous });

    expect(routes).toBe(previous);
    expect(urls).toHaveLength(0);
  });

  test('planProfile merges onto the previous answer: an answered board takes the new journey, an unanswered one keeps the old', async () => {
    const previous: ProfileRoutes = {
      boards: new Map<number, BoardRoutes>([
        [0, { rows: new Map([['a', row('old')]]), origin: null, destinationKey: DEST_A }],
        [1, { rows: new Map([['b', row('old')]]), origin: null, destinationKey: DEST_A }],
      ]),
      at: NOW - 60_000,
      destinationKey: DEST_A,
      // Stale, and deliberately keyed differently from what `options` below
      // produces: this stands for the plan already on screen when a fresh
      // answer comes in, not for an unrelated question nobody asked.
      stale: true,
      key: 'stale-key',
    };
    const { fetchImpl } = mockFetch(() =>
      wireAnswer({
        destinationKey: DEST_A,
        routes: [
          // Board 0 is answered, with a fresh journey for the same row: the
          // new one wins. Board 1 is not answered at all, which is what
          // `carryBoards` needs to leave its previous board untouched rather
          // than merging an empty answer over it.
          { index: 0, destinationKey: DEST_A, answered: true, origin: null, rows: [['a', row('new')]] },
          { index: 1, destinationKey: DEST_A, answered: false, origin: null, rows: [] },
        ],
      }),
    );
    const source = createServerSource({ base, fetchImpl });

    const routes = await source.planProfile({ ...planOptions(DEST_A, [BOARD, BOARD]), previous });

    expect(routes?.boards.get(0)?.rows.get('a')?.best?.exitStop).toBe('new');
    expect(routes?.boards.get(1)?.rows.get('b')?.best?.exitStop).toBe('old');
  });

  test('a non-200 from the server rejects', async () => {
    const { fetchImpl } = mockFetch(() => new Response('nope', { status: 500 }));
    const source = createServerSource({ base, fetchImpl });

    await expect(
      source.fetchProfile({ config: CONFIG, profile: PROFILE, startMs: NOW, horizonMinutes: 60, onStatus: () => {} }),
    ).rejects.toThrow();
  });

  test('an answer speaking a different wire version rejects', async () => {
    const { fetchImpl } = mockFetch(() => wireAnswer({ v: WIRE_VERSION + 1 }));
    const source = createServerSource({ base, fetchImpl });

    await expect(
      source.fetchProfile({ config: CONFIG, profile: PROFILE, startMs: NOW, horizonMinutes: 60, onStatus: () => {} }),
    ).rejects.toThrow();
  });
});

describe('createSwitchingSource', () => {
  const base = 'https://example.invalid/api';

  test('starts on the direct source, before any probe has run', () => {
    const source = createSwitchingSource({
      base,
      fetchImpl: async () => {
        throw new Error('a fresh switching source must not touch the network before it is asked to probe');
      },
    });

    expect(source.kind).toBe('direct');
    expect(source.state()).toEqual({ kind: 'direct', probedAt: null, fellBackAt: null, reason: null });
  });

  test('a health check that answers 200 moves the source onto the server', async () => {
    const { fetchImpl, urls } = mockFetch(() => new Response('{}', { status: 200 }));
    const source = createSwitchingSource({ base, fetchImpl });

    const outcome = await source.probe();

    expect(outcome).toBe('server');
    expect(source.state().kind).toBe('server');
    expect(source.state().reason).toBeNull();
    expect(urls).toEqual([`${base}/health`]);
  });

  test('a health check that answers 500 leaves the source on direct, with a reason', async () => {
    const { fetchImpl } = mockFetch(() => new Response('bad gateway', { status: 500 }));
    const source = createSwitchingSource({ base, fetchImpl });

    const outcome = await source.probe();

    expect(outcome).toBe('direct');
    expect(source.state().kind).toBe('direct');
    expect(source.state().reason).toBe('health said 500');
  });

  test('a probe against a health check that never answers gives up at its own deadline', async () => {
    let release: () => void = () => {};
    const fetchImpl: typeof fetch = (_input, init) =>
      new Promise<Response>((resolve, reject) => {
        release = () => resolve(new Response('{}', { status: 200 }));
        const signal = init?.signal;
        if (signal?.aborted) {
          reject(signal.reason);
          return;
        }
        signal?.addEventListener('abort', () => reject(signal.reason));
      });
    const source = createSwitchingSource({ base, fetchImpl, probeMs: 50 });

    const outcome = await source.probe();

    expect(outcome).toBe('direct');
    expect(source.state().reason).not.toBeNull();
    // The probe has already given up on this request by the time we get
    // here; releasing it now is only cleanup, so nothing is left pending.
    release();
  });

  test('a server reached by an earlier probe falls back to direct when a later request fails, and says why', async () => {
    let serverCalls = 0;
    const fetchImpl: typeof fetch = async (input) => {
      const url = String(input);
      if (url === `${base}/health`) return new Response('{}', { status: 200 });
      if (url.startsWith(`${base}/profile/`)) {
        serverCalls += 1;
        return new Response('service unavailable', { status: 503 });
      }
      // Whatever the direct fallback then asks of the outside world (the real
      // departure backends) is answered with a failure too. `fetchProfile`
      // treats a backend failure as a board with no departures rather than as
      // a reason to reject the whole call, so this is enough to prove the
      // fallback resolves without a single request reaching a real network.
      return new Response('not found', { status: 404 });
    };
    const source = createSwitchingSource({ base, fetchImpl });
    await source.probe();
    expect(source.state().kind).toBe('server');

    const result = await throughDirect(fetchImpl, () =>
      source.fetchProfile({ config: CONFIG, profile: PROFILE, startMs: NOW, horizonMinutes: 60, onStatus: () => {} }),
    );

    expect(result.boards.length).toBeGreaterThan(0);
    expect(source.state().kind).toBe('direct');
    expect(source.state().reason).not.toBeNull();
    expect(serverCalls).toBe(1);
  });

  test('once fallen back, it does not probe again before reprobeMs has passed, and does after', async () => {
    let now = NOW;
    let healthCalls = 0;
    const fetchImpl: typeof fetch = async (input) => {
      const url = String(input);
      if (url === `${base}/health`) {
        healthCalls += 1;
        return new Response('bad gateway', { status: 502 });
      }
      return new Response('not found', { status: 404 });
    };
    const source = createSwitchingSource({ base, fetchImpl, now: () => now, reprobeMs: 1000 });
    await source.probe();
    expect(healthCalls).toBe(1);
    const fellBackAt = source.state().fellBackAt;
    expect(fellBackAt).not.toBeNull();

    await throughDirect(fetchImpl, () =>
      source.fetchProfile({ config: CONFIG, profile: PROFILE, startMs: NOW, horizonMinutes: 60, onStatus: () => {} }),
    );
    // Barely a moment later, by the injected clock: too soon to ask again.
    expect(healthCalls).toBe(1);

    now = (fellBackAt as number) + 1001;
    await throughDirect(fetchImpl, () =>
      source.fetchProfile({ config: CONFIG, profile: PROFILE, startMs: NOW, horizonMinutes: 60, onStatus: () => {} }),
    );
    expect(healthCalls).toBe(2);
  });
});
