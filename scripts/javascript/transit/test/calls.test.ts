import { beforeEach, describe, expect, test } from 'bun:test';
import {
  CALLS_RETRY_MS,
  callsExpanded,
  clearCalls,
  configureCalls,
  expandCalls,
  onwardCalls,
  peekCalls,
} from '../src/page/calls.ts';
import { clearTripCache } from '../src/trip.ts';
import type { Departure } from '../src/model.ts';
import { mockFetch } from './helpers.ts';

const STOP = 'de:00000:10';
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

const RUN = {
  legs: [
    {
      from: { stopId: `de-DELFI_${STOP}:6:6`, name: 'Umstiegspunkt', departure: '2026-01-01T08:00:00Z' },
      intermediateStops: [
        { stopId: 'de-DELFI_de:00000:100:1:1', name: 'Hauptplatz', arrival: '2026-01-01T08:12:00Z', departure: '2026-01-01T08:13:00Z' },
        { stopId: 'de-DELFI_de:00000:200:1:1', name: 'Ostweg', arrival: '2026-01-01T08:20:00Z', departure: '2026-01-01T08:21:00Z' },
      ],
      to: { stopId: 'de-DELFI_de:00000:300:1:1', name: 'Nordweg', arrival: '2026-01-01T08:30:00Z' },
    },
  ],
};

/** Wait for the module's coalesced wake-up and whatever it was waiting on. */
async function settle(): Promise<void> {
  for (let turn = 0; turn < 8; turn += 1) await Promise.resolve();
  await new Promise((resolve) => setTimeout(resolve, 0));
}

beforeEach(() => {
  clearCalls();
  clearTripCache();
});

describe('asking where a departure goes after it leaves', () => {
  test('a row that carries its own trip identifier is asked directly', async () => {
    const { fetchImpl, urls } = mockFetch(() => RUN);
    let woke = 0;
    configureCalls({ baseUrl: BASE, fetchImpl, rows: async () => [], onLoaded: () => (woke += 1) });

    expect(onwardCalls('k', row({ tripId: 'run' })).kind).toBe('loading');
    await settle();
    const state = onwardCalls('k', row({ tripId: 'run' }));
    expect(state.kind).toBe('ready');
    expect(state.kind === 'ready' && state.calls.map((call) => call.name)).toEqual(['Hauptplatz', 'Ostweg', 'Nordweg']);
    expect(woke).toBe(1);
    // The stop the reader is standing at is not where this train goes next, and
    // nothing asked the aggregator for a stop it did not need.
    expect(urls.filter((url) => url.includes('stoptimes')).length).toBe(0);
  });

  test('a row with no identifier borrows one from the aggregator', async () => {
    const { fetchImpl } = mockFetch(() => RUN);
    configureCalls({
      baseUrl: BASE,
      fetchImpl,
      rows: async () => [row({ backend: 'transitous', tripId: 'run' })],
      onLoaded: () => {},
    });
    onwardCalls('k', row());
    await settle();
    expect(onwardCalls('k', row()).kind).toBe('ready');
  });

  test('a run nothing identifies settles as unknown and is not asked twice', async () => {
    let asks = 0;
    const { fetchImpl } = mockFetch(() => RUN);
    configureCalls({
      baseUrl: BASE,
      fetchImpl,
      rows: async () => {
        asks += 1;
        return [];
      },
      onLoaded: () => {},
    });
    onwardCalls('k', row());
    await settle();
    expect(onwardCalls('k', row()).kind).toBe('unknown');
    onwardCalls('k', row());
    await settle();
    expect(asks).toBe(1);
  });

  test('peeking starts nothing', async () => {
    let asks = 0;
    const { fetchImpl } = mockFetch(() => RUN);
    configureCalls({
      baseUrl: BASE,
      fetchImpl,
      rows: async () => {
        asks += 1;
        return [];
      },
      onLoaded: () => {},
    });
    expect(peekCalls('k')).toBeUndefined();
    await settle();
    expect(asks).toBe(0);
  });

  test('rows landing together cost one redraw, not one each', async () => {
    const { fetchImpl } = mockFetch(() => RUN);
    let woke = 0;
    configureCalls({ baseUrl: BASE, fetchImpl, rows: async () => [], onLoaded: () => (woke += 1) });
    for (let index = 0; index < 3; index += 1) onwardCalls(`k${index}`, row({ tripId: `run${index}` }));
    await settle();
    expect(woke).toBe(1);

    // A board's worth of them arrives in as many batches as the lookup gate
    // allows in the air at once, which is a redraw per batch and not per row.
    woke = 0;
    for (let index = 0; index < 24; index += 1) onwardCalls(`n${index}`, row({ tripId: `other${index}` }));
    await settle();
    expect(woke).toBeGreaterThan(0);
    expect(woke).toBeLessThan(12);
  });

  test('the whole list is asked for once and stays asked for', () => {
    expect(callsExpanded('k')).toBe(false);
    expandCalls('k');
    expect(callsExpanded('k')).toBe(true);
  });
});

describe('why a run has no onward calls', () => {
  /**
   * One sentence per cause. "Not published" used to cover all of these, and a
   * reader holding a phone could not tell a station the aggregator does not
   * carry from a run it does not list from a network that was not there.
   */
  test('a stop the aggregator lists nothing for says so, rather than blaming the run', async () => {
    configureCalls({ baseUrl: BASE, rows: async () => [], onLoaded: () => {} });
    onwardCalls('k', row());
    await settle();
    expect(peekCalls('k')).toEqual({ kind: 'unknown', gap: 'no-stop-times' });
  });

  test('a stop it does list, with nothing that is this departure, is a different sentence', async () => {
    configureCalls({
      baseUrl: BASE,
      // A real run at this stop, four minutes away and under another name, so
      // no tier of the matcher will take it.
      rows: async () => [
        row({ backend: 'transitous', tripId: 'other', line: 'S9', destination: 'Ostweg', planned: AT + 240_000, realtime: AT + 240_000 }),
      ],
      onLoaded: () => {},
    });
    onwardCalls('k', row());
    await settle();
    expect(peekCalls('k')).toEqual({ kind: 'unknown', gap: 'no-match' });
  });

  test('a run that is identified and has no published sequence is a third', async () => {
    const { fetchImpl } = mockFetch(() => ({ legs: [] }));
    configureCalls({ baseUrl: BASE, fetchImpl, rows: async () => [], onLoaded: () => {} });
    onwardCalls('k', row({ tripId: 'run' }));
    await settle();
    expect(peekCalls('k')).toEqual({ kind: 'unknown', gap: 'no-sequence' });
  });

  test('a request that never arrives is not a claim about the run at all', async () => {
    configureCalls({
      baseUrl: BASE,
      rows: async () => {
        throw new Error('synthetic network failure');
      },
      onLoaded: () => {},
    });
    onwardCalls('k', row());
    await settle();
    expect(peekCalls('k')).toEqual({ kind: 'unknown', gap: 'unreachable' });
  });
});

describe('asking a second time', () => {
  test('one failed request is retried before anything is concluded from it', async () => {
    let attempts = 0;
    configureCalls({
      baseUrl: BASE,
      rows: async () => {
        attempts += 1;
        if (attempts === 1) throw new Error('synthetic blip');
        return [];
      },
      onLoaded: () => {},
    });
    onwardCalls('k', row());
    await settle();
    // The second attempt answered, so the verdict is about the stop rather than
    // about the network.
    expect(peekCalls('k')).toEqual({ kind: 'unknown', gap: 'no-stop-times' });
    expect(attempts).toBe(2);
  });

  test('a network failure may be asked about again, and not on every render', async () => {
    let asks = 0;
    configureCalls({
      baseUrl: BASE,
      rows: async () => {
        asks += 1;
        throw new Error('synthetic network failure');
      },
      onLoaded: () => {},
    });
    onwardCalls('k', row());
    await settle();
    // The attempt and its one retry, and nothing more while the interval runs.
    expect(asks).toBe(2);
    for (let render = 0; render < 5; render += 1) onwardCalls('k', row());
    await settle();
    expect(asks).toBe(2);
    // The interval is a real one, so the answer is not permanent either.
    expect(CALLS_RETRY_MS).toBeGreaterThan(0);
  });
});
