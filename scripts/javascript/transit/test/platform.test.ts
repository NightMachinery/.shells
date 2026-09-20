import { describe, expect, test } from 'bun:test';
import { groupPlatform, platformOf } from '../src/page/platform.ts';
import { parseTrip } from '../src/trip.ts';
import type { Departure } from '../src/model.ts';
import { FIXTURE_NOW } from './helpers.ts';

const STOP = 'de:00000:1';

function row(overrides: Partial<Departure> = {}): Departure {
  return {
    line: 'S3',
    mode: 'SBAHN',
    destination: 'Nordweg',
    planned: FIXTURE_NOW,
    realtime: FIXTURE_NOW,
    delayMin: 0,
    cancelled: false,
    sev: false,
    platform: null,
    direction: 'H',
    backend: 'mvg',
    stop: STOP,
    realtimeKnown: true,
    ...overrides,
  };
}

const call = (platform: string | null) => ({ stopId: STOP, name: 'Here', arrivalMs: FIXTURE_NOW, departureMs: FIXTURE_NOW, platform });

describe('which source a platform comes from', () => {
  test('the row’s own feed answers when it published one', () => {
    expect(platformOf(row({ platform: '2', platformGuess: '9' }), call('11'))).toBe('2');
  });

  test('the aggregator’s row for the same run answers when it did not', () => {
    expect(platformOf(row({ platformGuess: '9' }), call('11'))).toBe('9');
  });

  test('the run’s own record is the last word', () => {
    expect(platformOf(row(), call('11'))).toBe('11');
  });

  test('an unpublished platform stays unpublished rather than becoming a guess', () => {
    expect(platformOf(row())).toBeNull();
    expect(platformOf(row({ platform: '' }), call(null))).toBeNull();
  });

  test('a run’s record carries the track the aggregator publishes for it', () => {
    const calls = parseTrip({
      legs: [
        {
          from: { stopId: `de-DELFI_${STOP}`, name: 'Here', departure: '2026-01-01T08:00:00Z', track: '7' },
          to: { stopId: 'de-DELFI_de:00000:2', name: 'Next', arrival: '2026-01-01T08:10:00Z', scheduledTrack: '3' },
        },
      ],
    });
    expect(calls.map((entry) => entry.platform)).toEqual(['7', '3']);
  });
});

describe('the platform a strip group leaves from', () => {
  test('is the one most of its times use', () => {
    const rows = [...Array(9)].map(() => row({ platform: '2' }));
    rows.push(row({ platform: '5' }));
    expect(groupPlatform(rows)).toBe('2');
  });

  test('counts a borrowed platform the same as a published one', () => {
    expect(groupPlatform([row({ platformGuess: '4' }), row({ platformGuess: '4' }), row({ platform: '1' })])).toBe('4');
  });

  test('is nothing at all when no time in the group has one', () => {
    expect(groupPlatform([row(), row()])).toBeNull();
  });

  test('is the earliest when two platforms are used equally often', () => {
    expect(groupPlatform([row({ platform: '3' }), row({ platform: '8' })])).toBe('3');
  });
});
