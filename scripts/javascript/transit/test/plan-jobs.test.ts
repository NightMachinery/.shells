import { describe, expect, test } from 'bun:test';
import { planJobs } from '../src/page/commute.ts';
import type { Board } from '../src/model.ts';
import type { ExportedBoard, ExportedConfig, ExportedProfile } from '../src/page/types.ts';

// Which boards a planning run searches for. The case under test is the page
// behind a planning server patching the server's answer: it plans the boards
// the server could not answer for, and must not plan the ones it could, which
// would be the phone doing the server's whole job a second time.

const STOP = 'de:00000:1';
const PLACE_STOP = 'de:00000:9';

function exported(title: string, commute: boolean): ExportedBoard {
  return {
    title,
    stops: [STOP],
    modes: null,
    lines: null,
    direction: null,
    via: null,
    destinations: null,
    walk_minutes: 5,
    walk_minutes_by_stop: null,
    stop_labels: null,
    commute,
    connection: null,
  };
}

const PROFILE: ExportedProfile = {
  key: 'home',
  title: 'Home',
  boards: [exported('a', true), exported('b', false), exported('c', true), exported('d', true)],
};

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
  profiles: [PROFILE],
  places: [{ name: 'there', label: 'There', lat: null, lon: null, stop: PLACE_STOP }],
};

function board(title: string): Board {
  return { title, stops: [STOP], backend: 'transitous', departures: [], walkMinutes: 5 };
}

const BOARDS = PROFILE.boards.map((entry) => board(entry.title));

describe('planJobs', () => {
  test('plans every commute board when not told otherwise', () => {
    const jobs = planJobs({ config: CONFIG, profile: PROFILE, boards: BOARDS, destinationKey: 'there' });
    expect(jobs.map((job) => job.index)).toEqual([0, 2, 3]);
  });

  test('told which boards, plans only those, and never a board that is not a commute board', () => {
    const jobs = planJobs({
      config: CONFIG,
      profile: PROFILE,
      boards: BOARDS,
      destinationKey: 'there',
      onlyIndexes: new Set([1, 3]),
    });
    expect(jobs.map((job) => job.index)).toEqual([3]);
  });
});
