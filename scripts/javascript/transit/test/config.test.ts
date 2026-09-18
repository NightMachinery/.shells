import { describe, expect, test } from 'bun:test';
import {
  ConfigError,
  looksLikeStopId,
  parseConfig,
  resolveBoardTarget,
  resolveProfile,
  type Config,
} from '../src/config.ts';
import { boardsDocument, configExportDocument } from '../src/json.ts';
import { planTargets } from '../src/targets.ts';

const PATH = '/synthetic/address.toml';

function issuesOf(raw: unknown): string[] {
  try {
    parseConfig(raw, PATH);
  } catch (error) {
    if (error instanceof ConfigError) return error.issues;
    throw error;
  }
  throw new Error('expected the config to be rejected');
}

describe('config validation', () => {
  test('names the profile, the board index and the field', () => {
    const issues = issuesOf({
      profiles: {
        alpha: {
          title: 'Alpha',
          boards: [{ title: 'first', stops: ['de:00000:1'] }, { title: 'second' }],
        },
      },
    });
    expect(issues).toHaveLength(1);
    expect(issues[0]).toContain('profiles.alpha.boards[1].stops');
  });

  test('reports every problem, not only the first', () => {
    const issues = issuesOf({
      defaults: { horizon_minutes: -5, backend: 'nonesuch' },
      profiles: {
        alpha: {
          title: 'Alpha',
          boards: [
            { title: '', stops: [] },
            { title: 'second', stops: ['de:00000:1'], direction: 'X', walk_minutes: -1 },
          ],
        },
      },
    });
    const joined = issues.join('\n');
    expect(issues.length).toBeGreaterThanOrEqual(6);
    expect(joined).toContain('defaults.horizon_minutes');
    expect(joined).toContain('defaults.backend');
    expect(joined).toContain('profiles.alpha.boards[0].title');
    expect(joined).toContain('profiles.alpha.boards[0].stops');
    expect(joined).toContain('profiles.alpha.boards[1].direction');
    expect(joined).toContain('profiles.alpha.boards[1].walk_minutes');
  });

  test('a per-stop walking time for a stop the board does not serve is an error', () => {
    const issues = issuesOf({
      profiles: {
        alpha: {
          title: 'Alpha',
          boards: [
            {
              title: 'first',
              stops: ['de:00000:1'],
              walk_minutes_by_stop: { 'de:00000:1': 3, 'de:00000:9': 4 },
            },
          ],
        },
      },
    });
    expect(issues.join('\n')).toContain('profiles.alpha.boards[0].walk_minutes_by_stop');
    expect(issues.join('\n')).toContain('de:00000:9');
  });

  test('accepts a well-formed document and applies the defaults', () => {
    const config = parseConfig(
      {
        profiles: {
          alpha: {
            title: 'Alpha',
            boards: [{ title: 'first', stops: ['de:00000:1', 'de:00000:2'], walk_minutes_by_stop: { 'de:00000:2': 6 } }],
          },
        },
      },
      PATH,
    );
    expect(config.defaults.transportTypes).toContain('BAHN');
    expect(config.defaults.home).toBeNull();
    const board = config.profiles[0]?.boards[0];
    expect(board?.walkMinutes).toBe(0);
    expect(board?.walkMinutesByStop).toEqual({ 'de:00000:2': 6 });
    expect(board?.stopLabels).toBeUndefined();
  });

  test('a stop may be given as a table carrying a short label', () => {
    const config = parseConfig(
      {
        profiles: {
          alpha: {
            title: 'Alpha',
            boards: [{ title: 'first', stops: [{ id: 'de:00000:1', label: 'Varn' }, 'de:00000:2'] }],
          },
        },
      },
      PATH,
    );
    const board = config.profiles[0]?.boards[0];
    expect(board?.stops).toEqual(['de:00000:1', 'de:00000:2']);
    expect(board?.stopLabels).toEqual({ 'de:00000:1': 'Varn' });
  });

  test('a stop table without an id is rejected', () => {
    const issues = issuesOf({
      profiles: { alpha: { title: 'Alpha', boards: [{ title: 'first', stops: [{ label: 'Varn' }] }] } },
    });
    expect(issues.join('\n')).toContain('profiles.alpha.boards[0].stops');
  });
});

describe('the home alias', () => {
  function withHome(home: string): Config {
    return parseConfig(
      {
        defaults: { home },
        profiles: {
          alpha: { title: 'Alpha', boards: [{ title: 'first', stops: ['de:00000:1'] }] },
          beta: { title: 'Beta', boards: [{ title: 'first', stops: ['de:00000:2'] }] },
        },
      },
      PATH,
    );
  }

  test('pointing at a profile that does not exist is a validation error naming the field', () => {
    const issues = issuesOf({
      defaults: { home: 'nonesuch' },
      profiles: { alpha: { title: 'Alpha', boards: [{ title: 'first', stops: ['de:00000:1'] }] } },
    });
    expect(issues.join('\n')).toContain('defaults.home');
  });

  test('resolves the alias and reports what was asked for alongside what was rendered', () => {
    const config = withHome('beta');
    const profile = resolveProfile(config, 'home');
    expect(profile?.key).toBe('beta');

    const document = boardsDocument(profile?.key ?? null, 'home', [], Date.now()) as Record<string, unknown>;
    expect(document.profile).toBe('beta');
    expect(document.requested_profile).toBe('home');
  });

  test('an exact profile key always wins over the alias', () => {
    const config = parseConfig(
      {
        defaults: { home: 'beta' },
        profiles: {
          home: { title: 'Literal', boards: [{ title: 'first', stops: ['de:00000:1'] }] },
          beta: { title: 'Beta', boards: [{ title: 'first', stops: ['de:00000:2'] }] },
        },
      },
      PATH,
    );
    expect(resolveProfile(config, 'home')?.title).toBe('Literal');
  });

  test('no other name is aliased', () => {
    expect(resolveProfile(withHome('beta'), 'nonesuch')).toBeUndefined();
  });
});

describe('telling a profile key from a stop id', () => {
  const config = parseConfig(
    {
      defaults: { home: 'beta' },
      profiles: {
        alpha: { title: 'Alpha', boards: [{ title: 'first', stops: ['de:00000:1'] }] },
        beta: { title: 'Beta', boards: [{ title: 'first', stops: ['de:00000:2'] }] },
      },
    },
    PATH,
  );

  test('the shape test keys on the colon', () => {
    expect(looksLikeStopId('de:00000:1')).toBe(true);
    expect(looksLikeStopId('alpha')).toBe(false);
  });

  test('a known profile key resolves to that profile', () => {
    const target = resolveBoardTarget(config, ['alpha']);
    expect(target.kind).toBe('profile');
    if (target.kind === 'profile') expect(target.profile.key).toBe('alpha');
  });

  test('the alias resolves and records what was typed', () => {
    const target = resolveBoardTarget(config, ['home']);
    expect(target.kind).toBe('profile');
    if (target.kind === 'profile') {
      expect(target.profile.key).toBe('beta');
      expect(target.requested).toBe('home');
    }
  });

  test('stop-shaped arguments are taken as stops', () => {
    const target = resolveBoardTarget(config, ['de:00000:1', 'de:00000:2']);
    expect(target.kind).toBe('stops');
    if (target.kind === 'stops') expect(target.stops).toHaveLength(2);
  });

  test('a name that is neither is rejected, and the error lists the configured keys', () => {
    const target = resolveBoardTarget(config, ['nonesuch']);
    expect(target.kind).toBe('error');
    if (target.kind === 'error') {
      expect(target.message).toContain('unknown profile: nonesuch');
      expect(target.message).toContain('alpha');
      expect(target.message).toContain('beta');
    }
  });

  test('a stop-shaped argument still reaches the backend, wrong or not', () => {
    // A genuinely bad identifier must still produce the upstream not-found
    // rather than being swallowed here.
    expect(resolveBoardTarget(config, ['de:99999:9999']).kind).toBe('stops');
  });
});

describe('places', () => {
  const profiles = { alpha: { title: 'Alpha', boards: [{ title: 'b', stops: ['de:00000:1'], walk_minutes: 4 }] } };

  test('a stop place needs no coordinates and exports with a final walk of nothing', () => {
    const config = parseConfig({ profiles, places: { station: { stop: 'de:00000:7', label: 'Marienplatz' } } }, PATH);
    expect(config.places.station).toEqual({ name: 'station', label: 'Marienplatz', lat: null, lon: null, stop: 'de:00000:7' });
    const document = configExportDocument(config, { mvgBaseUrl: 'a', transitousBaseUrl: 'b' }) as {
      places?: Array<{ name: string; stop: string | null; lat: number | null }>;
    };
    // Exported even without `--with-places`: a stop id is not geography, and
    // this document is already a list of stop ids.
    expect(document.places).toEqual([{ name: 'station', label: 'Marienplatz', lat: null, lon: null, stop: 'de:00000:7' }]);
    expect(planTargets(config.places.station as never, [])[0]?.walkMinutes).toBe(0);
  });

  test('a doorstep keeps its coordinates out of the document unless asked for', () => {
    const config = parseConfig({ profiles, places: { alpha: { lat: 1, lon: 2 } } }, PATH);
    const withheld = configExportDocument(config, { mvgBaseUrl: 'a', transitousBaseUrl: 'b' }) as { places?: unknown };
    expect(withheld.places).toBeUndefined();
    const shared = configExportDocument(config, { withPlaces: true, mvgBaseUrl: 'a', transitousBaseUrl: 'b' }) as {
      places?: Array<{ lat: number | null }>;
    };
    expect(shared.places?.[0]?.lat).toBe(1);
  });

  test('a place cannot be both a stop and a coordinate', () => {
    expect(issuesOf({ profiles, places: { odd: { stop: 'de:00000:7', lat: 1, lon: 2 } } })).toContain(
      'places.odd: declare either stop, or lat and lon, not both',
    );
  });

  test('a profile may name the destinations it offers, and they must exist', () => {
    const config = parseConfig(
      { profiles: { alpha: { ...profiles.alpha, destinations: ['station'] } }, places: { station: { stop: 'de:00000:7' } } },
      PATH,
    );
    expect(config.profiles[0]?.destinations).toEqual(['station']);
    expect(issuesOf({ profiles: { alpha: { ...profiles.alpha, destinations: ['nowhere'] } } })).toContain(
      'profiles.alpha.destinations: no place named nowhere is declared',
    );
  });

  test('a walked minute cannot be worth less than a ridden one', () => {
    expect(issuesOf({ profiles, defaults: { walk_weight: 0.5 } })).toContain(
      'defaults.walk_weight: must be a number of at least 1',
    );
    expect(parseConfig({ profiles, defaults: { walk_weight: 3 } }, PATH).defaults.walkWeight).toBe(3);
  });
});
