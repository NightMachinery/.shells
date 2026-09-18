import { describe, expect, test } from 'bun:test';
import {
  ConfigError,
  looksLikeStopId,
  parseConfig,
  resolveBoardTarget,
  resolveProfile,
  type Config,
} from '../src/config.ts';
import { boardsDocument } from '../src/json.ts';

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
