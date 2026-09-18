import {
  ALL_MODES,
  isMode,
  type BoardConfig,
  type ConnectionConfig,
  type Direction,
  type Mode,
  type Profile,
} from './model.ts';

// Every default the package has lives here, named. Nothing else may hardcode
// one: a knob with two homes is a knob that disagrees with itself.

/** Environment variable that overrides the config location outright. */
export const CONFIG_PATH_ENV = 'ADDRESS_CONFIG';
/** Looked for under the home directory when the environment variable is unset. */
export const DEFAULT_CONFIG_RELATIVE_PATH = '.address-config/address.toml';

/**
 * The transit modes a journey may use, as the planner's own vocabulary names
 * them. An allow-list rather than a deny-list, because that is the shape the
 * parameter takes: leaving it unset allows everything.
 *
 * Long-distance rail is deliberately absent. The planner routes over the whole
 * national timetable and will happily put an inter-city or high-speed train in
 * the middle of a commute, which is a fine journey and not one a local ticket
 * covers, so the recommendation would be wrong in the way that costs money.
 * `defaults.plan_modes` exists for a reader who does hold such a ticket.
 */
export const DEFAULT_PLAN_MODES: readonly string[] = ['SUBURBAN', 'SUBWAY', 'TRAM', 'BUS', 'REGIONAL_RAIL'];

export const DEFAULT_HORIZON_MINUTES = 120;
export const DEFAULT_BACKEND = 'mvg';
export const DEFAULT_FALLBACK = 'transitous';
export const DEFAULT_TRANSPORT_TYPES: readonly Mode[] = ALL_MODES;
export const DEFAULT_TIMEZONE = 'Europe/Berlin';
export const DEFAULT_WALK_MINUTES = 0;

/**
 * What one minute on foot costs, measured in minutes on a vehicle.
 *
 * Two, which says a rider would rather sit for twenty minutes than walk for
 * eleven. It exists because ranking journeys by arrival alone produces advice
 * nobody follows: a train that arrives two minutes earlier and leaves you a
 * quarter of an hour from the door loses to one that arrives later at the stop
 * by your street, and only a weight says so. One turns the preference off and
 * ranks by arrival, which is what the planner does on its own.
 */
export const DEFAULT_WALK_WEIGHT = 2;

export const BACKEND_NAMES = ['mvg', 'transitous'] as const;
export type BackendName = (typeof BACKEND_NAMES)[number];

/**
 * The one aliased profile name. `board home` resolves through this when no
 * profile is literally called `home`; no other name is aliased, and profile
 * keys themselves are arbitrary short strings that this package never assumes
 * the spelling of.
 */
export const HOME_ALIAS = 'home';

export interface Defaults {
  horizonMinutes: number;
  backend: BackendName;
  /** Backend to fall back to, or `null` when the config disables falling back. */
  fallback: BackendName | null;
  transportTypes: Mode[];
  /**
   * Transit modes a journey plan may use, in the planner's own vocabulary.
   * Unset means the package's default, which leaves long-distance rail out.
   */
  planModes: string[];
  /** What a walked minute costs in ridden minutes, when journeys are ranked. */
  walkWeight: number;
  timezone: string;
  /** Profile key that the alias resolves to, or `null` when unset. */
  home: string | null;
}

/**
 * Somewhere a journey can end.
 *
 * Two kinds, because the two questions are different. A *coordinate place* is a
 * doorstep: the planner routes to the point and the final walk is whatever it
 * works out. A *stop place* is a station the reader is themselves travelling
 * to, named by its identifier: the journey ends when the vehicle does, the
 * final walk is zero, and it is offered from every profile rather than being
 * tied to one. Exactly one of the two is declared.
 */
export interface Place {
  /** The key it is declared under, and how the rest of the config names it. */
  name: string;
  /** What the picker calls it; the key when nothing else is given. */
  label: string | null;
  /** Set on a coordinate place. */
  lat: number | null;
  lon: number | null;
  /** Set on a stop place: the stop that is the destination. */
  stop: string | null;
}

/** Whether this place is a stop the reader travels to rather than a doorstep. */
export function isStopPlace(place: Place): boolean {
  return place.stop !== null;
}

/** What a place is called on screen. */
export function placeLabel(place: Place): string {
  return place.label ?? place.name;
}

export interface Config {
  /** Where this config was read from; useful in error messages. */
  path: string;
  defaults: Defaults;
  places: Record<string, Place>;
  profiles: Profile[];
}

/** Carries every problem found, not just the one that stopped the parse. */
export class ConfigError extends Error {
  readonly issues: string[];
  constructor(issues: string[]) {
    super(issues.join('\n'));
    this.name = 'ConfigError';
    this.issues = issues;
  }
}

function homeDir(): string {
  return process.env.HOME ?? '.';
}

/** `--config`, then the environment variable, then the conventional location. */
export function resolveConfigPath(explicit?: string): string {
  if (explicit !== undefined && explicit.length > 0) return explicit;
  const fromEnv = process.env[CONFIG_PATH_ENV];
  if (fromEnv !== undefined && fromEnv.length > 0) return fromEnv;
  return `${homeDir()}/${DEFAULT_CONFIG_RELATIVE_PATH}`;
}

function isTable(value: unknown): value is Record<string, unknown> {
  return typeof value === 'object' && value !== null && !Array.isArray(value);
}

function stringList(value: unknown): string[] | null {
  if (!Array.isArray(value)) return null;
  const out: string[] = [];
  for (const entry of value) {
    if (typeof entry !== 'string' || entry.trim().length === 0) return null;
    out.push(entry.trim());
  }
  return out;
}

/**
 * A board's `stops`, in either of the two accepted spellings.
 *
 * A bare string is the common case and stays the common case: most boards have
 * one stop and nothing to say about it. A table adds a label, which is the tag
 * a merged board puts on every row, and is worth having only because the
 * alternative on screen is a stop id. Both forms mix freely in one list.
 *
 * Returns null when the list is unusable, so the caller can report it with the
 * profile and board index it has and this function does not.
 */
export function parseStops(raw: unknown): { stops: string[]; labels: Record<string, string> } | null {
  if (!Array.isArray(raw) || raw.length === 0) return null;
  const stops: string[] = [];
  const labels: Record<string, string> = {};
  for (const entry of raw) {
    if (typeof entry === 'string') {
      const id = entry.trim();
      if (id.length === 0) return null;
      stops.push(id);
      continue;
    }
    if (!isTable(entry)) return null;
    const id = typeof entry.id === 'string' ? entry.id.trim() : '';
    if (id.length === 0) return null;
    stops.push(id);
    if (entry.label !== undefined) {
      const label = typeof entry.label === 'string' ? entry.label.trim() : '';
      if (label.length === 0) return null;
      labels[id] = label;
    }
  }
  return { stops, labels };
}

function parseDefaults(raw: unknown, issues: string[]): Defaults {
  const table = isTable(raw) ? raw : {};
  const defaults: Defaults = {
    horizonMinutes: DEFAULT_HORIZON_MINUTES,
    backend: DEFAULT_BACKEND,
    fallback: DEFAULT_FALLBACK,
    transportTypes: [...DEFAULT_TRANSPORT_TYPES],
    planModes: [...DEFAULT_PLAN_MODES],
    walkWeight: DEFAULT_WALK_WEIGHT,
    timezone: DEFAULT_TIMEZONE,
    home: null,
  };

  if (table.horizon_minutes !== undefined) {
    const value = table.horizon_minutes;
    if (typeof value !== 'number' || !Number.isFinite(value) || value <= 0) {
      issues.push('defaults.horizon_minutes: must be a positive number of minutes');
    } else {
      defaults.horizonMinutes = value;
    }
  }

  if (table.backend !== undefined) {
    const value = table.backend;
    if (typeof value !== 'string' || !(BACKEND_NAMES as readonly string[]).includes(value)) {
      issues.push(`defaults.backend: must be one of ${BACKEND_NAMES.join(', ')}`);
    } else {
      defaults.backend = value as BackendName;
    }
  }

  if (table.fallback !== undefined) {
    const value = table.fallback;
    if (value === false || value === '' || value === 'none') {
      defaults.fallback = null;
    } else if (typeof value !== 'string' || !(BACKEND_NAMES as readonly string[]).includes(value)) {
      issues.push(`defaults.fallback: must be one of ${BACKEND_NAMES.join(', ')}, or "none"`);
    } else {
      defaults.fallback = value as BackendName;
    }
  }

  if (table.transport_types !== undefined) {
    const list = stringList(table.transport_types);
    if (list === null || list.length === 0 || !list.every(isMode)) {
      issues.push(`defaults.transport_types: must be a non-empty list drawn from ${ALL_MODES.join(', ')}`);
    } else {
      defaults.transportTypes = list as Mode[];
    }
  }

  if (table.plan_modes !== undefined) {
    // Not checked against a list of names: the vocabulary is the journey
    // planner's, it grows there without asking, and a validator here would
    // reject a mode that works. A name the planner does not know is ignored
    // upstream, which is the failure this would be protecting against anyway.
    const list = stringList(table.plan_modes);
    if (list === null || list.length === 0) {
      issues.push('defaults.plan_modes: must be a non-empty list of journey-planner transit mode names');
    } else {
      defaults.planModes = list.map((mode) => mode.trim().toUpperCase());
    }
  }

  if (table.walk_weight !== undefined) {
    const value = table.walk_weight;
    if (typeof value !== 'number' || !Number.isFinite(value) || value < 1) {
      // Below one would mean a rider prefers walking to riding, which is a
      // coherent preference and not one this ranking can express: the score
      // would then reward journeys that arrive later for no reason it can name.
      issues.push('defaults.walk_weight: must be a number of at least 1');
    } else {
      defaults.walkWeight = value;
    }
  }

  if (table.timezone !== undefined) {
    const value = table.timezone;
    if (typeof value !== 'string' || value.trim().length === 0) {
      issues.push('defaults.timezone: must be an IANA time zone name');
    } else {
      defaults.timezone = value.trim();
    }
  }

  if (table.home !== undefined) {
    const value = table.home;
    if (typeof value !== 'string' || value.trim().length === 0) {
      issues.push('defaults.home: must be the key of a declared profile');
    } else {
      defaults.home = value.trim();
    }
  }

  return defaults;
}

function parsePlaces(raw: unknown, issues: string[]): Record<string, Place> {
  const places: Record<string, Place> = {};
  if (raw === undefined) return places;
  if (!isTable(raw)) {
    issues.push('places: must be a table of named places');
    return places;
  }
  for (const [name, entry] of Object.entries(raw)) {
    if (!isTable(entry)) {
      issues.push(`places.${name}: must be a table with either lat and lon, or stop`);
      continue;
    }
    const label = typeof entry.label === 'string' && entry.label.trim().length > 0 ? entry.label.trim() : null;
    const stop = entry.stop;
    if (stop !== undefined) {
      if (typeof stop !== 'string' || stop.trim().length === 0) {
        issues.push(`places.${name}.stop: must be a stop id`);
        continue;
      }
      if (entry.lat !== undefined || entry.lon !== undefined) {
        issues.push(`places.${name}: declare either stop, or lat and lon, not both`);
        continue;
      }
      places[name] = { name, label, lat: null, lon: null, stop: stop.trim() };
      continue;
    }
    const lat = entry.lat;
    const lon = entry.lon;
    let ok = true;
    if (typeof lat !== 'number' || !Number.isFinite(lat)) {
      issues.push(`places.${name}.lat: must be a number`);
      ok = false;
    }
    if (typeof lon !== 'number' || !Number.isFinite(lon)) {
      issues.push(`places.${name}.lon: must be a number`);
      ok = false;
    }
    if (ok) places[name] = { name, label, lat: lat as number, lon: lon as number, stop: null };
  }
  return places;
}

function parseBoard(profileKey: string, index: number, raw: unknown, issues: string[]): BoardConfig | null {
  const where = `profiles.${profileKey}.boards[${index}]`;
  if (!isTable(raw)) {
    issues.push(`${where}: must be a table`);
    return null;
  }

  let failed = false;

  const title = raw.title;
  if (typeof title !== 'string' || title.trim().length === 0) {
    issues.push(`${where}.title: must be a non-empty string`);
    failed = true;
  }

  const parsedStops = parseStops(raw.stops);
  if (parsedStops === null) {
    issues.push(`${where}.stops: must be a list of one or more stop ids, each a string or a table with an id and an optional label`);
    failed = true;
  }
  const stops = parsedStops?.stops ?? null;

  const board: BoardConfig = {
    title: typeof title === 'string' ? title.trim() : '',
    stops: stops ?? [],
    walkMinutes: DEFAULT_WALK_MINUTES,
  };
  if (parsedStops !== null && Object.keys(parsedStops.labels).length > 0) {
    board.stopLabels = parsedStops.labels;
  }

  if (raw.modes !== undefined) {
    const list = stringList(raw.modes);
    if (list === null || list.length === 0 || !list.every(isMode)) {
      issues.push(`${where}.modes: must be a non-empty list drawn from ${ALL_MODES.join(', ')}`);
      failed = true;
    } else {
      board.modes = list as Mode[];
    }
  }

  if (raw.lines !== undefined) {
    const list = stringList(raw.lines);
    if (list === null || list.length === 0) {
      issues.push(`${where}.lines: must be a non-empty list of line labels`);
      failed = true;
    } else {
      board.lines = list;
    }
  }

  if (raw.direction !== undefined) {
    const value = raw.direction;
    if (value !== 'H' && value !== 'R') {
      issues.push(`${where}.direction: must be "H" or "R"`);
      failed = true;
    } else {
      board.direction = value;
    }
  }

  if (raw.destinations !== undefined) {
    const list = stringList(raw.destinations);
    if (list === null || list.length === 0) {
      issues.push(`${where}.destinations: must be a non-empty list of regular expressions`);
      failed = true;
    } else {
      const good: string[] = [];
      for (const source of list) {
        try {
          new RegExp(source, 'i');
          good.push(source);
        } catch (error) {
          issues.push(`${where}.destinations: ${source} is not a valid regular expression (${String(error)})`);
          failed = true;
        }
      }
      board.destinations = good;
    }
  }

  if (raw.walk_minutes !== undefined) {
    const value = raw.walk_minutes;
    if (typeof value !== 'number' || !Number.isFinite(value) || value < 0) {
      issues.push(`${where}.walk_minutes: must be a non-negative number of minutes`);
      failed = true;
    } else {
      board.walkMinutes = value;
    }
  }

  if (raw.walk_minutes_by_stop !== undefined) {
    const table = raw.walk_minutes_by_stop;
    if (!isTable(table)) {
      issues.push(`${where}.walk_minutes_by_stop: must be a table mapping stop ids to minutes`);
      failed = true;
    } else {
      const known = new Set(stops ?? []);
      const overrides: Record<string, number> = {};
      for (const [stop, value] of Object.entries(table)) {
        if (!known.has(stop)) {
          issues.push(`${where}.walk_minutes_by_stop: ${stop} is not one of this board's stops`);
          failed = true;
          continue;
        }
        if (typeof value !== 'number' || !Number.isInteger(value) || value < 0) {
          issues.push(`${where}.walk_minutes_by_stop: ${stop} must be a non-negative whole number of minutes`);
          failed = true;
          continue;
        }
        overrides[stop] = value;
      }
      board.walkMinutesByStop = overrides;
    }
  }

  if (raw.commute !== undefined) {
    if (typeof raw.commute !== 'boolean') {
      issues.push(`${where}.commute: must be true or false`);
      failed = true;
    } else if (raw.commute) {
      board.commute = true;
    }
  }

  if (raw.connection !== undefined) {
    const connection = parseConnection(where, raw.connection, issues);
    if (connection === null) failed = true;
    else board.connection = connection;
  }

  return failed ? null : board;
}

/**
 * The onward service a board points at. Everything is required except the
 * direction letter, which some interchanges do not need because the line only
 * runs one way from there.
 */
function parseConnection(where: string, raw: unknown, issues: string[]): ConnectionConfig | null {
  const at = `${where}.connection`;
  if (!isTable(raw)) {
    issues.push(`${at}: must be a table with stop, lines, ride_minutes and transfer_minutes`);
    return null;
  }
  let failed = false;

  const stop = typeof raw.stop === 'string' ? raw.stop.trim() : '';
  if (stop.length === 0) {
    issues.push(`${at}.stop: must be a non-empty stop id`);
    failed = true;
  }

  const lines = stringList(raw.lines);
  if (lines === null || lines.length === 0) {
    issues.push(`${at}.lines: must be a non-empty list of line labels`);
    failed = true;
  }

  let direction: Exclude<Direction, null> | undefined;
  if (raw.direction !== undefined) {
    if (raw.direction !== 'H' && raw.direction !== 'R') {
      issues.push(`${at}.direction: must be "H" or "R"`);
      failed = true;
    } else {
      direction = raw.direction;
    }
  }

  const minutes: Record<'ride_minutes' | 'transfer_minutes', number> = { ride_minutes: 0, transfer_minutes: 0 };
  for (const key of ['ride_minutes', 'transfer_minutes'] as const) {
    const value = raw[key];
    if (typeof value !== 'number' || !Number.isInteger(value) || value < 0) {
      issues.push(`${at}.${key}: must be a non-negative whole number of minutes`);
      failed = true;
      continue;
    }
    minutes[key] = value;
  }

  if (failed) return null;
  const parsed: ConnectionConfig = {
    stop,
    lines: lines ?? [],
    rideMinutes: minutes.ride_minutes,
    transferMinutes: minutes.transfer_minutes,
  };
  if (direction !== undefined) parsed.direction = direction;
  return parsed;
}

function parseProfiles(raw: unknown, issues: string[]): Profile[] {
  const profiles: Profile[] = [];
  if (raw === undefined) return profiles;
  if (!isTable(raw)) {
    issues.push('profiles: must be a table of named profiles');
    return profiles;
  }

  for (const [key, entry] of Object.entries(raw)) {
    if (!isTable(entry)) {
      issues.push(`profiles.${key}: must be a table`);
      continue;
    }
    const title = entry.title;
    if (typeof title !== 'string' || title.trim().length === 0) {
      issues.push(`profiles.${key}.title: must be a non-empty string`);
    }
    const boardsRaw = entry.boards;
    if (!Array.isArray(boardsRaw) || boardsRaw.length === 0) {
      issues.push(`profiles.${key}.boards: must declare at least one board`);
      continue;
    }
    const boards: BoardConfig[] = [];
    for (let index = 0; index < boardsRaw.length; index += 1) {
      const board = parseBoard(key, index, boardsRaw[index], issues);
      if (board !== null) boards.push(board);
    }
    const profile: Profile = { key, title: typeof title === 'string' ? title.trim() : key, boards };
    if (entry.destinations !== undefined) {
      const list = stringList(entry.destinations);
      if (list === null) issues.push(`profiles.${key}.destinations: must be a list of place keys`);
      else profile.destinations = list.map((name) => name.trim()).filter((name) => name.length > 0);
    }
    profiles.push(profile);
  }
  return profiles;
}

/** Validate an already-parsed TOML document. Throws `ConfigError` listing every problem. */
export function parseConfig(raw: unknown, path: string): Config {
  const issues: string[] = [];
  if (!isTable(raw)) throw new ConfigError([`${path}: top level must be a TOML table`]);

  const defaults = parseDefaults(raw.defaults, issues);
  const places = parsePlaces(raw.places, issues);
  const profiles = parseProfiles(raw.profiles, issues);

  // Cross-check last, once both halves are known: an alias that points nowhere
  // is a config error rather than a silent miss at lookup time.
  for (const profile of profiles) {
    for (const name of profile.destinations ?? []) {
      if (places[name] === undefined) {
        issues.push(`profiles.${profile.key}.destinations: no place named ${name} is declared`);
      }
    }
  }

  if (defaults.home !== null && !profiles.some((profile) => profile.key === defaults.home)) {
    issues.push(`defaults.home: no profile named ${defaults.home} is declared`);
  }

  if (issues.length > 0) throw new ConfigError(issues);
  return { path, defaults, places, profiles };
}

/**
 * Read and validate the config. The path is only known at runtime, so the file
 * is read and parsed explicitly rather than through an import attribute, which
 * would need a literal path.
 */
export async function loadConfig(explicit?: string): Promise<Config> {
  const path = resolveConfigPath(explicit);
  const asked = explicit !== undefined || process.env[CONFIG_PATH_ENV] !== undefined;
  const file = Bun.file(path);
  if (!(await file.exists())) {
    // A path someone named explicitly and got wrong is an error. The
    // conventional path simply being absent is not: raw stop ids need no
    // config at all, and refusing to look one up would make the tool useless
    // before it is set up.
    if (asked) throw new ConfigError([`${path}: no such file`]);
    process.stderr.write(`transit: no config at ${path}; profiles are unavailable, raw stop ids still work\n`);
    return {
      path,
      defaults: {
        horizonMinutes: DEFAULT_HORIZON_MINUTES,
        backend: DEFAULT_BACKEND,
        fallback: DEFAULT_FALLBACK,
        transportTypes: [...DEFAULT_TRANSPORT_TYPES],
        planModes: [...DEFAULT_PLAN_MODES],
        walkWeight: DEFAULT_WALK_WEIGHT,
        timezone: DEFAULT_TIMEZONE,
        home: null,
      },
      places: {},
      profiles: [],
    };
  }
  let raw: unknown;
  try {
    raw = Bun.TOML.parse(await file.text());
  } catch (error) {
    throw new ConfigError([`${path}: not valid TOML (${error instanceof Error ? error.message : String(error)})`]);
  }
  return parseConfig(raw, path);
}

/** Look a profile up by its literal key. */
export function findProfile(config: Config, key: string): Profile | undefined {
  return config.profiles.find((profile) => profile.key === key);
}

/**
 * Resolve a name the user typed to a profile. An exact key match always wins,
 * so a config that really does have a profile called `home` keeps it. Only when
 * no such key exists, and the name is exactly the alias, and the config sets
 * the alias, does the alias apply.
 */
export function resolveProfile(config: Config, name: string): Profile | undefined {
  const exact = findProfile(config, name);
  if (exact !== undefined) return exact;
  if (name === HOME_ALIAS && config.defaults.home !== null) return findProfile(config, config.defaults.home);
  return undefined;
}

/**
 * Shape test telling a stop id from a profile key. Stop ids are colon-joined
 * national identifiers and profile keys are bare words, so the colon alone
 * separates them. The test lives here, in one place, because getting it wrong
 * in either direction produces a confusing error: a profile key mistaken for a
 * stop id becomes an upstream 404, and a stop id mistaken for a profile key
 * becomes an unknown-profile complaint about something that is not a profile.
 */
export function looksLikeStopId(name: string): boolean {
  return name.includes(':');
}

/** What a `board` invocation's arguments mean. */
export type BoardTarget =
  | { kind: 'profile'; profile: Profile; requested: string }
  | { kind: 'stops'; stops: string[] }
  | { kind: 'error'; message: string };

/**
 * Decide whether `board`'s arguments name a profile or raw stops. An argument
 * that is neither is rejected here rather than forwarded upstream, where the
 * failure would arrive as an opaque not-found for an identifier the user never
 * typed.
 */
export function resolveBoardTarget(config: Config, args: string[]): BoardTarget {
  if (args.length === 0) return { kind: 'error', message: 'board needs a profile key or one or more stop ids' };

  if (args.length === 1) {
    const name = args[0] as string;
    const profile = resolveProfile(config, name);
    if (profile !== undefined) return { kind: 'profile', profile, requested: name };
  }

  const unknown = args.filter((arg) => !looksLikeStopId(arg));
  if (unknown.length > 0) {
    const keys = config.profiles.map((profile) => profile.key);
    const known = keys.length > 0 ? `configured profiles: ${keys.join(', ')}` : 'no profiles are configured';
    return { kind: 'error', message: `unknown profile: ${unknown.join(', ')}\n${known}` };
  }
  return { kind: 'stops', stops: args };
}
