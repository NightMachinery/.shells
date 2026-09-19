#!/usr/bin/env bun
import { chain, type ChainedBackend } from './backends/chain.ts';
import { createMvgBackend, MVG_DEFAULT_BASE_URL } from './backends/mvg.ts';
import { createTransitousBackend, TRANSITOUS_DEFAULT_BASE_URL } from './backends/transitous.ts';
import type { Backend, DepartureOptions, Window } from './backends/types.ts';
import { createCache, lookupCacheAdapter, withCache, type TransitCache } from './cache.ts';
import {
  BACKEND_NAMES,
  BEYOND_HORIZON_EXTENSION_MINUTES,
  findProfile,
  HOME_ALIAS,
  loadConfig,
  resolveBoardTarget,
  resolveProfile,
  type BackendName,
  type Config,
  ConfigError,
  type Place,
} from './config.ts';
import { attachConnections } from './connect.ts';
import { applyFilters, mergeBoards } from './filter.ts';
import { boardsDocument, configExportDocument, stopTag } from './json.ts';
import { envOverride, HttpError } from './http.ts';
import type { Board, BoardConfig, Departure, Direction, Message, Profile } from './model.ts';
import { DEFAULT_EARLY_BUFFER_MINUTES, planBoard, routeDocument, type PlannedRow, type PlanTarget } from './plan.ts';
import { UnresolvableOriginError, type OriginLevel } from './origin.ts';
import { destinationLabel, planTargets } from './targets.ts';

/**
 * How weak a claim each step of the origin chain makes, weakest highest. A
 * board that resolved at several steps reports the weakest, because that is
 * the strongest thing it can honestly say about itself.
 */
const ORIGIN_RANK: Readonly<Record<OriginLevel, number>> = { parent: 0, platform: 1, coordinate: 2 };
import {
  NEAR_WINDOW_MINUTES,
  renderBoards,
  renderDiscovery,
  renderMessages,
  renderPlannedBoards,
  renderStopHits,
  type DiscoveryGroup,
  type PlannedBoardView,
  type TerminalOptions,
} from './format/terminal.ts';

const EXIT_OK = 0;
const EXIT_RUNTIME = 1;
const EXIT_CONFIG = 2;

/** How often `--watch` re-renders. Chosen to sit just under the departure cache lifetime. */
export const WATCH_INTERVAL_MS = 30_000;

/** How many sample destinations `discover` prints per (line, direction) pair. */
export const DISCOVER_SAMPLE_COUNT = 3;

/**
 * The other end of the commute. `route` needs a destination and the config
 * already names both ends of the journey people actually plan, so with no
 * `--to` it pairs the home profile with this key and this key with the home
 * profile. One rule, no further guessing: anything else has to say where it is
 * going.
 */
export const WORK_KEY = 'work';

interface Flags {
  json: boolean;
  cache: boolean;
  horizon: number | null;
  backend: BackendName | null;
  watch: boolean;
  noColor: boolean;
  config: string | null;
  verbose: boolean;
  withPlaces: boolean;
  to: string | null;
  earlyBuffer: number | null;
  help: boolean;
}

interface Invocation {
  command: string;
  args: string[];
  flags: Flags;
}

const HELP = `transit - departure boards from the command line

usage: bun src/cli.ts <command> [arguments] [flags]

commands:
  board <profile|stop...>   Render the boards of a configured profile, or an
                            ad-hoc board for one or more raw stop ids.
  discover <stop>           List each (line, direction letter) pair seen at a
                            stop with a few sample destinations. This is how a
                            board's direction letter is found.
  search <query>            Search stops by name.
  nearby <lat> <lon>        List stops near a coordinate.
  route <profile>           Commute view: for every departure on that profile's
                            commute boards, the journey it starts, where you
                            change and when you arrive.
  messages [profile]        Service messages, narrowed to a profile's lines
                            when a profile is named.
  config-export             Print the loaded config as JSON for the browser
                            page. Deterministic: no timestamp, same bytes for
                            the same input.

flags:
  --json                    Machine-readable output instead of a rendering.
  --cache                   Use the Redis response cache. Off by default.
  --horizon <minutes>       Override how far ahead to look.
  --backend <name>          Force the primary backend (${BACKEND_NAMES.join(', ')}).
  --watch                   Re-render on an interval until interrupted.
  --no-color                Never emit colour.
  --config <path>           Read this config file instead of the default.
  --with-places             config-export only: include place coordinates.
  --to <profile>            route only: where the commute ends. Defaults to the
                            other end of the home-and-work pair.
  --early-buffer <minutes>  route only: how early an onward departure may leave
                            and still be offered, marked tight.
  --verbose                 Log every outbound request to stderr.
  --help                    This text.

exit codes: 0 ok, 1 runtime error, 2 config validation error.
`;

function fail(message: string, code: number): never {
  process.stderr.write(`${message}\n`);
  return process.exit(code);
}

export function parseArgs(argv: string[]): Invocation {
  const flags: Flags = {
    json: false,
    cache: false,
    horizon: null,
    backend: null,
    watch: false,
    noColor: false,
    config: null,
    verbose: false,
    withPlaces: false,
    to: null,
    earlyBuffer: null,
    help: false,
  };
  const positional: string[] = [];

  for (let i = 0; i < argv.length; i += 1) {
    const token = argv[i] as string;
    switch (token) {
      case '--json':
        flags.json = true;
        break;
      case '--cache':
        flags.cache = true;
        break;
      case '--watch':
        flags.watch = true;
        break;
      case '--no-color':
      case '--no-colour':
        flags.noColor = true;
        break;
      case '--verbose':
        flags.verbose = true;
        break;
      case '--with-places':
        flags.withPlaces = true;
        break;
      case '--help':
      case '-h':
        flags.help = true;
        break;
      case '--horizon': {
        const value = Number(argv[++i]);
        if (!Number.isFinite(value) || value <= 0) fail('--horizon needs a positive number of minutes', EXIT_RUNTIME);
        flags.horizon = value;
        break;
      }
      case '--backend': {
        const value = argv[++i] ?? '';
        if (!(BACKEND_NAMES as readonly string[]).includes(value)) {
          fail(`--backend must be one of ${BACKEND_NAMES.join(', ')}`, EXIT_RUNTIME);
        }
        flags.backend = value as BackendName;
        break;
      }
      case '--to':
        flags.to = argv[++i] ?? null;
        break;
      case '--early-buffer': {
        const value = Number(argv[++i]);
        if (!Number.isFinite(value) || value < 0) fail('--early-buffer needs a non-negative number of minutes', EXIT_RUNTIME);
        flags.earlyBuffer = value;
        break;
      }
      case '--config':
        flags.config = argv[++i] ?? null;
        break;
      default:
        if (token.startsWith('--')) fail(`unknown flag ${token}`, EXIT_RUNTIME);
        positional.push(token);
    }
  }

  return { command: positional[0] ?? '', args: positional.slice(1), flags };
}

function colorEnabled(flags: Flags): boolean {
  if (flags.noColor) return false;
  if (process.env.NO_COLOR !== undefined) return false;
  return process.stdout.isTTY === true;
}

interface Runtime {
  config: Config;
  cache: TransitCache;
  backend: Backend;
  /** Set when a fallback is configured, so per-stop outcomes are observable. */
  chained: ChainedBackend | null;
  primaryName: string;
  /** Set only under `--verbose`; handed on to anything that makes its own requests. */
  debug: ((line: string) => void) | undefined;
  terminal: TerminalOptions;
  window(now: number): Window;
}

function makeBackend(name: BackendName, config: Config, cache: TransitCache, debug: ((line: string) => void) | undefined): Backend {
  const shared = {
    transportTypes: config.defaults.transportTypes,
    ...(debug === undefined ? {} : { onDebug: debug }),
  };
  const raw =
    name === 'mvg'
      ? createMvgBackend(shared)
      : createTransitousBackend({ ...shared, ...(cache.enabled ? { lookupCache: lookupCacheAdapter(cache) } : {}) });
  return withCache(raw, cache, config.defaults.transportTypes);
}

async function makeRuntime(flags: Flags): Promise<Runtime> {
  const config = await loadConfig(flags.config ?? undefined);
  const cache = createCache(flags.cache);
  const debug = flags.verbose ? (line: string) => process.stderr.write(`[transit] ${line}\n`) : undefined;

  const primaryName: BackendName = flags.backend ?? config.defaults.backend;
  const fallbackName: BackendName | null =
    config.defaults.fallback !== null && config.defaults.fallback !== primaryName ? config.defaults.fallback : null;

  const primary = makeBackend(primaryName, config, cache, debug);
  let backend: Backend = primary;
  let chained: ChainedBackend | null = null;
  if (fallbackName !== null) {
    chained = chain(primary, makeBackend(fallbackName, config, cache, debug), (outcome) => {
      if (outcome.error !== undefined) {
        process.stderr.write(`transit: ${outcome.stop}: ${primaryName} failed (${outcome.error}); used ${outcome.backend}\n`);
      }
    });
    backend = chained;
  }

  const horizon = flags.horizon ?? config.defaults.horizonMinutes;
  return {
    config,
    cache,
    backend,
    chained,
    primaryName,
    debug,
    terminal: { now: Date.now(), timezone: config.defaults.timezone, color: colorEnabled(flags) },
    window: (now: number) => ({ fromMs: now, toMs: now + horizon * 60_000 }),
  };
}

/** Which backend answered for a board's stops, or `"mixed"` when they disagree. */
function boardBackend(runtime: Runtime, stops: string[], rows: Departure[]): string {
  const names = new Set<string>();
  for (const stop of stops) {
    const outcome = runtime.chained?.outcomes.get(stop);
    names.add(outcome?.backend ?? runtime.primaryName);
  }
  for (const row of rows) names.add(row.backend);
  if (names.size === 1) {
    const only = [...names][0];
    return only ?? runtime.primaryName;
  }
  return 'mixed';
}

async function buildBoard(runtime: Runtime, boardConfig: BoardConfig, now: number): Promise<Board> {
  const window = runtime.window(now);
  // How far past the end of `window` the interchange has to be looked at; see
  // `BEYOND_HORIZON_EXTENSION_MINUTES`. The board's own stops are not asked
  // that far: what is printed is the window, and rows past it were read by
  // nothing.
  const reachThroughMs = window.toMs + BEYOND_HORIZON_EXTENSION_MINUTES * 60_000;
  const multiStop = boardConfig.stops.length > 1;
  // A board that names its categories asks for those and no others. The page
  // limit is spent on rows the board can use instead of on five categories it
  // is about to delete, which is what decides whether a long horizon at a busy
  // interchange fits in the pages the backend is allowed to walk.
  const options: DepartureOptions | undefined =
    boardConfig.modes !== undefined && boardConfig.modes.length > 0
      ? { transportTypes: boardConfig.modes }
      : undefined;
  const perStop: Departure[][] = [];
  for (const stop of boardConfig.stops) {
    const rows = await runtime.backend.departures(stop, window, options);
    // The mode filter inside `applyFilters` is now redundant for such a board,
    // and stays anyway: the hint is a hint, and a backend that ignores it must
    // not be able to widen a board.
    const filtered = applyFilters(rows, boardConfig);
    if (multiStop) for (const row of filtered) row.stopTag = stopTag(row.stop, boardConfig.stopLabels);
    perStop.push(filtered);
  }
  const departures = mergeBoards(perStop);

  const connection = boardConfig.connection;
  if (connection !== undefined) {
    // The onward window is this board's window shifted by the whole journey to
    // the interchange: nothing leaving before that is reachable from any row
    // here. Its end reaches past the printed window, because the onward service
    // the last printed row needs leaves after that window has closed.
    const reach = (connection.rideMinutes + connection.transferMinutes) * 60_000;
    try {
      const onward = await runtime.backend.departures(connection.stop, {
        fromMs: window.fromMs + reach,
        toMs: reachThroughMs + reach,
      });
      attachConnections(departures, onward, connection);
    } catch {
      // An interchange that cannot be reached leaves the slot empty rather than
      // failing the board; the departures are what the caller asked for.
      for (const row of departures) row.connection = null;
    }
  }

  const board: Board = {
    title: boardConfig.title,
    stops: boardConfig.stops,
    backend: boardBackend(runtime, boardConfig.stops, departures),
    departures,
    walkMinutes: boardConfig.walkMinutes,
  };
  board.planThroughMs = reachThroughMs;
  if (boardConfig.walkMinutesByStop !== undefined) board.walkMinutesByStop = boardConfig.walkMinutesByStop;
  if (boardConfig.stopLabels !== undefined) board.stopLabels = boardConfig.stopLabels;
  if (connection !== undefined) board.connection = connection;
  return board;
}

function adHocBoard(stops: string[]): BoardConfig {
  return { title: stops.length === 1 ? 'departures' : `departures (${stops.length} stops)`, stops, walkMinutes: 0 };
}

async function commandBoard(runtime: Runtime, args: string[], flags: Flags): Promise<number> {
  const target = resolveBoardTarget(runtime.config, args);
  if (target.kind === 'error') {
    process.stderr.write(`${target.message}\n`);
    return EXIT_CONFIG;
  }

  const boardConfigs: BoardConfig[] = target.kind === 'profile' ? target.profile.boards : [adHocBoard(target.stops)];
  const resolvedKey: string | null = target.kind === 'profile' ? target.profile.key : null;
  const requestedKey: string | null = target.kind === 'profile' ? target.requested : null;

  const render = async (): Promise<void> => {
    const now = Date.now();
    const boards: Board[] = [];
    for (const boardConfig of boardConfigs) boards.push(await buildBoard(runtime, boardConfig, now));
    if (flags.json) {
      process.stdout.write(`${JSON.stringify(boardsDocument(resolvedKey, requestedKey, boards, now), null, 2)}\n`);
      return;
    }
    const options: TerminalOptions = { ...runtime.terminal, now };
    process.stdout.write(`${renderBoards(boards, options)}\n`);
  };

  if (!flags.watch) {
    await render();
    return EXIT_OK;
  }

  // Watch mode: clear, redraw, repeat. Ctrl-C leaves the terminal in a sane
  // state rather than mid-frame.
  let stop = false;
  process.on('SIGINT', () => {
    stop = true;
    process.stdout.write('\x1b[?25h\n');
    process.exit(EXIT_OK);
  });
  process.stdout.write('\x1b[?25l');
  while (!stop) {
    process.stdout.write('\x1b[2J\x1b[H');
    await render();
    await new Promise((resolve) => setTimeout(resolve, WATCH_INTERVAL_MS));
  }
  process.stdout.write('\x1b[?25h');
  return EXIT_OK;
}

/**
 * Where a commute ends, resolved from a name the caller typed or from the
 * home-and-work pair.
 *
 * The name is looked up as a profile first and as a place key second, because a
 * profile is a set of boards near a location and the location itself is the
 * place of the same name. Both steps are needed: the destination of a commute
 * often has no boards of its own and therefore no profile, and a profile's key
 * is what names its place.
 */
function resolveDestination(config: Config, name: string): Place | undefined {
  const profile = resolveProfile(config, name);
  return config.places[profile?.key ?? name];
}

/** The key `--to` defaults to, or null when the pair cannot be completed. */
function defaultDestinationKey(config: Config, origin: Profile, typed: string): string | null {
  if (origin.key === WORK_KEY) return config.defaults.home;
  if (typed === HOME_ALIAS || origin.key === config.defaults.home) return WORK_KEY;
  return null;
}

async function commandRoute(runtime: Runtime, args: string[], flags: Flags): Promise<number> {
  const target = resolveBoardTarget(runtime.config, args.slice(0, 1));
  if (target.kind === 'error') {
    process.stderr.write(`${target.message}\n`);
    return EXIT_CONFIG;
  }
  if (target.kind !== 'profile') {
    // Raw stop ids carry no `commute` flag and no place to travel to, so there
    // is nothing for this command to work from.
    process.stderr.write('route needs a configured profile, not raw stop ids\n');
    return EXIT_CONFIG;
  }
  const profile = target.profile;

  const commuting = profile.boards.filter((board) => board.commute === true);
  // A profile may answer the question itself: when every board that plans
  // anything names the place it plans towards, there is no destination left to
  // ask for and `--to` would have nothing to override.
  const selfDirected = commuting.length > 0 && commuting.every((board) => board.destinationPlace !== undefined);

  const wanted = flags.to ?? defaultDestinationKey(runtime.config, profile, target.requested);
  if (wanted === null && !selfDirected) {
    process.stderr.write(
      `route: no destination. Pass --to, or name the ${HOME_ALIAS} profile to go to ${WORK_KEY}, ` +
        `or ${WORK_KEY} to come back\n`,
    );
    return EXIT_CONFIG;
  }
  const place = wanted === null ? undefined : resolveDestination(runtime.config, wanted);
  if (wanted !== null && place === undefined) {
    process.stderr.write(`route: no place named ${wanted} in ${runtime.config.path}\n`);
    return EXIT_CONFIG;
  }

  const boardConfigs = commuting;
  if (boardConfigs.length === 0) {
    // Not an error: most boards are "is there a bus soon" and were never meant
    // to be planned. Saying so and stopping is the honest answer.
    process.stdout.write(`no board in ${profile.key} takes part in the commute view; set commute = true on one\n`);
    return EXIT_OK;
  }

  // Where the plan actually goes: the place itself, and, when it is a profile's
  // doorstep, every stop that profile is built from. See `planTargets`.
  const targetCache = new Map<string, readonly PlanTarget[]>();
  const targetsFor = (destination: Place): readonly PlanTarget[] => {
    const known = targetCache.get(destination.name);
    if (known !== undefined) return known;
    const targets = planTargets(destination, findProfile(runtime.config, destination.name)?.boards ?? []);
    targetCache.set(destination.name, targets);
    return targets;
  };
  if (place !== undefined && targetsFor(place).length === 0) {
    process.stderr.write(`route: ${place.name} declares neither coordinates nor a stop\n`);
    return EXIT_CONFIG;
  }

  const earlyBufferMinutes = flags.earlyBuffer ?? DEFAULT_EARLY_BUFFER_MINUTES;
  const now = Date.now();
  // The commute view plans what it prints, and what it prints is the near
  // window: past that the board itself stops showing individual rows.
  const boundary = now + NEAR_WINDOW_MINUTES * 60_000;
  // A row right at that boundary needs an onward change to search for same as
  // any other row does; without this the search window handed to the planner
  // would stop exactly where the last row leaves, which is the one place it
  // must not stop.
  const coverThroughMs = boundary + BEYOND_HORIZON_EXTENSION_MINUTES * 60_000;

  const views: PlannedBoardView[] = [];
  for (const boardConfig of boardConfigs) {
    // A board that names its own destination is planned towards that one, and
    // `--to` does not reach it: it is a fact about where those platforms go.
    const fixed = boardConfig.destinationPlace === undefined ? undefined : runtime.config.places[boardConfig.destinationPlace];
    const destination = fixed ?? place;
    if (destination === undefined) continue;
    const targets = targetsFor(destination);
    if (targets.length === 0) {
      process.stderr.write(`route: ${destination.name} declares neither coordinates nor a stop\n`);
      continue;
    }
    const board = await buildBoard(runtime, boardConfig, now);
    const byStop = new Map<string, Departure[]>();
    for (const row of board.departures) {
      if (row.realtime > boundary) continue;
      const bucket = byStop.get(row.stop);
      if (bucket === undefined) byStop.set(row.stop, [row]);
      else bucket.push(row);
    }

    const planned: PlannedRow[] = [];
    // Which step of the chain answered, for the board as a whole. A board that
    // merges stops can in principle resolve them at different steps; the
    // weakest one is reported, because that is the claim the board can make.
    let origin: OriginLevel | null = null;
    for (const [stop, rows] of byStop) {
      try {
        planned.push(
          ...(await planBoard({
            stop: boardConfig.planStop ?? stop,
            targets,
            rows,
            startMs: now,
            coverThroughMs,
            earlyBufferMinutes,
            walkWeight: runtime.config.defaults.walkWeight,
            planModes: runtime.config.defaults.planModes,
            onOrigin: (resolved) => {
              if (origin === null || ORIGIN_RANK[resolved.level] > ORIGIN_RANK[origin]) origin = resolved.level;
            },
            ...(runtime.cache.enabled ? { originCache: lookupCacheAdapter(runtime.cache) } : {}),
            ...(runtime.debug === undefined ? {} : { onDebug: runtime.debug }),
          })),
        );
      } catch (error) {
        // One stop that cannot be planned leaves its rows unplanned rather than
        // taking the whole view down: the departures are still worth showing.
        //
        // A 404 is the interesting failure and deserves its own wording. The
        // planner is the aggregator, and a board is free to be configured with a
        // stop identifier only the primary backend carries; that board's
        // departures are fine and its journeys are unanswerable, which is worth
        // saying plainly rather than as an HTTP status.
        const message =
          error instanceof UnresolvableOriginError || (error instanceof HttpError && error.status === 404)
            ? 'the journey planner does not know this stop, under any identifier it was offered'
            : error instanceof Error
              ? error.message
              : String(error);
        process.stderr.write(`transit: ${stop}: ${message}\n`);
        for (const row of rows) planned.push({ departure: row, best: null, options: [] });
      }
    }
    planned.sort((a, b) => a.departure.realtime - b.departure.realtime);
    views.push({ board, rows: planned, origin, destination: destinationLabel(destination) });
  }

  // What the run as a whole was towards. With no picker at all it is the set of
  // destinations the boards named, which is the honest answer and reads as one
  // when they agree.
  const headline =
    place !== undefined
      ? destinationLabel(place)
      : [...new Set(views.map((view) => view.destination ?? ''))].filter((name) => name !== '').join(' and ');

  if (flags.json) {
    const document = routeDocument({
      profile: profile.key,
      requestedProfile: target.requested,
      destinationKey: wanted ?? '',
      destinationName: headline,
      earlyBufferMinutes,
      walkWeight: runtime.config.defaults.walkWeight,
      boards: views,
      now,
    });
    process.stdout.write(`${JSON.stringify(document, null, 2)}\n`);
    return EXIT_OK;
  }

  process.stdout.write(`${renderPlannedBoards(views, headline, { ...runtime.terminal, now })}\n`);
  return EXIT_OK;
}

async function commandDiscover(runtime: Runtime, args: string[], flags: Flags): Promise<number> {
  const stop = args[0];
  if (stop === undefined) {
    process.stderr.write('discover needs a stop id\n');
    return EXIT_RUNTIME;
  }
  const now = Date.now();
  const rows = await runtime.backend.departures(stop, runtime.window(now));
  const groups = new Map<string, DiscoveryGroup>();
  for (const row of rows) {
    const key = JSON.stringify([row.line, row.direction]);
    let group = groups.get(key);
    if (group === undefined) {
      group = { line: row.line, direction: row.direction as Direction, count: 0, destinations: [], mode: row.mode };
      groups.set(key, group);
    }
    group.count += 1;
    if (group.destinations.length < DISCOVER_SAMPLE_COUNT && !group.destinations.includes(row.destination)) {
      group.destinations.push(row.destination);
    }
  }
  const list = [...groups.values()].sort((a, b) => a.line.localeCompare(b.line) || String(a.direction).localeCompare(String(b.direction)));
  if (flags.json) {
    process.stdout.write(
      `${JSON.stringify(
        {
          stop,
          directions: list.map((group) => ({
            line: group.line,
            mode: group.mode,
            direction: group.direction,
            count: group.count,
            sample_destinations: group.destinations,
          })),
        },
        null,
        2,
      )}\n`,
    );
    return EXIT_OK;
  }
  process.stdout.write(`${renderDiscovery(stop, list, { ...runtime.terminal, now })}\n`);
  return EXIT_OK;
}

async function commandSearch(runtime: Runtime, args: string[], flags: Flags): Promise<number> {
  const query = args.join(' ').trim();
  if (query.length === 0) {
    process.stderr.write('search needs a query\n');
    return EXIT_RUNTIME;
  }
  const hits = await runtime.backend.search(query);
  if (flags.json) {
    process.stdout.write(`${JSON.stringify(hits, null, 2)}\n`);
    return EXIT_OK;
  }
  process.stdout.write(`${renderStopHits(hits, runtime.terminal)}\n`);
  return EXIT_OK;
}

async function commandNearby(runtime: Runtime, args: string[], flags: Flags): Promise<number> {
  const lat = Number(args[0]);
  const lon = Number(args[1]);
  if (!Number.isFinite(lat) || !Number.isFinite(lon)) {
    process.stderr.write('nearby needs a latitude and a longitude\n');
    return EXIT_RUNTIME;
  }
  const hits = await runtime.backend.nearby(lat, lon);
  if (flags.json) {
    process.stdout.write(`${JSON.stringify(hits, null, 2)}\n`);
    return EXIT_OK;
  }
  process.stdout.write(`${renderStopHits(hits, runtime.terminal)}\n`);
  return EXIT_OK;
}

function profileLines(profile: Profile): Set<string> {
  const lines = new Set<string>();
  for (const board of profile.boards) for (const line of board.lines ?? []) lines.add(line.replace(/\s+/g, '').toLowerCase());
  return lines;
}

async function commandMessages(runtime: Runtime, args: string[], flags: Flags): Promise<number> {
  let messages: Message[] = await runtime.backend.messages();
  const name = args[0];
  if (name !== undefined) {
    const profile = resolveProfile(runtime.config, name);
    if (profile === undefined) {
      process.stderr.write(`no profile named ${name}\n`);
      return EXIT_RUNTIME;
    }
    const wanted = profileLines(profile);
    // A board that names no lines narrows nothing; only filter when at least
    // one board in the profile actually pins its lines down.
    if (wanted.size > 0) {
      messages = messages.filter((message) =>
        message.lines.some((line) => wanted.has(line.replace(/\s+/g, '').toLowerCase())),
      );
    }
  }
  if (flags.json) {
    process.stdout.write(`${JSON.stringify(messages, null, 2)}\n`);
    return EXIT_OK;
  }
  process.stdout.write(`${renderMessages(messages, runtime.terminal)}\n`);
  return EXIT_OK;
}

function commandConfigExport(runtime: Runtime, flags: Flags): number {
  const document = configExportDocument(runtime.config, {
    withPlaces: flags.withPlaces,
    mvgBaseUrl: envOverride('MVG_BASE_URL') ?? MVG_DEFAULT_BASE_URL,
    transitousBaseUrl: envOverride('TRANSITOUS_BASE_URL') ?? TRANSITOUS_DEFAULT_BASE_URL,
  });
  process.stdout.write(`${JSON.stringify(document, null, 2)}\n`);
  return EXIT_OK;
}

async function main(): Promise<number> {
  const invocation = parseArgs(process.argv.slice(2));
  if (invocation.flags.help || invocation.command === '' || invocation.command === 'help') {
    process.stdout.write(HELP);
    return EXIT_OK;
  }

  let runtime: Runtime;
  try {
    runtime = await makeRuntime(invocation.flags);
  } catch (error) {
    if (error instanceof ConfigError) {
      process.stderr.write(`config: ${error.issues.length} problem${error.issues.length === 1 ? '' : 's'}\n`);
      for (const issue of error.issues) process.stderr.write(`  ${issue}\n`);
      return EXIT_CONFIG;
    }
    throw error;
  }

  try {
    switch (invocation.command) {
      case 'board':
        return await commandBoard(runtime, invocation.args, invocation.flags);
      case 'discover':
        return await commandDiscover(runtime, invocation.args, invocation.flags);
      case 'search':
        return await commandSearch(runtime, invocation.args, invocation.flags);
      case 'nearby':
        return await commandNearby(runtime, invocation.args, invocation.flags);
      case 'route':
        return await commandRoute(runtime, invocation.args, invocation.flags);
      case 'messages':
        return await commandMessages(runtime, invocation.args, invocation.flags);
      case 'config-export':
        return commandConfigExport(runtime, invocation.flags);
      default:
        process.stderr.write(`unknown command ${invocation.command}\n\n${HELP}`);
        return EXIT_RUNTIME;
    }
  } finally {
    runtime.cache.close();
  }
}

// The exit status is set rather than forced: calling `process.exit` here would
// truncate a large `--json` document still being written to stdout.
main()
  .then((code) => {
    process.exitCode = code;
  })
  .catch((error: unknown) => {
    process.stderr.write(`transit: ${error instanceof Error ? error.message : String(error)}\n`);
    process.exitCode = EXIT_RUNTIME;
  });
