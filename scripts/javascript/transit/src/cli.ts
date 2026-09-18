#!/usr/bin/env bun
import { chain, type ChainedBackend } from './backends/chain.ts';
import { createMvgBackend, MVG_DEFAULT_BASE_URL } from './backends/mvg.ts';
import { createTransitousBackend, TRANSITOUS_DEFAULT_BASE_URL } from './backends/transitous.ts';
import type { Backend, DepartureOptions, Window } from './backends/types.ts';
import { createCache, lookupCacheAdapter, withCache, type TransitCache } from './cache.ts';
import {
  BACKEND_NAMES,
  loadConfig,
  resolveBoardTarget,
  resolveProfile,
  type BackendName,
  type Config,
  ConfigError,
} from './config.ts';
import { applyFilters, mergeBoards } from './filter.ts';
import { boardsDocument, configExportDocument, stopTag } from './json.ts';
import { envOverride } from './http.ts';
import type { Board, BoardConfig, Departure, Direction, Message, Profile } from './model.ts';
import {
  renderBoards,
  renderDiscovery,
  renderMessages,
  renderStopHits,
  type DiscoveryGroup,
  type TerminalOptions,
} from './format/terminal.ts';

const EXIT_OK = 0;
const EXIT_RUNTIME = 1;
const EXIT_CONFIG = 2;
const EXIT_NOT_IMPLEMENTED = 3;

/** How often `--watch` re-renders. Chosen to sit just under the departure cache lifetime. */
export const WATCH_INTERVAL_MS = 30_000;

/** How many sample destinations `discover` prints per (line, direction) pair. */
export const DISCOVER_SAMPLE_COUNT = 3;

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
  route <from> <to>         Journey planning. Not implemented yet.
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
  --verbose                 Log every outbound request to stderr.
  --help                    This text.

exit codes: 0 ok, 1 runtime error, 2 config validation error, 3 not implemented.
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

  const board: Board = {
    title: boardConfig.title,
    stops: boardConfig.stops,
    backend: boardBackend(runtime, boardConfig.stops, departures),
    departures,
    walkMinutes: boardConfig.walkMinutes,
  };
  if (boardConfig.walkMinutesByStop !== undefined) board.walkMinutesByStop = boardConfig.walkMinutesByStop;
  if (boardConfig.stopLabels !== undefined) board.stopLabels = boardConfig.stopLabels;
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

  if (invocation.command === 'route') {
    process.stderr.write('route: journey planning is not implemented yet\n');
    return EXIT_NOT_IMPLEMENTED;
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
