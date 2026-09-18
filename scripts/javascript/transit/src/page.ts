// Browser entry point. Bundled by `bun build --target browser` into the file
// the page shell loads.
//
// Nothing here may reach the bun runtime: no `Bun.*`, no `node:*`, and in
// particular never `src/cache.ts`. The page's only state is in memory and in
// `localStorage`.

import { chain } from './backends/chain.ts';
import { createMvgBackend, MVG_DEFAULT_BASE_URL } from './backends/mvg.ts';
import { createTransitousBackend, TRANSITOUS_DEFAULT_BASE_URL } from './backends/transitous.ts';
import type { Backend } from './backends/types.ts';
import { contrastText, cssCustomProperties, resolveColor } from './colors.ts';
import { applyFilters, catchableOnBoard, describeWalk, mergeBoards } from './filter.ts';
import type { Board, BoardConfig, Departure, Message, Mode } from './model.ts';

/** How often the page re-fetches, while the tab is actually being looked at. */
const REFRESH_MS = 30_000;
/** How often the relative minute counts are recomputed, with no network traffic. */
const TICK_MS = 1_000;
/** Minutes of horizon each far-window toggle shows; `null` means everything fetched. */
const FAR_RANGES: Array<{ label: string; minutes: number | null }> = [
  { label: '60 min', minutes: 60 },
  { label: '3 h', minutes: 180 },
  { label: 'all', minutes: null },
];
/** Where the row list stops and the strip view starts. */
const NEAR_WINDOW_MINUTES = 60;
const STORAGE_KEY = 'transit.profile';

interface ExportedBoard {
  title: string;
  stops: string[];
  modes: Mode[] | null;
  lines: string[] | null;
  direction: 'H' | 'R' | null;
  destinations: string[] | null;
  walk_minutes: number;
  walk_minutes_by_stop: Record<string, number> | null;
  stop_labels: Record<string, string> | null;
}

interface ExportedProfile {
  key: string;
  title: string;
  boards: ExportedBoard[];
}

interface ExportedConfig {
  schema_version: number;
  defaults: {
    horizon_minutes: number;
    backend: string;
    fallback: string | null;
    transport_types: Mode[];
    timezone: string;
    home: string | null;
  };
  backends: { mvg_base_url: string; transitous_base_url: string };
  profiles: ExportedProfile[];
}

interface PageState {
  config: ExportedConfig | null;
  profileKey: string | null;
  boards: Board[] | null;
  messages: Message[];
  lastUpdatedMs: number | null;
  stale: boolean;
  farMinutes: number | null;
  loading: boolean;
}

const state: PageState = {
  config: null,
  profileKey: null,
  boards: null,
  messages: [],
  lastUpdatedMs: null,
  stale: false,
  farMinutes: FAR_RANGES[0]?.minutes ?? 60,
  loading: false,
};

function readStoredProfile(): string | null {
  try {
    return localStorage.getItem(STORAGE_KEY);
  } catch {
    // Private browsing and blocked site data both throw here. The page works
    // fine without a remembered tab.
    return null;
  }
}

function storeProfile(key: string): void {
  try {
    localStorage.setItem(STORAGE_KEY, key);
  } catch {
    /* see readStoredProfile */
  }
}

function toBoardConfig(board: ExportedBoard): BoardConfig {
  const config: BoardConfig = { title: board.title, stops: board.stops, walkMinutes: board.walk_minutes };
  if (board.modes !== null) config.modes = board.modes;
  if (board.lines !== null) config.lines = board.lines;
  if (board.direction !== null) config.direction = board.direction;
  if (board.destinations !== null) config.destinations = board.destinations;
  if (board.walk_minutes_by_stop !== null) config.walkMinutesByStop = board.walk_minutes_by_stop;
  if (board.stop_labels !== null) config.stopLabels = board.stop_labels;
  return config;
}

function makeBackend(config: ExportedConfig): { backend: Backend; outcomes: ReadonlyMap<string, { backend: string }> } {
  const shared = { transportTypes: config.defaults.transport_types };
  const mvg = createMvgBackend({ ...shared, baseUrl: config.backends.mvg_base_url || MVG_DEFAULT_BASE_URL });
  const transitous = createTransitousBackend({
    ...shared,
    baseUrl: config.backends.transitous_base_url || TRANSITOUS_DEFAULT_BASE_URL,
  });
  const chained = chain(mvg, transitous);
  return { backend: chained, outcomes: chained.outcomes };
}

/** The same rule as the CLI's: the configured label, else the id's last field. */
function stopTagOf(stop: string, labels?: Record<string, string>): string {
  const label = labels?.[stop];
  if (label !== undefined && label.length > 0) return label;
  const fields = stop.split(':');
  const last = fields[fields.length - 1];
  return last !== undefined && last.length > 0 ? last : stop;
}

async function loadBoards(): Promise<void> {
  const config = state.config;
  const profileKey = state.profileKey;
  if (config === null || profileKey === null || state.loading) return;
  const profile = config.profiles.find((entry) => entry.key === profileKey);
  if (profile === undefined) return;

  state.loading = true;
  const { backend, outcomes } = makeBackend(config);
  const now = Date.now();
  const window = { fromMs: now, toMs: now + config.defaults.horizon_minutes * 60_000 };

  try {
    const built: Board[] = [];
    for (const exported of profile.boards) {
      const boardConfig = toBoardConfig(exported);
      const multiStop = boardConfig.stops.length > 1;
      const perStop: Departure[][] = [];
      for (const stop of boardConfig.stops) {
        const rows = applyFilters(await backend.departures(stop, window), boardConfig);
        if (multiStop) for (const row of rows) row.stopTag = stopTagOf(row.stop, boardConfig.stopLabels);
        perStop.push(rows);
      }
      const departures = mergeBoards(perStop);
      const names = new Set<string>();
      for (const stop of boardConfig.stops) names.add(outcomes.get(stop)?.backend ?? config.defaults.backend);
      const board: Board = {
        title: boardConfig.title,
        stops: boardConfig.stops,
        backend: names.size === 1 ? ([...names][0] ?? config.defaults.backend) : 'mixed',
        departures,
        walkMinutes: boardConfig.walkMinutes,
      };
      if (boardConfig.walkMinutesByStop !== undefined) board.walkMinutesByStop = boardConfig.walkMinutesByStop;
      if (boardConfig.stopLabels !== undefined) board.stopLabels = boardConfig.stopLabels;
      built.push(board);
    }
    state.boards = built;
    state.lastUpdatedMs = Date.now();
    state.stale = false;
  } catch {
    // Keep the last good board on screen rather than blanking it; the banner
    // says how old it is.
    state.stale = true;
  } finally {
    state.loading = false;
  }

  try {
    state.messages = await backend.messages();
  } catch {
    /* a missing disruption list is not worth failing the render over */
  }
  render();
}

function el(tag: string, className?: string, text?: string): HTMLElement {
  const node = document.createElement(tag);
  if (className !== undefined) node.className = className;
  if (text !== undefined) node.textContent = text;
  return node;
}

function clockTime(epochMs: number, timezone: string): string {
  return new Intl.DateTimeFormat('en-GB', {
    hour: '2-digit',
    minute: '2-digit',
    hour12: false,
    timeZone: timezone,
  }).format(new Date(epochMs));
}

function badge(dep: Pick<Departure, 'line' | 'mode' | 'color'>): HTMLElement {
  const colour = resolveColor(dep);
  const node = el('span', 'badge', dep.line);
  node.style.backgroundColor = colour;
  node.style.color = contrastText(colour);
  return node;
}

function renderNearRow(dep: Departure, board: Board, now: number, timezone: string, highlight: boolean): HTMLElement {
  const reachable = catchableOnBoard(dep, board, now);
  const row = el('li', `row${reachable ? '' : ' unreachable'}${highlight ? ' first-catchable' : ''}`);

  const minutes = Math.max(0, Math.floor((dep.realtime - now) / 60_000));
  row.append(el('span', 'minutes', String(minutes)));
  row.append(badge(dep));

  const middle = el('span', 'middle');
  middle.append(el('span', `destination${dep.cancelled ? ' cancelled' : ''}`, dep.destination));
  const meta = el('span', 'meta');
  meta.append(el('span', 'clock', clockTime(dep.realtime, timezone)));
  if (dep.delayMin !== 0) meta.append(el('span', dep.delayMin > 0 ? 'delay late' : 'delay early', `${dep.delayMin > 0 ? '+' : ''}${dep.delayMin}`));
  if (dep.platform !== null) meta.append(el('span', 'platform', `Pl ${dep.platform}`));
  if (dep.sev) meta.append(el('span', 'flag sev', 'SEV'));
  if (dep.cancelled) meta.append(el('span', 'flag cancelled-flag', 'cancelled'));
  if (dep.stopTag !== undefined) meta.append(el('span', 'stop-tag', `@${dep.stopTag}`));
  middle.append(meta);
  row.append(middle);

  row.append(el('span', `dot${dep.realtimeKnown ? ' live' : ' scheduled'}`, dep.realtimeKnown ? '●' : '○'));
  return row;
}

function renderStrip(rows: Departure[], timezone: string): HTMLElement {
  const list = el('ul', 'strips');
  const groups = new Map<string, { head: Departure; entries: Departure[] }>();
  for (const row of rows) {
    // Per stop as well as per line: see the terminal renderer's note.
    const key = JSON.stringify([row.line, row.direction, row.stopTag ?? null]);
    const group = groups.get(key);
    if (group === undefined) groups.set(key, { head: row, entries: [row] });
    else group.entries.push(row);
  }
  const ordered = [...groups.values()].sort((a, b) => (a.entries[0]?.realtime ?? 0) - (b.entries[0]?.realtime ?? 0));
  for (const group of ordered) {
    const item = el('li', 'strip');
    item.append(badge(group.head));
    item.append(el('span', 'direction', group.head.direction ?? '-'));
    if (group.head.stopTag !== undefined) item.append(el('span', 'stop-tag', `@${group.head.stopTag}`));
    const times = el('span', 'times');
    for (const dep of group.entries) {
      const cell = el('span', `time${dep.cancelled ? ' cancelled' : ''}`, clockTime(dep.realtime, timezone));
      if (dep.delayMin !== 0) cell.append(el('span', dep.delayMin > 0 ? 'delay late' : 'delay early', `${dep.delayMin > 0 ? '+' : ''}${dep.delayMin}`));
      times.append(cell);
    }
    item.append(times);
    list.append(item);
  }
  return list;
}

function renderBoard(board: Board, now: number, timezone: string): HTMLElement {
  const section = el('section', 'board');
  const header = el('header', 'board-header');
  header.append(el('h2', undefined, board.title));
  header.append(el('span', 'backend', board.backend));
  section.append(header);
  section.append(el('p', 'board-walk', describeWalk(board.stops, board, (stop) => stopTagOf(stop, board.stopLabels))));

  if (board.departures.length === 0) {
    section.append(el('p', 'empty', 'nothing in the window'));
    return section;
  }

  const boundary = now + NEAR_WINDOW_MINUTES * 60_000;
  const near = board.departures.filter((dep) => dep.realtime <= boundary);
  const farLimit = state.farMinutes === null ? Number.POSITIVE_INFINITY : now + state.farMinutes * 60_000;
  const far = board.departures.filter((dep) => dep.realtime > boundary && dep.realtime <= farLimit);

  const list = el('ul', 'rows');
  let highlighted = false;
  for (const dep of near) {
    const reachable = catchableOnBoard(dep, board, now);
    const highlight = reachable && !highlighted;
    if (highlight) highlighted = true;
    list.append(renderNearRow(dep, board, now, timezone, highlight));
  }
  section.append(list);

  if (far.length > 0) {
    section.append(el('h3', 'later', 'later'));
    section.append(renderStrip(far, timezone));
  }
  return section;
}

function renderTabs(config: ExportedConfig): HTMLElement {
  const nav = el('nav', 'tabs');
  for (const profile of config.profiles) {
    const button = document.createElement('button');
    button.type = 'button';
    button.className = `tab${profile.key === state.profileKey ? ' active' : ''}`;
    button.textContent = profile.title;
    button.addEventListener('click', () => {
      if (state.profileKey === profile.key) return;
      state.profileKey = profile.key;
      state.boards = null;
      storeProfile(profile.key);
      render();
      void loadBoards();
    });
    nav.append(button);
  }
  return nav;
}

function renderRangeToggle(): HTMLElement {
  const wrap = el('div', 'ranges');
  for (const range of FAR_RANGES) {
    const button = document.createElement('button');
    button.type = 'button';
    button.className = `range${range.minutes === state.farMinutes ? ' active' : ''}`;
    button.textContent = range.label;
    button.addEventListener('click', () => {
      state.farMinutes = range.minutes;
      render();
    });
    wrap.append(button);
  }
  return wrap;
}

function visibleLines(config: ExportedConfig): Set<string> {
  const profile = config.profiles.find((entry) => entry.key === state.profileKey);
  const lines = new Set<string>();
  for (const board of profile?.boards ?? []) for (const line of board.lines ?? []) lines.add(line.replace(/\s+/g, '').toLowerCase());
  return lines;
}

/**
 * Whether the disruption list is expanded. Module state rather than DOM state,
 * because the whole page is re-rendered every refresh and a `<details>` that
 * snapped shut every thirty seconds while you were reading it would be worse
 * than not having the feature.
 */
let disruptionsOpen = false;

function renderMessages(config: ExportedConfig): HTMLElement | null {
  if (state.messages.length === 0) return null;
  const wanted = visibleLines(config);
  const relevant =
    wanted.size === 0
      ? state.messages
      : state.messages.filter((message) => message.lines.some((line) => wanted.has(line.replace(/\s+/g, '').toLowerCase())));
  if (relevant.length === 0) return null;

  /* Collapsed by default. These notices run to several paragraphs each and the
     operator posts one per affected section, so rendering them open pushes
     every board below the fold on a phone, which is the opposite of what the
     page is for. The summary says how many and which lines, which is enough to
     decide whether to open it. */
  const affected = new Set<string>();
  for (const message of relevant) for (const line of message.lines) affected.add(line);
  const lineList = [...affected].sort();

  const box = document.createElement('details');
  box.className = 'disruptions';
  box.open = disruptionsOpen;
  box.addEventListener('toggle', () => {
    disruptionsOpen = box.open;
  });

  const summary = document.createElement('summary');
  const count = relevant.length === 1 ? '1 service message' : `${relevant.length} service messages`;
  summary.append(el('strong', undefined, count));
  if (lineList.length > 0) summary.append(el('span', 'disruption-lines', lineList.join(' ')));
  box.append(summary);

  for (const message of relevant) {
    const item = el('div', 'disruption');
    item.append(el('strong', undefined, message.title));
    if (message.lines.length > 0) item.append(el('span', 'disruption-lines', message.lines.join(' ')));
    item.append(el('p', undefined, message.text));
    box.append(item);
  }
  return box;
}

function render(): void {
  const app = document.getElementById('app');
  if (app === null) return;
  app.replaceChildren();

  const config = state.config;
  if (config === null) {
    app.append(el('p', 'empty', 'loading configuration'));
    return;
  }

  app.append(renderTabs(config));

  const status = el('div', `status${state.stale ? ' stale' : ''}`);
  if (state.lastUpdatedMs === null) {
    status.append(el('span', undefined, state.loading ? 'loading' : 'no data yet'));
  } else {
    const age = Math.max(0, Math.round((Date.now() - state.lastUpdatedMs) / 1000));
    status.append(el('span', undefined, `Updated ${age}s ago`));
  }
  if (state.stale) status.append(el('span', 'stale-flag', 'refresh failed, showing last known data'));
  app.append(status);

  const disruptions = renderMessages(config);
  if (disruptions !== null) app.append(disruptions);

  app.append(renderRangeToggle());

  const now = Date.now();
  const timezone = config.defaults.timezone;
  if (state.boards === null) {
    app.append(el('p', 'empty', 'loading departures'));
    return;
  }
  for (const board of state.boards) app.append(renderBoard(board, now, timezone));
}

function injectColors(): void {
  const style = document.createElement('style');
  style.textContent = cssCustomProperties();
  document.head.append(style);
}

async function boot(): Promise<void> {
  injectColors();
  render();
  try {
    const response = await fetch('data/config.json', { headers: { Accept: 'application/json' } });
    if (!response.ok) throw new Error(`config.json: HTTP ${response.status}`);
    const config = (await response.json()) as ExportedConfig;
    state.config = config;
    const stored = readStoredProfile();
    const known = config.profiles.some((profile) => profile.key === stored);
    // Tabs are exactly the configured profiles, in config order. No profile key
    // is special-cased; the first one is simply the first one.
    state.profileKey = known && stored !== null ? stored : (config.profiles[0]?.key ?? null);
  } catch (error) {
    const app = document.getElementById('app');
    if (app !== null) {
      app.replaceChildren(el('p', 'empty', `could not load configuration: ${error instanceof Error ? error.message : String(error)}`));
    }
    return;
  }

  render();
  await loadBoards();

  // Refresh only while the document is visible: a phone left in a pocket should
  // not keep hitting a free API all day.
  setInterval(() => {
    if (document.visibilityState === 'visible') void loadBoards();
  }, REFRESH_MS);
  document.addEventListener('visibilitychange', () => {
    if (document.visibilityState === 'visible') void loadBoards();
  });
  // Countdowns tick locally; this touches no network.
  setInterval(render, TICK_MS);
}

void boot();
