// Browser entry point. Bundled by `bun build --target browser` into the file
// the page shell loads.
//
// Nothing here may reach the bun runtime: no `Bun.*`, no `node:*`, and in
// particular never `src/cache.ts`. The page's state is in memory, in
// `localStorage` for the reader's choices, and in IndexedDB for the last known
// boards.
//
// The render is whole-page and idempotent: `render()` rebuilds everything from
// `state`. That is affordable at this size and it removes a whole class of bug
// where a partial update and the model disagree. The one thing it cannot do is
// run every second, because it would destroy a selection in progress and make
// the page unscrollable, so the second-by-second countdowns are patched through
// registered callbacks instead. See `tick()`.

import { cssCustomProperties } from './colors.ts';
import { normaliseLine } from './filter.ts';
import type { Board, Message } from './model.ts';
import { renderBar, HORIZONS } from './page/bar.ts';
import { closeFilters, renderBoard, viewOf, type BoardContext } from './page/board.ts';
import { planProfile } from './page/commute.ts';
import { fetchMessages, fetchProfile } from './page/data.ts';
import { el, selectionInsideBoards } from './page/dom.ts';
import { idbGet, idbSet, STORE_BOARDS } from './page/idb.ts';
import { primeMessageState, renderMessages, resetMessageFilters } from './page/messages.ts';
import { rearm, setOnAlarmsChanged } from './page/notify.ts';
import {
  boardId,
  readDestination,
  readHorizon,
  readProfile,
  readSortByArrival,
  writeDestination,
  writeHorizon,
  writeProfile,
  writeSortByArrival,
  writeView,
} from './page/store.ts';
import type { BoardStatus, ExportedConfig, ExportedProfile, PageState, ProfileData } from './page/types.ts';
import { DEFAULT_EARLY_BUFFER_MINUTES } from './plan.ts';

/** How often the visible profile is re-fetched, while the tab is looked at. */
const REFRESH_MS = 30_000;
/** How often the relative minute counts are recomputed, with no network traffic. */
const TICK_MS = 1_000;
/** How stale cached data may be before switching to a tab triggers a refresh. */
const STALE_MS = 30_000;

const state: PageState = {
  config: null,
  profileKey: null,
  data: new Map(),
  status: new Map(),
  messages: [],
  inFlight: 0,
  startMode: 'now',
  startMs: Date.now(),
  horizonMinutes: HORIZONS[1] ?? 180,
  lastError: null,
  destinationKey: null,
  sortByArrival: readSortByArrival(),
  earlyBufferMinutes: DEFAULT_EARLY_BUFFER_MINUTES,
  routes: new Map(),
};

/** Callbacks the current render registered for the one-second tick. */
let ticks: Array<() => void> = [];
/** Backends that answered the visible profile's most recent fetch. */
let backendsUsed: string[] = [];

function currentBoards(): Board[] {
  const key = state.profileKey;
  return key === null ? [] : (state.data.get(key)?.boards ?? []);
}

function statusFor(profileKey: string, index: number): BoardStatus {
  return state.status.get(profileKey)?.[index] ?? { kind: 'idle' };
}

function setStatus(profileKey: string, index: number, status: BoardStatus): void {
  const list = state.status.get(profileKey) ?? [];
  list[index] = status;
  state.status.set(profileKey, list);
}

/**
 * The start instant the boards are fetched for.
 *
 * With `now` this moves with the clock, so a refresh five minutes later asks
 * about five minutes later. With a picked moment it is frozen, which is the
 * whole point of picking one.
 */
function startInstant(): number {
  return state.startMode === 'picked' ? state.startMs : Date.now();
}

// ------------------------------------------------------------------ fetching

/** Where a profile's cached boards live between visits. */
function cacheKey(profileKey: string): string {
  return `boards:${profileKey}`;
}

async function loadCached(profileKey: string): Promise<void> {
  const cached = await idbGet<ProfileData>(STORE_BOARDS, cacheKey(profileKey));
  if (cached === null) return;
  // Only useful while nothing fresher has arrived; a fetch that finished first
  // is by definition better than a cache written before it.
  if (state.data.has(profileKey)) return;
  state.data.set(profileKey, cached);
  render();
}

/**
 * Re-plan a profile's opted-in boards from the departures already on screen.
 *
 * Separate from the fetch because the tight-connection window changes the
 * plan without changing the departures, and because the planner's own cache
 * makes an immediate re-plan nearly free: the itineraries are already there
 * and only the filtering of them differs.
 */
async function replanProfile(
  profileKey: string,
  profile: ExportedProfile,
  boards: Board[],
  startMs: number,
): Promise<void> {
  const config = state.config;
  if (config === null) return;
  const routes = await planProfile({
    config,
    profile,
    boards,
    destinationKey: state.destinationKey,
    startMs,
    earlyBufferMinutes: state.earlyBufferMinutes,
  });
  if (routes.size === 0) state.routes.delete(profileKey);
  else state.routes.set(profileKey, routes);
  render();
}

/** Re-plan whatever is on screen now, for a change that does not need a fetch. */
function replanVisible(): void {
  const config = state.config;
  const profileKey = state.profileKey;
  if (config === null || profileKey === null) return;
  const profile = config.profiles.find((entry) => entry.key === profileKey);
  const data = state.data.get(profileKey);
  if (profile === undefined || data === undefined) return;
  void replanProfile(profileKey, profile, data.boards, data.startMs);
}

async function refreshProfile(profileKey: string, force = false): Promise<void> {
  const config = state.config;
  if (config === null) return;
  const profile = config.profiles.find((entry) => entry.key === profileKey);
  if (profile === undefined) return;

  const startMs = startInstant();
  const existing = state.data.get(profileKey);
  if (
    !force &&
    existing !== undefined &&
    Date.now() - existing.fetchedAtMs < STALE_MS &&
    existing.horizonMinutes === state.horizonMinutes &&
    (state.startMode === 'now' || existing.startMs === startMs)
  ) {
    return;
  }

  state.inFlight += 1;
  render();
  try {
    const result = await fetchProfile({
      config,
      profile,
      startMs,
      horizonMinutes: state.horizonMinutes,
      onStatus: (index, status) => {
        setStatus(profileKey, index, status);
        if (profileKey === state.profileKey) render();
      },
    });
    const data: ProfileData = {
      boards: result.boards,
      fetchedAtMs: Date.now(),
      startMs,
      horizonMinutes: state.horizonMinutes,
    };
    state.data.set(profileKey, data);
    if (profileKey === state.profileKey) {
      backendsUsed = result.backends;
      state.lastError = null;
      rearm(result.boards, Date.now());
    }
    void idbSet(STORE_BOARDS, cacheKey(profileKey), data);

    // The plan comes after the departures and never blocks them: a board that
    // cannot be planned is still a board, and the journey planner is a slower
    // and heavier service than the departure feed.
    void replanProfile(profileKey, profile, result.boards, startMs);
  } catch (error) {
    // The last good boards stay on screen; the bar says the refresh failed and
    // the age keeps counting up, which together are more useful than a blank.
    if (profileKey === state.profileKey) state.lastError = error instanceof Error ? error.message : String(error);
  } finally {
    state.inFlight -= 1;
    render();
  }
}

async function refreshMessages(): Promise<void> {
  const config = state.config;
  if (config === null) return;
  try {
    const messages: Message[] = await fetchMessages(config);
    state.messages = messages;
    await primeMessageState(messages);
    render();
  } catch {
    /* a missing disruption list is not worth failing the render over */
  }
}

// ------------------------------------------------------------------- render

/**
 * The lines a disruption notice has to name to be worth showing.
 *
 * Both what the profile pins and what its boards are actually showing right now.
 * The configured labels alone are not enough: a board filtered by category
 * rather than by line pins nothing, so a profile made of rapid-transit boards
 * would have matched no notice at all, which is exactly the profile where a
 * notice matters most. Taking the lines off the fetched rows makes the rule
 * "notices about lines you can see", which is what a reader means.
 */
function relevantLines(config: ExportedConfig): Set<string> {
  const profile = config.profiles.find((entry) => entry.key === state.profileKey);
  const lines = new Set<string>();
  for (const board of profile?.boards ?? []) for (const line of board.lines ?? []) lines.add(normaliseLine(line));
  for (const board of currentBoards()) for (const dep of board.departures) lines.add(normaliseLine(dep.line));
  return lines;
}

/**
 * Where a profile plans towards when the reader has not said.
 *
 * From anywhere that is not work you are going to work, and from work you are
 * going to whichever home the configuration calls the default one. That covers
 * the journey people actually repeat; anything else is a choice they make in the
 * picker. A configuration with no places gets no commute view at all, which is
 * the right answer rather than an error, since the feature needs a coordinate.
 */
function defaultDestination(config: ExportedConfig, profileKey: string): string | null {
  const places = new Set((config.places ?? []).map((place) => place.name));
  if (places.size === 0) return null;
  if (profileKey !== 'work' && places.has('work')) return 'work';
  const home = config.defaults.home;
  if (profileKey === 'work' && home !== null && places.has(home)) return home;
  for (const place of places) if (place !== profileKey) return place;
  return null;
}

function applyDestination(config: ExportedConfig, profileKey: string): void {
  const stored = readDestination(profileKey);
  const places = new Set((config.places ?? []).map((place) => place.name));
  state.destinationKey = stored !== null && places.has(stored) ? stored : defaultDestination(config, profileKey);
}

function selectProfile(key: string): void {
  if (state.profileKey === key) return;
  state.profileKey = key;
  writeProfile(key);
  if (state.config !== null) applyDestination(state.config, key);
  resetMessageFilters();
  closeFilters();
  render();
  // Cached boards first, so the tab switch is instant, then a refresh if what
  // we had is older than a glance.
  void loadCached(key).then(() => refreshProfile(key));
}

function render(): void {
  const app = document.getElementById('app');
  if (app === null) return;
  const config = state.config;
  ticks = [];

  if (config === null) {
    app.replaceChildren(el('p', 'empty', 'loading configuration'));
    return;
  }

  const now = state.startMode === 'picked' ? state.startMs : Date.now();
  const profileKey = state.profileKey;
  const boards = currentBoards();
  const data = profileKey === null ? undefined : state.data.get(profileKey);
  const nodes: Node[] = [];

  nodes.push(
    renderBar({
      state,
      config,
      boards,
      now,
      backends: backendsUsed,
      ageSeconds: data === undefined ? null : Math.max(0, Math.round((Date.now() - data.fetchedAtMs) / 1000)),
      onProfile: selectProfile,
      onHorizon: (minutes) => {
        state.horizonMinutes = minutes;
        writeHorizon(minutes);
        render();
        if (profileKey !== null) void refreshProfile(profileKey, true);
      },
      onStart: (mode, ms) => {
        state.startMode = mode;
        state.startMs = ms;
        render();
        if (profileKey !== null) void refreshProfile(profileKey, true);
      },
      onRefresh: () => {
        if (profileKey !== null) void refreshProfile(profileKey, true);
        void refreshMessages();
      },
      onDestination: (key) => {
        state.destinationKey = key;
        if (profileKey !== null) writeDestination(profileKey, key);
        render();
        if (profileKey !== null) void refreshProfile(profileKey, true);
      },
      onSort: (value) => {
        state.sortByArrival = value;
        writeSortByArrival(value);
        render();
      },
      ticks,
    }),
  );

  const disruptions = renderMessages(state.messages, relevantLines(config), render);
  if (disruptions !== null) nodes.push(disruptions);

  if (profileKey !== null) {
    const barBackend = backendsUsed.length === 1 ? (backendsUsed[0] ?? '') : '';
    const routes = state.routes.get(profileKey);
    boards.forEach((board, index) => {
      const context: BoardContext = {
        profileKey,
        index,
        now,
        timezone: config.defaults.timezone,
        status: statusFor(profileKey, index),
        planned: state.startMode === 'picked',
        barBackend,
        onChange: render,
        onRetry: () => void refreshProfile(profileKey, true),
        sortByArrival: state.sortByArrival,
        earlyBufferMinutes: state.earlyBufferMinutes,
        onEarlyBuffer: (minutes) => {
          if (minutes === state.earlyBufferMinutes) return;
          state.earlyBufferMinutes = minutes;
          render();
          replanVisible();
        },
        ...(routes?.get(index) === undefined ? {} : { routes: routes.get(index) }),
        ticks,
      };
      nodes.push(renderBoard(board, context));
    });
    if (boards.length === 0) nodes.push(el('p', 'empty', state.inFlight > 0 ? 'loading departures' : 'no departures yet'));
  }

  app.replaceChildren(...nodes);
}

/**
 * The one-second update. It patches the text of the minute counts and the
 * freshness label and touches nothing else, because a full render every second
 * would drop a selection in progress and fight the reader for the scroll
 * position. While a selection is live inside a board it does not even do that.
 */
function tick(): void {
  if (selectionInsideBoards()) return;
  for (const patch of ticks) patch();
}

// ------------------------------------------------------------------ keyboard

function isTyping(target: EventTarget | null): boolean {
  const node = target as HTMLElement | null;
  if (node === null) return false;
  const tag = node.tagName;
  return tag === 'INPUT' || tag === 'TEXTAREA' || tag === 'SELECT' || node.isContentEditable;
}

function stepHorizon(direction: -1 | 1): void {
  const index = HORIZONS.indexOf(state.horizonMinutes);
  const next = HORIZONS[Math.min(HORIZONS.length - 1, Math.max(0, (index === -1 ? 1 : index) + direction))];
  if (next === undefined || next === state.horizonMinutes) return;
  state.horizonMinutes = next;
  writeHorizon(next);
  render();
  if (state.profileKey !== null) void refreshProfile(state.profileKey, true);
}

function installKeys(): void {
  document.addEventListener('keydown', (event) => {
    if (isTyping(event.target) || event.metaKey || event.ctrlKey || event.altKey) return;
    const config = state.config;
    if (config === null) return;

    if (/^[1-9]$/.test(event.key)) {
      const profile = config.profiles[Number.parseInt(event.key, 10) - 1];
      if (profile !== undefined) {
        event.preventDefault();
        selectProfile(profile.key);
      }
      return;
    }
    if (event.key === 'r') {
      event.preventDefault();
      if (state.profileKey !== null) void refreshProfile(state.profileKey, true);
      void refreshMessages();
      return;
    }
    if (event.key === '[') {
      event.preventDefault();
      stepHorizon(-1);
      return;
    }
    if (event.key === ']') {
      event.preventDefault();
      stepHorizon(1);
      return;
    }
    if (event.key === 'f') {
      // The board the reader is looking at, which is the one under the top of
      // the viewport, and the first one otherwise.
      const profileKey = state.profileKey;
      const boards = currentBoards();
      if (profileKey === null || boards.length === 0) return;
      event.preventDefault();
      let index = 0;
      for (let candidate = 0; candidate < boards.length; candidate += 1) {
        const node = document.getElementById(`board-${candidate}`);
        if (node !== null && node.getBoundingClientRect().bottom > 120) {
          index = candidate;
          break;
        }
      }
      const board = boards[index];
      if (board === undefined) return;
      const id = boardId(profileKey, board.title);
      writeView(id, viewOf(profileKey, board, state.routes.get(profileKey)?.has(index) === true) === 'full' ? 'integrated' : 'full');
      render();
    }
  });
}

// ---------------------------------------------------------------------- boot

function injectColors(): void {
  const style = document.createElement('style');
  style.textContent = cssCustomProperties();
  document.head.append(style);
}

/**
 * Register the service worker, which is what makes the installed app open to
 * its own shell instead of the browser's offline page. The boards it shows at
 * that moment come from IndexedDB, not from the worker's cache: the shell is
 * static and cacheable, the departures are not.
 */
function installServiceWorker(): void {
  if (!('serviceWorker' in navigator)) return;
  window.addEventListener('load', () => {
    void navigator.serviceWorker.register('sw.js').catch(() => {
      // An unregistrable worker costs the page nothing; it just means no
      // offline shell. Not worth a message to the reader.
    });
  });
}

async function boot(): Promise<void> {
  injectColors();
  installServiceWorker();
  setOnAlarmsChanged(render);
  render();

  try {
    const response = await fetch('data/config.json', { headers: { Accept: 'application/json' } });
    if (!response.ok) throw new Error(`config.json: HTTP ${response.status}`);
    const config = (await response.json()) as ExportedConfig;
    state.config = config;
    state.horizonMinutes = readHorizon() ?? config.defaults.horizon_minutes;
    const stored = readProfile();
    const known = config.profiles.some((profile) => profile.key === stored);
    // Tabs are exactly the configured profiles, in config order. No profile key
    // is special-cased; the first one is simply the first one.
    state.profileKey = known && stored !== null ? stored : (config.profiles[0]?.key ?? null);
    if (state.profileKey !== null) applyDestination(config, state.profileKey);
  } catch (error) {
    const app = document.getElementById('app');
    if (app !== null) {
      app.replaceChildren(el('p', 'empty', `could not load configuration: ${error instanceof Error ? error.message : String(error)}`));
    }
    return;
  }

  installKeys();

  // Which optional slots a row carries depends on how wide the screen is, and
  // a phone changes width when it is turned over. Nothing else here reacts to
  // a resize, because the layout is otherwise CSS's business.
  if (typeof window.matchMedia === 'function') {
    window.matchMedia('(max-width: 480px)').addEventListener('change', () => render());
  }

  render();

  // The cache for every profile, so a tab switch is never a blank screen. It is
  // deliberately not awaited: a storage layer that never answers, which is what
  // a blocked or evicted origin looks like, must not be able to hold up the
  // network fetch that is the real source of truth.
  const config = state.config;
  for (const profile of config.profiles) void loadCached(profile.key);

  // The visible profile first, then the others: a prefetch is worth doing but
  // never at the cost of the tab the reader is actually on.
  if (state.profileKey !== null) await refreshProfile(state.profileKey, true);
  void refreshMessages();
  for (const profile of config.profiles) {
    if (profile.key !== state.profileKey) void refreshProfile(profile.key, true);
  }

  // Refresh only while the document is visible: a phone left in a pocket should
  // not keep hitting a free API all day.
  setInterval(() => {
    if (document.visibilityState !== 'visible') return;
    if (state.profileKey !== null) void refreshProfile(state.profileKey, true);
  }, REFRESH_MS);
  document.addEventListener('visibilitychange', () => {
    if (document.visibilityState !== 'visible') return;
    if (state.profileKey !== null) void refreshProfile(state.profileKey);
  });
  setInterval(tick, TICK_MS);
}

void boot();
