import type { Board, Departure } from '../model.ts';
import { visibleRows } from './board.ts';
import { button, compact, el, minutesUntil, timeLabel, timeNode } from './dom.ts';
import { lineBadge } from './journey.ts';
import { attachTip } from './tip.ts';
import type { ExportedConfig, PageState } from './types.ts';

// The sticky bar: which profile, how far ahead, from when, how fresh, and a way
// to jump to any board without scrolling for it.

/**
 * The horizons on offer, in minutes.
 *
 * Five steps rather than a free number, because the choice is really between
 * four questions: what is leaving now, what is leaving this morning, what is
 * left today, and what does tomorrow morning look like. A slider would invite
 * precision that means nothing.
 */
export const HORIZONS: readonly number[] = [60, 180, 360, 720, 1440];

function horizonLabel(minutes: number): string {
  return minutes % 60 === 0 ? `${minutes / 60} h` : `${minutes} min`;
}

export interface BarContext {
  state: PageState;
  config: ExportedConfig;
  boards: Board[];
  now: number;
  /** Backends that actually answered the current data, for the provenance note. */
  backends: string[];
  ageSeconds: number | null;
  onProfile: (key: string) => void;
  onHorizon: (minutes: number) => void;
  onStart: (mode: 'now' | 'picked', ms: number) => void;
  onRefresh: () => void;
  onDestination: (key: string | null) => void;
  onSort: (value: boolean) => void;
  /** Registered by the age label so the one-second tick can patch it in place. */
  ticks: Array<() => void>;
}

/** The `YYYY-MM-DDTHH:mm` a `datetime-local` input wants, in the configured zone. */
function localInputValue(epochMs: number, timezone: string): string {
  const parts = new Intl.DateTimeFormat('en-CA', {
    year: 'numeric',
    month: '2-digit',
    day: '2-digit',
    hour: '2-digit',
    minute: '2-digit',
    hour12: false,
    timeZone: timezone,
  }).formatToParts(new Date(epochMs));
  const get = (type: string): string => parts.find((part) => part.type === type)?.value ?? '00';
  return `${get('year')}-${get('month')}-${get('day')}T${get('hour')}:${get('minute')}`;
}

/**
 * The next occurrence of a wall-clock hour, in the configured zone.
 *
 * Implemented by stepping quarter-hours rather than by constructing a date in
 * another zone, which cannot be done from a plain `Date` without either a
 * library or arithmetic that breaks twice a year at the daylight-saving
 * boundary. Stepping and formatting asks the platform the question it can
 * actually answer.
 */
function nextWallClock(fromMs: number, hour: number, timezone: string): number {
  const step = 15 * 60_000;
  let candidate = Math.ceil(fromMs / step) * step;
  for (let index = 0; index < 4 * 24 * 2; index += 1) {
    const formatted = new Intl.DateTimeFormat('en-GB', {
      hour: '2-digit',
      minute: '2-digit',
      hour12: false,
      timeZone: timezone,
    }).format(new Date(candidate));
    if (formatted === `${String(hour).padStart(2, '0')}:00`) return candidate;
    candidate += step;
  }
  return fromMs;
}

function renderRefresh(context: BarContext): HTMLElement {
  const wrap = el('div', 'refresh-wrap');
  const control = button('refresh', undefined, 'refresh now');
  control.setAttribute('aria-label', 'refresh now');

  // The ring is the same element whether or not a fetch is running, so the
  // button does not change size when one starts. Only the spin class moves.
  // Planning counts as work in flight. It is the slower half of a refresh, and
  // a ring that stopped while the journeys were still being worked out said the
  // page was idle when it was not.
  const busy = context.state.inFlight > 0 || context.state.planInFlight > 0;
  const ring = el('span', `refresh-ring${busy ? ' spinning' : ''}`);
  control.append(ring);

  const age = el('span', 'refresh-age');
  const write = (): void => {
    if (context.state.inFlight > 0) {
      age.textContent = 'updating';
      return;
    }
    if (context.state.planInFlight > 0) {
      age.textContent = 'planning';
      return;
    }
    age.textContent = context.ageSeconds === null ? 'no data' : `${context.ageSeconds}s`;
  };
  write();
  context.ticks.push(write);
  control.append(age);

  control.addEventListener('click', context.onRefresh);
  wrap.append(control);
  return wrap;
}

function renderTabs(context: BarContext): HTMLElement {
  const nav = el('nav', 'tabs');
  context.config.profiles.forEach((profile, index) => {
    const tab = button(`tab${profile.key === context.state.profileKey ? ' active' : ''}`, profile.title);
    if (index < 9) tab.title = `press ${index + 1}`;
    tab.addEventListener('click', () => context.onProfile(profile.key));
    nav.append(tab);
  });
  return nav;
}

/**
 * One chip per board, saying how long until its next departure and scrolling to
 * it when tapped.
 *
 * The summary counts only the rows the board is actually showing. A chip that
 * promised a departure the board's own filter is hiding would send someone to a
 * stop for a bus they had already decided not to take.
 */
function renderChips(context: BarContext): HTMLElement | null {
  if (context.boards.length === 0) return null;
  const bar = el('div', 'chips');
  context.boards.forEach((board, index) => {
    const profileKey = context.state.profileKey;
    if (profileKey === null) return;
    const rows = visibleRows(profileKey, board);
    const next = rows.find((dep) => dep.realtime >= context.now && !dep.cancelled);
    const chip = button('chip');
    // The chip is the narrowest thing on the page; the full title is on the
    // tooltip below, so nothing is lost by shortening what is drawn.
    chip.append(el('span', 'chip-title', compact(board.title)));
    const when = el('span', 'chip-next');
    const write = (): void => {
      when.textContent = next === undefined ? '-' : `${minutesUntil(next.realtime, Date.now())}'`;
    };
    write();
    context.ticks.push(write);
    chip.append(when);
    // A tap jumps to the board, so the tip opens on hover and on a long press
    // rather than on a tap: taking the tap for an explanation would break the
    // one thing the chip is for.
    attachTip(chip, () => chipTip(board, next, context), { label: chipLabel(board, next, context), tapOpens: false });
    chip.addEventListener('click', () => {
      document.getElementById(`board-${index}`)?.scrollIntoView({ behavior: 'smooth', block: 'start' });
    });
    bar.append(chip);
  });
  return bar;
}

/** What a jump chip says in plain text. */
function chipLabel(board: Board, next: Departure | undefined, context: BarContext): string {
  const timezone = context.config.defaults.timezone;
  if (next === undefined) return `${board.title}: nothing in the window, tap to jump to the board`;
  const minutes = minutesUntil(next.realtime, context.now);
  const when = minutes === 0 ? 'now' : `in ${minutes} min`;
  return `${board.title}, ${next.line} towards ${next.destination}: next departure ${when} (${timeLabel(next.realtime, context.now, timezone)}), tap to jump to the board`;
}

/** The same, as the page's own tooltip. */
function chipTip(board: Board, next: Departure | undefined, context: BarContext): HTMLElement {
  const timezone = context.config.defaults.timezone;
  const body = el('div', 'tip-body');
  const head = el('div', 'tip-head');
  if (next !== undefined) head.append(lineBadge(next));
  head.append(el('span', 'tip-title', board.title));
  body.append(head);
  if (next === undefined) {
    body.append(el('p', 'tip-note', 'nothing in the window'));
  } else {
    const line = el('p', 'tip-next');
    const minutes = minutesUntil(next.realtime, context.now);
    line.append(el('span', undefined, `towards ${next.destination}, ${minutes === 0 ? 'now' : `in ${minutes} min`} at `));
    line.append(timeNode(next.realtime, context.now, timezone));
    body.append(line);
  }
  body.append(el('p', 'tip-note', 'tap to jump to this board'));
  return body;
}

function renderHorizon(context: BarContext): HTMLElement {
  const wrap = el('div', 'horizon');
  const group = el('div', 'segmented');
  for (const minutes of HORIZONS) {
    const option = button(`segment${minutes === context.state.horizonMinutes ? ' active' : ''}`, horizonLabel(minutes));
    option.addEventListener('click', () => context.onHorizon(minutes));
    group.append(option);
  }
  wrap.append(group);

  // The end time rather than only the span, because "6 h" does not answer "does
  // this reach the last train" without arithmetic the reader should not be
  // doing on a platform.
  const startMs = context.state.startMode === 'picked' ? context.state.startMs : context.now;
  const endMs = startMs + context.state.horizonMinutes * 60_000;
  const timezone = context.config.defaults.timezone;
  const end = el('span', 'horizon-end');
  end.append(el('span', undefined, 'to '));
  end.append(timeNode(endMs, startMs, timezone));
  wrap.append(end);
  return wrap;
}

function renderStart(context: BarContext): HTMLElement {
  const wrap = el('div', `start${context.state.startMode === 'picked' ? ' picked' : ''}`);
  const timezone = context.config.defaults.timezone;
  const startMs = context.state.startMode === 'picked' ? context.state.startMs : context.now;

  const now = button(`start-now${context.state.startMode === 'now' ? ' active' : ''}`, 'Now', 'show what is leaving from this moment');
  now.addEventListener('click', () => context.onStart('now', Date.now()));
  wrap.append(now);

  const input = document.createElement('input');
  input.type = 'datetime-local';
  input.className = 'start-input';
  input.value = localInputValue(startMs, timezone);
  input.min = localInputValue(context.now, timezone);
  input.addEventListener('change', () => {
    const picked = Date.parse(input.value);
    if (Number.isFinite(picked)) context.onStart('picked', Math.max(picked, context.now));
  });
  wrap.append(input);

  const nudges: Array<[string, () => number]> = [
    ['-15', () => startMs - 15 * 60_000],
    ['+15', () => startMs + 15 * 60_000],
    ['+1 h', () => startMs + 60 * 60_000],
    ['08:00', () => nextWallClock(context.now, 8, timezone)],
  ];
  for (const [label, compute] of nudges) {
    const nudge = button('start-nudge', label);
    nudge.addEventListener('click', () => context.onStart('picked', Math.max(compute(), context.now)));
    wrap.append(nudge);
  }

  if (context.state.startMode === 'picked') {
    const note = el('span', 'start-note');
    note.append(el('span', undefined, 'showing '));
    note.append(timeNode(startMs, context.now, timezone));
    note.title = 'a timetable for the moment you picked. Nothing here is live.';
    wrap.append(note);
  }
  return wrap;
}

/**
 * Where the commute view plans to, and whether rows are ordered by arrival.
 *
 * Only rendered when the configuration carries places and the visible profile
 * has at least one board that opted in. A picker offering nowhere to go, or
 * governing nothing, is worse than no picker: it invites a reader to try it and
 * then shows them the same screen.
 */
function renderDestination(context: BarContext): HTMLElement | null {
  const places = context.config.places ?? [];
  if (places.length === 0) return null;
  const profile = context.config.profiles.find((entry) => entry.key === context.state.profileKey);
  if (profile === undefined || !profile.boards.some((board) => board.commute)) return null;

  const wrap = el('div', 'destination-pick');
  wrap.append(el('span', 'destination-label', 'to'));

  const select = document.createElement('select');
  select.className = 'destination-select';
  const off = document.createElement('option');
  off.value = '';
  off.textContent = 'nowhere';
  select.append(off);
  // Doorsteps first, then the stations a reader travels to, unless the profile
  // names its own order. A doorstep is where a journey usually ends, so those
  // are the answers worth putting under the thumb; a station is picked
  // deliberately, on the occasions it is wanted, and can afford the scroll.
  const declared = profile.destinations ?? null;
  const offered = [...places].filter((place) => place.name !== context.state.profileKey);
  offered.sort((a, b) => {
    if (declared !== null) {
      const left = declared.indexOf(a.name);
      const right = declared.indexOf(b.name);
      if (left !== right) return (left === -1 ? declared.length : left) - (right === -1 ? declared.length : right);
      return 0;
    }
    return Number(a.stop !== null) - Number(b.stop !== null);
  });
  for (const place of offered) {
    const option = document.createElement('option');
    option.value = place.name;
    // A stop place says what it is called; a doorstep borrows the tab's title,
    // which is the name a reader recognises, and falls back to the raw key.
    option.textContent =
      place.label ?? context.config.profiles.find((entry) => entry.key === place.name)?.title ?? place.name;
    select.append(option);
  }
  select.value = context.state.destinationKey ?? '';
  select.addEventListener('change', () => context.onDestination(select.value === '' ? null : select.value));
  wrap.append(select);

  if (context.state.destinationKey !== null) {
    const sort = button(`sort-arrival${context.state.sortByArrival ? ' active' : ''}`, 'by arrival', 'order commute rows by when they get you there, not when they leave');
    sort.addEventListener('click', () => context.onSort(!context.state.sortByArrival));
    wrap.append(sort);
  }
  return wrap;
}

export function renderBar(context: BarContext): HTMLElement {
  const bar = el('header', 'bar');

  const top = el('div', 'bar-row bar-top');
  top.append(renderTabs(context));
  // The provenance sits with the freshness rather than with the controls: both
  // answer "how much do I trust what I am looking at", and a board repeats its
  // own backend only when it disagrees with this one.
  const right = el('div', 'bar-right');
  if (context.backends.length > 0) {
    const source = el('span', 'bar-backend', context.backends.join(' + '));
    source.title =
      context.backends.length === 1
        ? `every board here was answered by ${context.backends[0]}`
        : 'two sources answered: the live one for the near window and the timetable for the rest';
    right.append(source);
  }
  right.append(renderRefresh(context));
  top.append(right);
  bar.append(top);

  const chips = renderChips(context);
  if (chips !== null) bar.append(chips);

  const controls = el('div', 'bar-row bar-controls');
  controls.append(renderHorizon(context));
  // Before the start-time control, which is wider and used far less often. The
  // controls row scrolls sideways on a phone, so what comes first is what a
  // reader can reach without scrolling, and "where am I going" beats "nudge the
  // clock by a quarter of an hour".
  const destination = renderDestination(context);
  if (destination !== null) controls.append(destination);
  controls.append(renderStart(context));
  bar.append(controls);

  if (context.state.lastError !== null) {
    bar.append(el('p', 'bar-error', `last refresh failed: ${context.state.lastError}`));
  }
  return bar;
}
