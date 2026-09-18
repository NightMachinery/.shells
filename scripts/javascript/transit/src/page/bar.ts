import type { Board } from '../model.ts';
import { visibleRows } from './board.ts';
import { button, clockTime, dayOffset, el, minutesUntil } from './dom.ts';
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
  const ring = el('span', `refresh-ring${context.state.inFlight > 0 ? ' spinning' : ''}`);
  control.append(ring);

  const age = el('span', 'refresh-age');
  const write = (): void => {
    if (context.state.inFlight > 0) {
      age.textContent = 'updating';
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
    chip.append(el('span', 'chip-title', board.title));
    const when = el('span', 'chip-next');
    const write = (): void => {
      when.textContent = next === undefined ? '-' : `${minutesUntil(next.realtime, Date.now())}'`;
    };
    write();
    context.ticks.push(write);
    chip.append(when);
    chip.title = next === undefined ? `${board.title}: nothing in the window` : `${board.title}: next at ${clockTime(next.realtime, context.config.defaults.timezone)}`;
    chip.addEventListener('click', () => {
      document.getElementById(`board-${index}`)?.scrollIntoView({ behavior: 'smooth', block: 'start' });
    });
    bar.append(chip);
  });
  return bar;
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
  const offset = dayOffset(endMs, startMs, timezone);
  wrap.append(el('span', 'horizon-end', `to ${clockTime(endMs, timezone)}${offset > 0 ? `+${offset}` : ''}`));
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
    const offset = dayOffset(startMs, context.now, timezone);
    const note = el('span', 'start-note', `showing ${clockTime(startMs, timezone)}${offset > 0 ? ` +${offset}` : ''}`);
    note.title = 'a timetable for the moment you picked. Nothing here is live.';
    wrap.append(note);
  }
  return wrap;
}

export function renderBar(context: BarContext): HTMLElement {
  const bar = el('header', 'bar');

  const top = el('div', 'bar-row bar-top');
  top.append(renderTabs(context));
  top.append(renderRefresh(context));
  bar.append(top);

  const chips = renderChips(context);
  if (chips !== null) bar.append(chips);

  const controls = el('div', 'bar-row bar-controls');
  controls.append(renderHorizon(context));
  controls.append(renderStart(context));
  if (context.backends.length > 0) {
    const source = el('span', 'bar-backend', context.backends.join(' + '));
    source.title =
      context.backends.length === 1
        ? `every board here was answered by ${context.backends[0]}`
        : 'two sources answered: the live one for the near window and the timetable for the rest';
    controls.append(source);
  }
  bar.append(controls);

  if (context.state.lastError !== null) {
    bar.append(el('p', 'bar-error', `last refresh failed: ${context.state.lastError}`));
  }
  return bar;
}
