import { buildLabel } from './build.ts';
import type { Board, Departure } from '../model.ts';
import { narrowViewport, visibleRows } from './board.ts';
import { button, compact, el, minutesUntil, timeLabel, timeNode } from './dom.ts';
import { lineBadge } from './journey.ts';
import { attachTip } from './tip.ts';
import type { ExportedConfig, ExportedProfile, PageState } from './types.ts';

// The sticky bar: which profile, how far ahead, from when, how fresh, and a way
// to jump to any board without scrolling for it.
//
// The bar is built once and patched from then on. It used to be rebuilt from
// state like everything else, which is the right default for a board and the
// wrong one here, because the controls row scrolls sideways and a scroll
// position lives on the element rather than in the model. A reader who had
// scrolled the row to reach the start-time nudges lost that scroll every thirty
// seconds, to a refresh that had changed nothing they were looking at. The rule
// this file now follows is that a data update never re-creates a control the
// reader can be in the middle of using: text nodes, classes and disabled states
// are patched, and a node is replaced only when the thing it lists has actually
// changed shape.

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

// ------------------------------------------------------------ patch helpers

/** Set text only when it differs, so an unchanged label is not touched at all. */
function setText(node: HTMLElement, text: string): void {
  if (node.textContent !== text) node.textContent = text;
}

/** Add or remove one class without reading the class list twice. */
function setClass(node: HTMLElement, name: string, on: boolean): void {
  node.classList.toggle(name, on);
}

/** Put a node in the tree before `before`, or take it out, idempotently. */
function setPresent(parent: HTMLElement, node: HTMLElement, present: boolean, before: Node | null = null): void {
  const inTree = node.parentNode === parent;
  if (present === inTree) return;
  if (present) parent.insertBefore(node, before);
  else node.remove();
}

/**
 * A slot holding one rendered instant, replaced only when the instant changes.
 *
 * A clock time is drawn as an element rather than as text, because it may carry
 * a `+1` day marker, so it cannot be patched with `textContent`. Remembering the
 * plain-text form of what is in there is what makes "has this changed" a string
 * comparison rather than a rebuild.
 */
interface TimeSlot {
  node: HTMLElement;
  label: string | null;
}

function timeSlot(className?: string): TimeSlot {
  return { node: el('span', className), label: null };
}

function setTime(slot: TimeSlot, epochMs: number, referenceMs: number, timezone: string): void {
  const label = timeLabel(epochMs, referenceMs, timezone);
  if (slot.label === label) return;
  slot.label = label;
  slot.node.replaceChildren(timeNode(epochMs, referenceMs, timezone));
}

// ------------------------------------------------------------------- pieces

interface ChipEntry {
  root: HTMLButtonElement;
  title: HTMLElement;
  when: HTMLElement;
  /** Patched by the one-second tick and by every update. */
  write: () => void;
  board: Board;
  next: Departure | undefined;
}

interface DestinationParts {
  wrap: HTMLElement;
  select: HTMLSelectElement;
  sort: HTMLButtonElement;
  /** The offered places, as one string, so the options are rebuilt only when they change. */
  offered: string;
}

interface StartParts {
  wrap: HTMLElement;
  now: HTMLButtonElement;
  input: HTMLInputElement;
  note: HTMLElement;
  noteTime: TimeSlot;
}

interface BarView {
  root: HTMLElement;
  /** The most recent context, which every event handler reads instead of closing over one. */
  context: BarContext;
  tabs: HTMLElement;
  tabButtons: Map<string, HTMLButtonElement>;
  tabsKey: string;
  right: HTMLElement;
  backend: HTMLElement;
  refreshWrap: HTMLElement;
  ring: HTMLElement;
  age: HTMLElement;
  writeAge: () => void;
  chips: HTMLElement;
  chipEntries: ChipEntry[];
  chipsKey: string;
  controls: HTMLElement;
  segments: Array<{ minutes: number; node: HTMLButtonElement }>;
  horizonEnd: TimeSlot;
  destination: DestinationParts;
  start: StartParts;
  error: HTMLElement;
}

let view: BarView | null = null;

/** Throw the bar away, so the next render builds a fresh one. For tests. */
export function resetBar(): void {
  view = null;
}

/**
 * The jump chips, one per board, saying how long until its next departure.
 *
 * The summary counts only the rows the board is actually showing. A chip that
 * promised a departure the board's own filter is hiding would send someone to a
 * stop for a bus they had already decided not to take.
 */
function chipsKeyOf(boards: Board[]): string {
  return boards.map((board) => board.title).join('\u0001');
}

function buildChip(bar: BarView, index: number): ChipEntry {
  const root = button('chip');
  // The chip is the narrowest thing on the page; the full title is on the
  // tooltip below, so nothing is lost by shortening what is drawn.
  const title = el('span', 'chip-title');
  const when = el('span', 'chip-next');
  root.append(title, when);
  const entry: ChipEntry = {
    root,
    title,
    when,
    board: bar.context.boards[index] as Board,
    next: undefined,
    write: () => undefined,
  };
  entry.write = (): void => {
    setText(entry.when, entry.next === undefined ? '-' : `${minutesUntil(entry.next.realtime, Date.now())}'`);
  };
  // A tap jumps to the board, so the tip opens on hover and on a long press
  // rather than on a tap: taking the tap for an explanation would break the
  // one thing the chip is for. Both the tip and its label read the entry, which
  // the update below keeps current, so the chip itself is never rebuilt.
  attachTip(root, () => chipTip(entry.board, entry.next, bar.context), { tapOpens: false });
  root.addEventListener('click', () => {
    document.getElementById(`board-${index}`)?.scrollIntoView({ behavior: 'smooth', block: 'start' });
  });
  return entry;
}

function updateChips(bar: BarView, context: BarContext): void {
  const key = chipsKeyOf(context.boards);
  if (key !== bar.chipsKey) {
    bar.chipsKey = key;
    bar.chipEntries = context.boards.map((_, index) => buildChip(bar, index));
    bar.chips.replaceChildren(...bar.chipEntries.map((entry) => entry.root));
  }
  setPresent(bar.root, bar.chips, context.boards.length > 0, bar.controls);
  const profileKey = context.state.profileKey;
  bar.chipEntries.forEach((entry, index) => {
    const board = context.boards[index];
    if (board === undefined || profileKey === null) return;
    entry.board = board;
    const rows = visibleRows(profileKey, board);
    entry.next = rows.find((dep) => dep.realtime >= context.now && !dep.cancelled);
    setText(entry.title, compact(board.title));
    const label = chipLabel(board, entry.next, context);
    if (entry.root.getAttribute('aria-label') !== label) {
      // Not `title`: the chip already carries the page's own tooltip
      // (`chipTip`, attached below), and a native bubble on top of it would
      // draw the same answer twice.
      entry.root.setAttribute('aria-label', label);
    }
    entry.write();
    context.ticks.push(entry.write);
  });
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

/**
 * Where the commute view plans to, and whether rows are ordered by arrival.
 *
 * Only shown when the configuration carries places and the visible profile has
 * at least one board that opted in. A picker offering nowhere to go, or
 * governing nothing, is worse than no picker: it invites a reader to try it and
 * then shows them the same screen.
 */
function destinationOffered(context: BarContext): Array<{ name: string; label: string }> | null {
  const places = context.config.places ?? [];
  if (places.length === 0) return null;
  const profile = context.config.profiles.find((entry) => entry.key === context.state.profileKey);
  if (profile === undefined || !profile.boards.some((board) => board.commute)) return null;
  // Every commute board here already knows where it is going, so there is
  // nothing left to pick. A control whose every setting changes nothing is worse
  // than no control: the reader tries it, and concludes the page is broken.
  if (profile.boards.every((board) => !board.commute || (board.destination ?? null) !== null)) return null;

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
  return offered.map((place) => {
    // A stop place says what it is called; a doorstep borrows the tab's title,
    // which is the name a reader recognises, and falls back to the raw key. The
    // glyph comes from the place, or from the profile the place is named after,
    // so a doorstep and its tab are marked the same way without saying so twice
    // in the configuration.
    const profile = context.config.profiles.find((entry) => entry.key === place.name);
    const label = place.label ?? profile?.title ?? place.name;
    const emoji = place.emoji ?? profile?.emoji ?? null;
    return {
      name: place.name,
      label: emoji === null || emoji.length === 0 ? label : `${emoji} ${label}`,
    };
  });
}

function updateDestination(bar: BarView, context: BarContext): void {
  const offered = destinationOffered(context);
  setPresent(bar.controls, bar.destination.wrap, offered !== null, bar.start.wrap);
  if (offered === null) return;
  const key = offered.map((place) => `${place.name}:${place.label}`).join('\u0001');
  if (key !== bar.destination.offered) {
    bar.destination.offered = key;
    const options: HTMLOptionElement[] = [];
    const off = document.createElement('option');
    off.value = '';
    off.textContent = 'nowhere';
    options.push(off);
    for (const place of offered) {
      const option = document.createElement('option');
      option.value = place.name;
      option.textContent = place.label;
      options.push(option);
    }
    bar.destination.select.replaceChildren(...options);
  }
  // Never while the reader has the menu open: setting `value` on a focused
  // select is how a picker changes under a thumb that is already on it.
  const wanted = context.state.destinationKey ?? '';
  if (document.activeElement !== bar.destination.select && bar.destination.select.value !== wanted) {
    bar.destination.select.value = wanted;
  }
  setClass(bar.destination.sort, 'active', context.state.sortByArrival);
  setPresent(bar.destination.wrap, bar.destination.sort, context.state.destinationKey !== null);
}

function updateStart(bar: BarView, context: BarContext): void {
  const picked = context.state.startMode === 'picked';
  const timezone = context.config.defaults.timezone;
  const startMs = picked ? context.state.startMs : context.now;
  setClass(bar.start.wrap, 'picked', picked);
  setClass(bar.start.now, 'active', !picked);
  // The value is the reader's if they are typing in it. A `datetime-local`
  // being rewritten mid-entry loses the digits already typed and the caret.
  if (document.activeElement !== bar.start.input) {
    const value = localInputValue(startMs, timezone);
    if (bar.start.input.value !== value) bar.start.input.value = value;
  }
  const min = localInputValue(context.now, timezone);
  if (bar.start.input.min !== min) bar.start.input.min = min;
  setPresent(bar.start.wrap, bar.start.note, picked);
  if (picked) setTime(bar.start.noteTime, startMs, context.now, timezone);
}

// -------------------------------------------------------------------- build

function createBar(context: BarContext): BarView {
  const root = el('header', 'bar');
  const top = el('div', 'bar-row bar-top');
  const tabs = el('nav', 'tabs');
  const right = el('div', 'bar-right');
  const backend = el('span', 'bar-backend');
  const refreshWrap = el('div', 'refresh-wrap');
  const control = button('refresh');
  control.setAttribute('aria-label', 'refresh now');
  // The ring is the same element whether or not a fetch is running, so the
  // button does not change size when one starts. Only the spin class moves.
  const ring = el('span', 'refresh-ring');
  const age = el('span', 'refresh-age');
  control.append(ring, age);
  refreshWrap.append(control);
  right.append(refreshWrap);
  top.append(tabs, right);
  root.append(top);

  const chips = el('div', 'chips');

  const controls = el('div', 'bar-row bar-controls');
  const horizon = el('div', 'horizon');
  const group = el('div', 'segmented');
  const segments = HORIZONS.map((minutes) => {
    const node = button('segment', horizonLabel(minutes));
    group.append(node);
    return { minutes, node };
  });
  horizon.append(group);
  // The end time rather than only the span, because "6 h" does not answer "does
  // this reach the last train" without arithmetic the reader should not be
  // doing on a platform.
  const horizonEnd = timeSlot('horizon-end-time');
  const end = el('span', 'horizon-end');
  end.append(el('span', undefined, 'to '), horizonEnd.node);
  horizon.append(end);
  controls.append(horizon);

  const destinationWrap = el('div', 'destination-pick');
  destinationWrap.append(el('span', 'destination-label', 'to'));
  const select = document.createElement('select');
  select.className = 'destination-select';
  destinationWrap.append(select);
  const sort = button('sort-arrival', 'by arrival');
  sort.setAttribute('aria-label', 'order commute rows by when they get you there, not when they leave');

  const startWrap = el('div', 'start');
  const startNow = button('start-now', 'Now');
  startNow.setAttribute('aria-label', 'show what is leaving from this moment');
  startWrap.append(startNow);
  const input = document.createElement('input');
  input.type = 'datetime-local';
  input.className = 'start-input';
  startWrap.append(input);
  const noteTime = timeSlot('start-note-time');
  const note = el('span', 'start-note');
  note.append(el('span', undefined, 'showing '), noteTime.node);
  note.setAttribute('aria-label', 'a timetable for the moment you picked. Nothing here is live.');

  // Before the start-time control, which is wider and used far less often. The
  // controls row scrolls sideways on a phone, so what comes first is what a
  // reader can reach without scrolling, and "where am I going" beats "nudge the
  // clock by a quarter of an hour".
  controls.append(startWrap);
  root.append(controls);

  const error = el('p', 'bar-error');

  const bar: BarView = {
    root,
    context,
    tabs,
    tabButtons: new Map(),
    tabsKey: '',
    right,
    backend,
    refreshWrap,
    ring,
    age,
    writeAge: () => undefined,
    chips,
    chipEntries: [],
    chipsKey: '\u0000never',
    controls,
    segments,
    horizonEnd,
    destination: { wrap: destinationWrap, select, sort, offered: '\u0000never' },
    start: { wrap: startWrap, now: startNow, input, note, noteTime },
    error,
  };

  control.addEventListener('click', () => bar.context.onRefresh());
  for (const segment of segments) {
    segment.node.addEventListener('click', () => bar.context.onHorizon(segment.minutes));
  }
  select.addEventListener('change', () => bar.context.onDestination(select.value === '' ? null : select.value));
  sort.addEventListener('click', () => bar.context.onSort(!bar.context.state.sortByArrival));
  startNow.addEventListener('click', () => bar.context.onStart('now', Date.now()));
  input.addEventListener('change', () => {
    const value = Date.parse(input.value);
    if (Number.isFinite(value)) bar.context.onStart('picked', Math.max(value, bar.context.now));
  });
  const nudges: Array<[string, () => number]> = [
    ['-15', () => startOf(bar) - 15 * 60_000],
    ['+15', () => startOf(bar) + 15 * 60_000],
    ['+1 h', () => startOf(bar) + 60 * 60_000],
    ['08:00', () => nextWallClock(bar.context.now, 8, bar.context.config.defaults.timezone)],
  ];
  for (const [label, compute] of nudges) {
    const nudge = button('start-nudge', label);
    nudge.addEventListener('click', () => bar.context.onStart('picked', Math.max(compute(), bar.context.now)));
    startWrap.append(nudge);
  }
  // Appended after the nudges so it stays the last thing in the row.
  startWrap.append(note);
  note.remove();

  bar.writeAge = (): void => {
    const state = bar.context.state;
    const busy = state.inFlight > 0 || state.planInFlight > 0;
    // On a phone the word is dropped and the age is left standing.
    //
    // Not for tidiness: "updating" is three times the width of "4s", the right
    // of the bar takes whatever width it asks for, and the tabs are what
    // yields, so the widest word here decided whether the last profile tab was
    // on the screen. It went off the edge every time a refresh started. The
    // ring beside it is already spinning, which is the same news in no width at
    // all, and which of the two halves of a refresh is running is a question
    // the timing lines in the filter answer properly.
    if (busy && !narrowViewport()) {
      // Planning counts as work in flight. It is the slower half of a refresh,
      // and a ring that stopped while the journeys were still being worked out
      // said the page was idle when it was not.
      setText(age, state.inFlight > 0 ? 'updating' : 'planning');
      return;
    }
    setText(age, bar.context.ageSeconds === null ? 'no data' : `${bar.context.ageSeconds}s`);
  };

  markScrollEdges(controls);
  return bar;
}

/** The instant the start control is nudging from. */
function startOf(bar: BarView): number {
  return bar.context.state.startMode === 'picked' ? bar.context.state.startMs : bar.context.now;
}

/**
 * What a tab says.
 *
 * The glyph is a landmark rather than a label: it is there so a thumb can find
 * the right tab without reading, and the word stays beside it so a reader who
 * does not recognise the picture is not guessing. On a narrow screen the word
 * shrinks to the profile's short form, because four full titles either wrap the
 * bar onto a second line or push the last tab off the edge, and both of those
 * cost more than the letters do. The full title is still the tab's accessible
 * name and its tooltip, so the abbreviation is never the only thing on offer.
 */
export function tabLabel(profile: ExportedProfile, narrow: boolean): string {
  const name = narrow ? (profile.short ?? profile.key.toUpperCase()) : profile.title;
  const emoji = profile.emoji ?? null;
  return emoji === null || emoji.length === 0 ? name : `${emoji} ${name}`;
}

function updateTabs(bar: BarView, context: BarContext): void {
  const key = context.config.profiles
    .map((profile) => `${profile.key}:${profile.title}:${profile.emoji ?? ''}:${profile.short ?? ''}`)
    .join('\u0001');
  if (key !== bar.tabsKey) {
    bar.tabsKey = key;
    bar.tabButtons = new Map();
    const nodes = context.config.profiles.map((profile, index) => {
      const tab = button('tab');
      // The name a screen reader reads is the full one whatever is drawn,
      // because the short form is an abbreviation this page invented and
      // nobody else uses. Not `title`: it would draw the browser's own
      // tooltip beside nothing, since a tab carries no custom one. The press
      // number is a keyboard shortcut, not part of the tab's name, so it goes
      // on the ARIA property built for exactly that rather than into the label.
      tab.setAttribute('aria-label', profile.title);
      if (index < 9) tab.setAttribute('aria-keyshortcuts', String(index + 1));
      tab.addEventListener('click', () => bar.context.onProfile(profile.key));
      bar.tabButtons.set(profile.key, tab);
      return tab;
    });
    bar.tabs.replaceChildren(...nodes);
  }
  // Which label fits depends on how wide the screen is, and a phone changes
  // width when it is turned over, so this is patched on every update rather
  // than set once when the tab is built.
  const narrow = narrowViewport();
  for (const profile of context.config.profiles) {
    const tab = bar.tabButtons.get(profile.key);
    if (tab === undefined) continue;
    setText(tab, tabLabel(profile, narrow));
    setClass(tab, 'active', profile.key === context.state.profileKey);
  }
}

function updateBar(bar: BarView, context: BarContext): void {
  updateTabs(bar, context);

  // The provenance sits with the freshness rather than with the controls: both
  // answer "how much do I trust what I am looking at", and a board repeats its
  // own backend only when it disagrees with this one.
  setPresent(bar.right, bar.backend, context.backends.length > 0, bar.refreshWrap);
  if (context.backends.length > 0) {
    setText(bar.backend, context.backends.join(' + '));
    bar.backend.setAttribute(
      'aria-label',
      (context.backends.length === 1
        ? `every board here was answered by ${context.backends[0]}`
        : 'two sources answered: the live one for the near window and the timetable for the rest') + `. ${buildLabel()}`,
    );
  }
  setClass(bar.ring, 'spinning', context.state.inFlight > 0 || context.state.planInFlight > 0);
  bar.writeAge();
  context.ticks.push(bar.writeAge);

  updateChips(bar, context);

  for (const segment of bar.segments) setClass(segment.node, 'active', segment.minutes === context.state.horizonMinutes);
  const startMs = context.state.startMode === 'picked' ? context.state.startMs : context.now;
  setTime(bar.horizonEnd, startMs + context.state.horizonMinutes * 60_000, startMs, context.config.defaults.timezone);

  updateDestination(bar, context);
  updateStart(bar, context);

  setPresent(bar.root, bar.error, context.state.lastError !== null);
  if (context.state.lastError !== null) setText(bar.error, `last refresh failed: ${context.state.lastError}`);
}

/**
 * Mark which side of a horizontally scrolling strip still has content past it.
 *
 * The strip hides its scrollbar so it reads as a bar rather than as a pane,
 * which leaves nothing at all to say that there is another control just off the
 * edge. The stylesheet fades whichever side is overflowing; this is what decides
 * which side that is. The fade comes off once the reader has reached that end,
 * because a permanently dimmed last control looks disabled rather than reachable.
 */
export function markScrollEdges(strip: HTMLElement): void {
  const update = (): void => {
    const slack = strip.scrollWidth - strip.clientWidth;
    // Fractional layout widths leave a sub-pixel remainder that is not content.
    const scrolls = slack > 1;
    strip.classList.toggle('fade-start', scrolls && strip.scrollLeft > 1);
    strip.classList.toggle('fade-end', scrolls && strip.scrollLeft < slack - 1);
  };
  strip.addEventListener('scroll', update, { passive: true });
  // Rotation and a font change both resize the strip without re-rendering it.
  // The observer dies with the element, which a window listener would not.
  if (typeof ResizeObserver === 'function') new ResizeObserver(update).observe(strip);
  // Nothing has a width until the bar is in the document.
  requestAnimationFrame(update);
}

export function renderBar(context: BarContext): HTMLElement {
  if (view === null) view = createBar(context);
  view.context = context;
  updateBar(view, context);
  return view.root;
}
