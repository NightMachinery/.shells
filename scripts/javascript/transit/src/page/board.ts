import { contrastText, resolveColor, resolveTextColor } from '../colors.ts';
import { catchableOnBoard, describeWalk, normaliseLine, walkMinutesFor } from '../filter.ts';
import type { Board, Departure } from '../model.ts';
import { button, clockTime, dayMarker, el, minutesUntil, slot } from './dom.ts';
import { alarmMarker, attachLongPress, openAlarmPopup } from './notify.ts';
import { stopTagOf } from './data.ts';
import { arrivalOf, rowKey, usualExits } from './commute.ts';
import type { PlannedRow } from '../plan.ts';
import { boardId, filterKey, readHidden, readView, writeHidden, writeView } from './store.ts';
import type { BoardStatus, BoardView } from './types.ts';

// One board on the page: its header, its three view states, its filter, and the
// two ways it can draw its departures.

/** How many destinations a strip row names before it gives up and says "and more". */
const STRIP_DESTINATIONS = 2;

export interface BoardContext {
  profileKey: string;
  index: number;
  now: number;
  timezone: string;
  status: BoardStatus;
  /** True while the page is showing a picked moment rather than the present. */
  planned: boolean;
  /** The backend named in the sticky bar; a board only names its own when it differs. */
  barBackend: string;
  onChange: () => void;
  onRetry: () => void;
  /** Journeys from this board to the chosen destination, keyed by row. */
  routes?: Map<string, PlannedRow>;
  /** Whether this board's rows are ordered by arrival rather than departure. */
  sortByArrival: boolean;
  /** The current tight-connection window, shown in the filter of a planned board. */
  earlyBufferMinutes: number;
  /** Called when the reader moves that window; re-plans rather than re-fetches. */
  onEarlyBuffer: (minutes: number) => void;
  /** Registered by rows that need their text patched on the one-second tick. */
  ticks: Array<() => void>;
}

/** The next view in the cycle a header click walks through. */
function nextView(view: BoardView): BoardView {
  if (view === 'integrated') return 'full';
  if (view === 'full') return 'collapsed';
  return 'integrated';
}

export function viewOf(profileKey: string, board: Board, commuting = false): BoardView {
  // A board being planned opens as rows, because the answer the commute view
  // exists to give lives in a row's own slot and would be invisible in a strip.
  // A stored choice still wins: the reader asked for that one.
  return readView(boardId(profileKey, board.title)) ?? (commuting ? 'full' : 'integrated');
}

/**
 * The rows a board is currently showing, after its own line filter.
 *
 * Exported because the sticky bar's "next in N" summary has to agree with what
 * the board shows. A chip that promised a departure the board is hiding would
 * be worse than no chip at all.
 */
export function visibleRows(profileKey: string, board: Board): Departure[] {
  const hidden = readHidden(boardId(profileKey, board.title));
  if (hidden.size === 0) return board.departures;
  return board.departures.filter((dep) => !hidden.has(filterKey(dep.stop, normaliseLine(dep.line))));
}

function lineBadge(dep: Pick<Departure, 'line' | 'mode' | 'color'>): HTMLElement {
  const background = resolveColor(dep);
  const node = el('span', 'badge', dep.line);
  node.style.backgroundColor = background;
  node.style.color = resolveTextColor(dep) ?? contrastText(background);
  return node;
}

/**
 * A time, with its planned time behind it when the two differ.
 *
 * The expected time is the headline because it is the one a rider acts on, and
 * the planned time is kept because a departure that is eleven minutes late is a
 * different thing from one that was always scheduled for then, and only the
 * pair says which. Delay and cancellation are carried by weight and by a struck
 * line as well as by colour, so the distinction survives a monochrome screen
 * and a reader who does not distinguish red from green.
 */
function timeGroup(dep: Departure, timezone: string, referenceMs: number): HTMLElement {
  const wrap = el('span', `times${dep.cancelled ? ' cancelled' : ''}`);
  const late = dep.delayMin > 0;
  const early = dep.delayMin < 0;
  const main = el('span', `time-main${late ? ' late' : ''}${early ? ' early' : ''}`, clockTime(dep.realtime, timezone));
  const marker = dayMarker(dep.realtime, referenceMs, timezone);
  if (marker !== null) main.append(marker);
  wrap.append(main);

  if (dep.delayMin !== 0) {
    wrap.append(el('span', 'time-planned', `(${clockTime(dep.planned, timezone)})`));
    wrap.title = `planned ${clockTime(dep.planned, timezone)}, ${dep.delayMin > 0 ? '+' : ''}${dep.delayMin} min`;
  } else {
    wrap.title = `planned ${clockTime(dep.planned, timezone)}, on time`;
  }
  if (dep.cancelled) wrap.title = `${wrap.title}. Cancelled.`;
  return wrap;
}

/** Which optional columns this board's rows have anything to put in. */
interface Columns {
  connection: boolean;
  route: boolean;
  platform: boolean;
  stop: boolean;
}

function columnsOf(board: Board, rows: Departure[], routes: Map<string, PlannedRow> | undefined): Columns {
  return {
    connection: board.connection !== undefined,
    route: routes !== undefined && routes.size > 0,
    // The upstream feed reports a platform for rail and never for trams or
    // buses, so a fixed platform column would be dead space on most boards.
    // Presence is decided per board, which keeps the slots aligned within a
    // board without spending a column that can never be filled.
    platform: rows.some((row) => row.platform !== null),
    stop: board.stops.length > 1,
  };
}

function gridTemplate(columns: Columns): string {
  const parts = ['var(--col-minutes)', 'var(--col-badge)', 'minmax(0, 1fr)'];
  if (columns.route) parts.push('var(--col-route)');
  if (columns.connection) parts.push('var(--col-connection)');
  if (columns.platform) parts.push('var(--col-platform)');
  parts.push('var(--col-state)', 'var(--col-alarm)');
  if (columns.stop) parts.push('var(--col-stop)');
  return parts.join(' ');
}

/**
 * The commute slot: where this departure puts you down, what you catch there,
 * and when you arrive. A tight option is one that only works if the first leg
 * runs early, so it is never the recommendation and says so when asked.
 */
function renderRoute(dep: Departure, context: BoardContext, usual: Map<string, string>): HTMLElement {
  // A cancelled departure gets no route, however good the planner thinks it is.
  // The planner works from the timetable and does not always know the vehicle
  // has been withdrawn, and a recommendation to take a train that is not running
  // is worse than no recommendation.
  if (dep.cancelled) return slot('route');
  const planned = context.routes?.get(rowKey(dep));
  const option = planned?.best ?? planned?.options[0];
  if (option === undefined) return slot('route');

  const better = usual.get(normaliseLine(dep.line)) !== undefined && usual.get(normaliseLine(dep.line)) !== option.exitStop;
  const node = el('span', `route${option.tight ? ' tight' : ''}${better && !option.tight ? ' better' : ''}`);
  const onward = option.legs[1];
  // The exit name is shortened the same way a destination is, because the slot
  // is narrow and the arrival time is the part a reader acts on. A stop called
  // "Somewhere (Something)" would otherwise push the time out of the slot
  // entirely, which is the one thing here that must never be cut.
  const exit = shortDestination(option.exitStopName);
  const head = onward === undefined ? exit : `${exit} · ${onward.line}`;
  node.append(el('span', 'route-head', head));
  node.append(el('span', 'route-arrival', clockTime(option.arrival, context.timezone)));

  const chain = option.legs.map((leg) => `${leg.line} ${clockTime(leg.departure, context.timezone)}`).join(' → ');
  const parts = [`change at ${option.exitStopName}`, chain, `arrive ${clockTime(option.arrival, context.timezone)}`];
  if (option.tight) parts.push('tight: needs the first leg to run early or the change to be quick');
  if (better) parts.push('a different exit from this line\u2019s usual one');
  node.title = parts.join(' · ');
  return node;
}

function renderRow(dep: Departure, board: Board, columns: Columns, context: BoardContext, usual: Map<string, string>): HTMLElement {
  const reachable = catchableOnBoard(dep, board, context.now);
  const row = el('li', `row${reachable ? '' : ' unreachable'}${dep.cancelled ? ' row-cancelled' : ''}`);
  row.style.gridTemplateColumns = gridTemplate(columns);

  const minutes = el('span', 'minutes', String(minutesUntil(dep.realtime, context.now)));
  // Registered rather than re-rendered: the tick patches this one text node, so
  // a selection elsewhere in the board survives and nothing else reflows.
  context.ticks.push(() => {
    minutes.textContent = String(minutesUntil(dep.realtime, Date.now()));
  });
  row.append(minutes);
  row.append(lineBadge(dep));

  const main = el('span', 'row-main');
  const destination = el('span', 'destination', dep.destination);
  destination.title = dep.destination;
  main.append(destination);
  const meta = el('span', 'row-times');
  meta.append(timeGroup(dep, context.timezone, context.now));
  if (dep.sev) meta.append(el('span', 'flag sev', 'SEV'));
  if (dep.cancelled) meta.append(el('span', 'flag cancelled-flag', 'cancelled'));
  main.append(meta);
  row.append(main);

  if (columns.route) row.append(renderRoute(dep, context, usual));

  if (columns.connection) {
    if (dep.connection === undefined || dep.connection === null) {
      row.append(slot('connection'));
    } else {
      const onward = el('span', 'connection', `${dep.connection.line} ${clockTime(dep.connection.departure, context.timezone)}`);
      const board2 = board.connection;
      onward.title =
        board2 === undefined
          ? 'onward departure'
          : `change after ${board2.rideMinutes} min on board and a ${board2.transferMinutes} min transfer`;
      row.append(onward);
    }
  }

  if (columns.platform) {
    if (dep.platform === null) row.append(slot('platform'));
    else {
      const platform = el('span', 'platform', dep.platform);
      platform.title = `platform ${dep.platform}`;
      row.append(platform);
    }
  }

  const known = dep.realtimeKnown && !context.planned;
  const state = el('span', `state ${known ? 'state-live' : 'state-plan'}`, known ? 'live' : 'plan');
  state.title = known
    ? 'a live time reported by the operator'
    : context.planned
      ? 'a timetable for the moment you picked, not a live time'
      : 'no live time for this departure, the timetable is all there is';
  row.append(state);

  row.append(alarmMarker(dep) ?? slot('alarm'));

  if (columns.stop) {
    const tag = dep.stopTag ?? stopTagOf(dep.stop, board.stopLabels);
    const node = el('span', 'stop-tag', tag);
    node.title = `from ${tag}`;
    row.append(node);
  }

  attachLongPress(row, () => openAlarmPopup(dep, board, row));
  return row;
}

interface Strip {
  head: Departure;
  entries: Departure[];
}

function stripGroups(rows: Departure[]): Strip[] {
  const groups = new Map<string, Strip>();
  for (const row of rows) {
    // Per stop as well as per line and direction: two stops with different
    // walking times merged into one rhythm row would describe a service nobody
    // can take from one doorstep.
    const key = JSON.stringify([normaliseLine(row.line), row.direction, row.stop]);
    const group = groups.get(key);
    if (group === undefined) groups.set(key, { head: row, entries: [row] });
    else group.entries.push(row);
  }
  return [...groups.values()].sort((a, b) => (a.entries[0]?.realtime ?? 0) - (b.entries[0]?.realtime ?? 0));
}

/**
 * What a strip row is called.
 *
 * Grouping is by direction letter, because a letter is the only stable key, but
 * a letter means nothing to a reader. So the group is grouped by the code and
 * labelled by its destinations, most frequent first, with the code kept in the
 * tooltip for the day someone needs to check the configuration against the
 * screen.
 */
function stripLabel(group: Strip): { text: string; title: string } {
  const counts = new Map<string, number>();
  for (const entry of group.entries) counts.set(entry.destination, (counts.get(entry.destination) ?? 0) + 1);
  const ordered = [...counts.entries()].sort((a, b) => b[1] - a[1] || a[0].localeCompare(b[0])).map(([name]) => name);
  const shown = ordered.slice(0, STRIP_DESTINATIONS);
  const text = `→ ${shown.join(' / ')}${ordered.length > shown.length ? ' …' : ''}`;
  const code = group.head.direction === null ? 'no direction code' : `direction ${group.head.direction}`;
  return { text, title: `${code}. Destinations: ${ordered.join(', ')}` };
}

function renderStrip(rows: Departure[], board: Board, context: BoardContext): HTMLElement {
  const list = el('ul', 'strips');
  const multiStop = board.stops.length > 1;
  for (const group of stripGroups(rows)) {
    const item = el('li', 'strip');
    item.append(lineBadge(group.head));
    const label = stripLabel(group);
    const direction = el('span', 'direction', label.text);
    direction.title = label.title;
    item.append(direction);

    // A platform is a property of the group when every run uses the same one,
    // which is the usual case, and only becomes per-time when it is not.
    const platforms = new Set(group.entries.map((entry) => entry.platform ?? ''));
    const uniform = platforms.size === 1 ? [...platforms][0] : undefined;
    if (uniform !== undefined && uniform !== '') {
      const node = el('span', 'platform', uniform);
      node.title = `platform ${uniform}`;
      item.append(node);
    }
    if (multiStop) {
      const tag = group.head.stopTag ?? stopTagOf(group.head.stop, board.stopLabels);
      const node = el('span', 'stop-tag', tag);
      node.title = `from ${tag}`;
      item.append(node);
    }

    const head = group.entries[0]?.destination ?? '';
    const times = el('span', 'times-strip');
    for (const dep of group.entries) {
      const late = dep.delayMin > 0;
      const early = dep.delayMin < 0;
      const cell = el(
        'span',
        `time${dep.cancelled ? ' cancelled' : ''}${late ? ' late' : ''}${early ? ' early' : ''}`,
        clockTime(dep.realtime, context.timezone),
      );
      const marker = dayMarker(dep.realtime, context.now, context.timezone);
      if (marker !== null) cell.append(marker);
      // A run that terminates short of the others is a different journey, and a
      // strip that hides that sends people onto a train that stops before their
      // stop. The suffix is the short form; the whole destination is in the
      // tooltip, along with everything else this time knows.
      if (dep.destination !== head) cell.append(el('span', 'time-note', shortDestination(dep.destination)));
      // A platform only appears per time when the group's platforms disagree.
      // When they agree it is on the row, once, which is where a reader looks.
      if (uniform === undefined && dep.platform !== null) cell.append(el('span', 'time-note', `pl ${dep.platform}`));
      const parts = [dep.destination];
      parts.push(dep.delayMin === 0 ? `planned ${clockTime(dep.planned, context.timezone)}, on time` : `planned ${clockTime(dep.planned, context.timezone)}, ${dep.delayMin > 0 ? '+' : ''}${dep.delayMin} min`);
      if (dep.platform !== null) parts.push(`platform ${dep.platform}`);
      if (dep.cancelled) parts.push('cancelled');
      cell.title = parts.join(' · ');
      if (dep.cancelled) cell.append(el('span', 'time-note', '✕'));
      times.append(cell);
    }
    item.append(times);
    list.append(item);
  }
  return list;
}

/** Enough of a destination to tell two branches of one line apart. */
function shortDestination(name: string): string {
  const head = name.split(/[,(]/)[0] ?? name;
  return head.trim().slice(0, 12);
}

function renderFilter(board: Board, context: BoardContext, rows: Departure[]): HTMLElement {
  const id = boardId(context.profileKey, board.title);
  const hidden = readHidden(id);

  // What there is to filter comes from the data, not the configuration: a board
  // that names no lines still serves several, and those are the ones a reader
  // wants to switch off.
  const byStop = new Map<string, Set<string>>();
  for (const row of board.departures) {
    const set = byStop.get(row.stop) ?? new Set<string>();
    set.add(normaliseLine(row.line));
    byStop.set(row.stop, set);
  }
  const labels = new Map<string, string>();
  for (const row of board.departures) labels.set(normaliseLine(row.line), row.line);

  const popover = el('div', 'filter-popover');
  const total = [...byStop.values()].reduce((sum, set) => sum + set.size, 0);
  if (total <= 1) {
    popover.append(el('p', 'filter-empty', 'only one line here, nothing to filter'));
    appendEarlyBuffer(popover, context);
    return popover;
  }

  const showStops = byStop.size > 1;
  for (const [stop, lines] of byStop) {
    if (showStops) {
      const tag = stopTagOf(stop, board.stopLabels);
      popover.append(el('p', 'filter-stop', tag));
    }
    for (const line of [...lines].sort()) {
      const label = document.createElement('label');
      label.className = 'filter-line';
      const box = document.createElement('input');
      box.type = 'checkbox';
      box.checked = !hidden.has(filterKey(stop, line));
      box.addEventListener('change', () => {
        const next = readHidden(id);
        if (box.checked) next.delete(filterKey(stop, line));
        else next.add(filterKey(stop, line));
        writeHidden(id, next);
        context.onChange();
      });
      label.append(box);
      label.append(el('span', undefined, labels.get(line) ?? line));
      popover.append(label);
    }
  }

  if (rows.length === 0 && board.departures.length > 0) {
    popover.append(el('p', 'filter-empty', 'everything is hidden'));
  }
  appendEarlyBuffer(popover, context);
  return popover;
}

/**
 * The tight-connection window, on planned boards only.
 *
 * It belongs in the filter rather than the bar because it changes what one
 * board shows and means nothing on the others, and because widening it is an
 * occasional question ("what if I run for it") rather than a setting.
 */
function appendEarlyBuffer(popover: HTMLElement, context: BoardContext): void {
  if (context.routes === undefined) return;
  const label = document.createElement('label');
  label.className = 'filter-buffer';
  label.append(el('span', undefined, 'tight window'));
  const input = document.createElement('input');
  input.type = 'number';
  input.min = '0';
  input.max = '15';
  input.step = '1';
  input.value = String(context.earlyBufferMinutes);
  input.title = 'how many minutes early an onward departure may be and still be offered as tight';
  input.addEventListener('change', () => {
    const value = Number(input.value);
    if (!Number.isFinite(value) || value < 0) return;
    context.onEarlyBuffer(Math.min(15, Math.round(value)));
  });
  label.append(input);
  label.append(el('span', 'filter-buffer-unit', 'min'));
  popover.append(label);
}

/** Which board's filter popover is open, if any. Null when none is. */
let openFilter: string | null = null;

export function closeFilters(): void {
  openFilter = null;
}

export function renderBoard(board: Board, context: BoardContext): HTMLElement {
  const id = boardId(context.profileKey, board.title);
  const view = viewOf(context.profileKey, board, context.routes !== undefined);
  const rows = visibleRows(context.profileKey, board);

  const section = el('section', `board board-${view}`);
  section.id = `board-${context.index}`;

  const header = el('header', 'board-header');
  const cycle = button('board-title', board.title, `showing ${view}; click for ${nextView(view)}`);
  cycle.setAttribute('aria-expanded', String(view !== 'collapsed'));
  cycle.addEventListener('click', () => {
    writeView(id, nextView(view));
    context.onChange();
  });
  header.append(cycle);

  const marks = el('span', 'board-marks');
  if (board.backend !== context.barBackend) {
    const node = el('span', 'backend', board.backend);
    node.title = `answered by ${board.backend}, which is not the source the rest of the page used`;
    marks.append(node);
  }
  const hidden = readHidden(id);
  if (hidden.size > 0) marks.append(el('span', 'filter-mark', `${hidden.size} hidden`));
  const filterButton = button('filter-button', '⚙', 'filter the lines on this board');
  filterButton.setAttribute('aria-label', 'filter the lines on this board');
  filterButton.addEventListener('click', (event) => {
    event.stopPropagation();
    openFilter = openFilter === id ? null : id;
    context.onChange();
  });
  marks.append(filterButton);
  header.append(marks);
  section.append(header);

  if (view === 'collapsed') {
    const next = rows.find((dep) => dep.realtime >= context.now);
    section.append(
      el('p', 'board-walk', next === undefined ? 'collapsed, nothing in the window' : `collapsed, next in ${minutesUntil(next.realtime, context.now)} min`),
    );
    return section;
  }

  section.append(el('p', 'board-walk', describeWalk(board.stops, board, (stop) => stopTagOf(stop, board.stopLabels))));
  if (openFilter === id) section.append(renderFilter(board, context, rows));

  if (context.status.kind === 'error') {
    const box = el('p', 'board-error');
    box.append(el('span', undefined, `could not load: ${context.status.detail}`));
    const retry = button('retry', 'retry');
    retry.addEventListener('click', context.onRetry);
    box.append(retry);
    section.append(box);
    return section;
  }

  if (context.status.kind === 'loading' && board.departures.length === 0) {
    const note = context.status.backend === null ? 'loading' : `${context.status.backend} · page ${context.status.page}`;
    section.append(el('p', 'board-progress', note));
    const skeleton = el('ul', 'rows skeleton');
    for (let index = 0; index < 4; index += 1) skeleton.append(el('li', 'row row-skeleton'));
    section.append(skeleton);
    return section;
  }

  if (rows.length === 0) {
    section.append(el('p', 'empty', board.departures.length === 0 ? 'nothing in the window' : 'every line here is hidden'));
    return section;
  }

  if (context.status.kind === 'loading') {
    const note = context.status.backend === null ? 'refreshing' : `${context.status.backend} · page ${context.status.page}`;
    section.append(el('p', 'board-progress', note));
  }

  if (view === 'integrated') {
    section.append(renderStrip(rows, board, context));
    return section;
  }

  const columns = columnsOf(board, rows, context.routes);
  const usual = context.routes === undefined ? new Map<string, string>() : usualExits(context.routes);
  const ordered =
    context.sortByArrival && columns.route
      ? [...rows].sort((a, b) => arrivalOf(a, context.routes) - arrivalOf(b, context.routes) || a.realtime - b.realtime)
      : rows;
  const list = el('ul', 'rows');
  let highlighted = false;
  for (const dep of ordered) {
    const node = renderRow(dep, board, columns, context, usual);
    if (!highlighted && catchableOnBoard(dep, board, context.now)) {
      node.classList.add('first-catchable');
      highlighted = true;
    }
    list.append(node);
  }
  section.append(list);
  return section;
}

/** The walking time this board would use for a row, for the alarm popup's seed. */
export function walkFor(board: Board, dep: Departure): number {
  return walkMinutesFor(board, dep.stop);
}
