import { icon } from './icons.ts';
import { buildLabel } from './build.ts';
import { describe, describeBoards, lastRun } from './timing.ts';
import { catchableOnBoard, describeWalk, normaliseLine, walkMinutesFor } from '../filter.ts';
import type { Board, Departure } from '../model.ts';
import { handoffFor, routeUrl, type RouteHandoff } from '../route-link.ts';
import { openRouteOverlay, standalone } from './route-overlay.ts';
import type { RouteOption } from '../plan.ts';
import { button, clockTime, compact, countdownLabel, el, minutesUntil, slot, timeLabel, timeNode } from './dom.ts';
import { destinationBadges, type DestinationBadge } from './badges.ts';
import { alternativeLine, journeySummary, lineBadge, renderJourney, slotHead } from './journey.ts';
import { attachTip } from './tip.ts';
import { alarmMarker, attachLongPress, openAlarmPopup } from './notify.ts';
import { stopTagOf } from './data.ts';
import { arrivalOf, rowKey, usualExits, type BoardRoutes } from './commute.ts';
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
  routes?: BoardRoutes;
  /** True while the journeys came from the cache and nothing fresh has landed. */
  routesStale: boolean;
  /** When the journeys on screen were planned, epoch ms; null when there are none. */
  routesAt: number | null;
  /** What the destination is called, for the journey slot and its tooltip. */
  destinationName: string;
  /** The place key it is, so the expanded page can plan the same journey again. */
  destinationKey: string;
  /** True when the board itself names where its journeys end, not the picker. */
  destinationFixed?: boolean;
  /** Where this board sits in a planning run, or null when none is running. */
  planning: { position: number; done: number; total: number } | null;
  /** What a walked minute costs in ridden minutes, shown in the filter popover. */
  walkWeight: number;
  /** Called when the reader changes that; re-plans rather than re-fetches. */
  onWalkWeight: (value: number) => void;
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

/**
 * Everything about a board that lives outside its data: which view it is in,
 * which lines are hidden, whether its filter popover is open, and whether the
 * viewport is narrow enough to drop a column.
 *
 * Exported so the page can tell an unchanged board from a changed one without
 * knowing where any of that is kept, which is here and in local storage.
 */
export function boardChrome(profileKey: string, board: Board, commuting: boolean): string {
  const id = boardId(profileKey, board.title);
  return `${viewOf(profileKey, board, commuting)}|${[...readHidden(id)].sort().join(',')}|${openFilter === id ? 1 : 0}|${narrowViewport() ? 1 : 0}`;
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
  const main = timeNode(dep.realtime, referenceMs, timezone, `time-main${late ? ' late' : ''}${early ? ' early' : ''}`);
  wrap.append(main);
  if (dep.delayMin !== 0) wrap.append(el('span', 'time-planned', `(${clockTime(dep.planned, timezone)})`));
  // Hover only. On a touch screen the whole row is one tap target and opens the
  // sheet, so a tip here as well would be two answers competing for one tap.
  attachTip(wrap, () => departureTip(dep, timezone, referenceMs), {
    label: departureLabel(dep, timezone, referenceMs),
    tapOpens: false,
  });
  return wrap;
}

/** What a time says in plain text: the timetable, the delay, where it is going. */
function departureLabel(dep: Departure, timezone: string, referenceMs: number): string {
  const parts = [dep.destination, `planned ${timeLabel(dep.planned, referenceMs, timezone)}`];
  parts.push(dep.delayMin === 0 ? 'on time' : `${dep.delayMin > 0 ? '+' : ''}${dep.delayMin} min`);
  if (dep.platform !== null) parts.push(`platform ${dep.platform}`);
  if (dep.cancelled) parts.push('cancelled');
  return parts.join(' · ');
}

/** The same, as the tooltip everything on this page uses. */
function departureTip(dep: Departure, timezone: string, referenceMs: number): HTMLElement {
  const body = el('div', 'tip-body');
  const head = el('div', 'tip-head');
  head.append(lineBadge(dep));
  head.append(el('span', 'tip-title', dep.destination));
  body.append(head);

  const rows = el('dl', 'tip-rows');
  const row = (term: string, value: Node | string): void => {
    rows.append(el('dt', undefined, term));
    const dd = el('dd');
    if (typeof value === 'string') dd.textContent = value;
    else dd.append(value);
    rows.append(dd);
  };
  row('planned', timeNode(dep.planned, referenceMs, timezone));
  row('expected', timeNode(dep.realtime, referenceMs, timezone));
  row('delay', dep.delayMin === 0 ? 'on time' : `${dep.delayMin > 0 ? '+' : ''}${dep.delayMin} min`);
  if (dep.platform !== null) row('platform', dep.platform);
  body.append(rows);
  if (dep.cancelled) body.append(el('p', 'tip-warning', 'This departure is cancelled.'));
  if (dep.sev) body.append(el('p', 'tip-note', 'A replacement service, not the usual vehicle.'));
  return body;
}

/** Which optional columns this board's rows have anything to put in. */
interface Columns {
  connection: boolean;
  route: boolean;
  /**
   * Whether the row's second line has to earn its place.
   *
   * On a phone it does: a line of vertical space per row is the difference
   * between five departures on a screen and eight, and a clock time that merely
   * restates the countdown is the cheapest thing on the row to drop. On a wider
   * screen it does not, because the space is there and an always-drawn line
   * keeps every row the same height.
   */
  terseTimes: boolean;
  stop: boolean;
}

/**
 * Whether the screen is too narrow to carry every optional slot.
 *
 * The breakpoint is the same one the stylesheet uses for its narrow layout, in
 * the one place a render can ask about it. A phone does not change width except
 * when it is turned over, which `render` is told about below.
 */
export function narrowViewport(): boolean {
  return typeof window !== 'undefined' && typeof window.matchMedia === 'function'
    ? window.matchMedia('(max-width: 480px)').matches
    : false;
}

function columnsOf(board: Board, routes: BoardRoutes | undefined): Columns {
  const route = routes !== undefined && routes.rows.size > 0;
  return {
    // On a phone a board with both an interchange and a journey has more fixed
    // width than the screen, and both end up at fifty pixels, which is not a
    // width anything is legible at. The journey wins, because it answers the
    // same question with more in it: where to get off, what to catch, and when
    // you arrive, chosen per row rather than configured once. On a wider screen
    // both fit and both are shown, because the configured interchange is a
    // question somebody asked for by name.
    connection: board.connection !== undefined && !(route && narrowViewport()),
    route,
    terseTimes: narrowViewport(),
    stop: board.stops.length > 1,
  };
}

function gridTemplate(columns: Columns): string {
  const parts = ['var(--col-minutes)', 'var(--col-badge)', 'minmax(var(--col-destination-min), 1fr)'];
  // The journey keeps a floor the destination does not. When the row runs out of
  // room something has to give, and the destination is the cheaper thing to
  // squeeze: it is already abbreviated, the same word is on every row of the
  // group, and the tooltip repeats it. A journey squeezed below its floor stops
  // naming a station at all, which is the whole reason the column exists.
  if (columns.route) parts.push('minmax(var(--col-route-min), var(--col-route))');
  if (columns.connection) parts.push('minmax(0, var(--col-connection))');
  // Allowed to reach zero, unlike the alarm column beside it. A grid whose fixed
  // tracks add up to more than the row is wide does not wrap or ellipsise: it
  // runs the last column off the side of the card, where the reader sees nothing
  // at all and the page reports no overflow because the card clips it. The stop
  // tag is the one that can vanish without the row losing its meaning, and it is
  // repeated in the sheet.
  // The alarm keeps a floor and may grow for its bell: a track sized only to
  // its content collapses on the rows that have no reminder, and then the
  // columns stop lining up down the board.
  // No track for the platform and none for the state. They are one badge in the
  // row's corner rather than two cells, so they cost the grid nothing at all;
  // see `stateBadge`.
  parts.push('minmax(var(--col-alarm), max-content)');
  if (columns.stop) parts.push('minmax(0, var(--col-stop))');
  return parts.join(' ');
}

/**
 * A link to the expanded view of one journey.
 *
 * In a browser tab it is exactly what it looks like: an anchor to a second
 * document, opened in a new tab so the board stays where it was, with every
 * affordance a link has (long press to copy, middle click, "open in") working
 * because nothing here reimplemented them.
 *
 * Installed to a home screen there are no tabs. A new-tab link then either does
 * nothing or throws the reader out into the browser, and either way the board
 * is gone. So the href stays, because it is still the address this journey has
 * and still the thing a share carries, and the tap is intercepted to draw the
 * same view over the board instead.
 */
function routeAnchor(handoff: RouteHandoff, className: string, text?: string): HTMLAnchorElement {
  const node = document.createElement('a');
  node.className = className;
  if (text !== undefined) node.textContent = text;
  node.href = routeUrl(handoff);
  if (standalone()) {
    node.addEventListener('click', (event) => {
      event.preventDefault();
      event.stopPropagation();
      openRouteOverlay(handoff);
    });
  } else {
    node.target = '_blank';
    node.rel = 'noopener';
  }
  return node;
}

/**
 * The commute slot: where this departure puts you down, what you catch there,
 * and when you arrive. A tight option is one that only works if the first leg
 * runs early, so it is never the recommendation and says so when asked.
 */
function renderRoute(dep: Departure, board: Board, context: BoardContext, usual: Map<string, string>): HTMLElement {
  // A cancelled departure gets no route, however good the planner thinks it is.
  // The planner works from the timetable and does not always know the vehicle
  // has been withdrawn, and a recommendation to take a train that is not running
  // is worse than no recommendation.
  if (dep.cancelled) return slot('route');
  const planned = context.routes?.rows.get(rowKey(dep));
  const option = planned?.best ?? planned?.options[0];
  if (option === undefined) {
    // A row whose plan is still being worked on says so rather than looking like
    // a row with no journey. The two are different answers and a reader acts on
    // them differently.
    if (context.planning === null) return slot('route');
    const waiting = el('span', 'route route-planning');
    waiting.append(el('span', 'spinner'));
    waiting.setAttribute('aria-label', 'planning this journey');
    return waiting;
  }
  const options = planned?.options ?? [option];

  const better = usual.get(normaliseLine(dep.line)) !== undefined && usual.get(normaliseLine(dep.line)) !== option.exitStop;
  const stale = context.routesStale ? ' route-stale' : '';
  // A link, not a span: the expanded view is a page with its own address, so it
  // opens in a new tab, it can be shared, and the browser's own affordances for
  // "this goes somewhere" all work without being reimplemented.
  const node = routeAnchor(
    handoffFor({
      board: board.title,
      destination: context.destinationName,
      destinationKey: context.destinationKey,
      timezone: context.timezone,
      plannedAt: context.routesAt ?? context.now,
      departure: dep,
      from: board.title,
      options,
    }),
    `route${option.tight ? ' tight' : ''}${better && !option.tight ? ' better' : ''}${stale}`,
  );

  node.append(slotHead(option));
  node.append(timeNode(option.arrival, context.now, context.timezone, 'route-arrival'));

  const notes = { better, origin: context.routes?.origin };
  // On a desktop this is the anchored popover on hover, and a click follows the
  // link. On a phone the row above it opens the sheet, which carries this same
  // journey and the links out of it: one tap target per row, because a row two
  // centimetres tall divided into six of them is six ways to miss.
  attachTip(node, () => routeTip(dep, board, context, options, notes), {
    label: journeySummary(option, context.timezone, context.now),
    tapOpens: false,
  });
  return node;
}

/**
 * The tooltip for a journey slot: the recommendation in full, then the other
 * ways of making the same trip, each of which expands where it stands.
 *
 * Three rather than all of them, because past the third every option is worse
 * on every measure than something already listed, and the tooltip has to stay
 * readable on a phone held in one hand.
 */
const TIP_OPTIONS_SHOWN = 3;

function routeTip(
  dep: Departure,
  board: Board,
  context: BoardContext,
  options: readonly RouteOption[],
  notes: { better: boolean; origin: BoardRoutes['origin'] | undefined },
): HTMLElement {
  const body = el('div', 'tip-body tip-journey');
  const shown = options.slice(0, TIP_OPTIONS_SHOWN);
  const best = shown[0] as RouteOption;

  const head = el('div', 'tip-head');
  head.append(lineBadge(dep));
  head.append(timeNode(dep.realtime, context.now, context.timezone, 'tip-departure'));
  head.append(el('span', 'tip-title', `${board.title} → ${context.destinationName}`));
  body.append(head);

  body.append(renderJourney(best, context.timezone, context.now, notes));

  if (shown.length > 1) {
    body.append(el('h4', 'tip-alternatives-title', 'Alternatives'));
    const list = el('ul', 'tip-alternatives');
    shown.slice(1).forEach((option, offset) => {
      const item = el('li', `tip-alternative${option.tight ? ' tight' : ''}`);
      const toggle = button('tip-alternative-line', alternativeLine(option, context.timezone));
      const detail = el('div', 'tip-alternative-detail');
      detail.hidden = true;
      detail.append(renderJourney(option, context.timezone, context.now, { origin: notes.origin }));
      toggle.addEventListener('click', (event) => {
        event.stopPropagation();
        detail.hidden = !detail.hidden;
      });
      item.append(toggle);
      const open = routeAnchor(
        handoffFor({
          board: board.title,
          destination: context.destinationName,
          destinationKey: context.destinationKey,
          timezone: context.timezone,
          plannedAt: context.routesAt ?? context.now,
          departure: dep,
          from: board.title,
          options: [...options],
          first: offset + 1,
        }),
        'tip-alternative-open',
        'open',
      );
      item.append(open);
      item.append(detail);
      list.append(item);
    });
    body.append(list);
  }

  if (context.routesStale && context.routesAt !== null) {
    const age = Math.max(0, Math.round((Date.now() - context.routesAt) / 1000));
    body.append(el('p', 'tip-note', `from the last visit, ${age} s old; planning again now`));
  }

  // The label says where it goes, and where it goes depends on whether there is
  // anywhere to go. "a new tab" is a promise the installed app cannot keep.
  const open = routeAnchor(
    handoffFor({
      board: board.title,
      destination: context.destinationName,
      destinationKey: context.destinationKey,
      timezone: context.timezone,
      plannedAt: context.routesAt ?? context.now,
      departure: dep,
      from: board.title,
      options: [...options],
    }),
    'tip-open',
    standalone() ? 'Open full view' : 'Open in a new tab',
  );
  body.append(open);
  return body;
}

/**
 * What a row's sheet is about, stable across re-renders.
 *
 * The board is rebuilt from scratch several times a minute, so the element a
 * sheet was opened from does not survive its own explanation. The key is how the
 * replacement claims it: same departure, same sheet, re-pointed rather than
 * closed.
 */
function rowTipKey(dep: Departure, context: BoardContext): string {
  return `row|${context.profileKey}|${rowKey(dep)}|${dep.direction}`;
}

/**
 * What the sheet currently says, cheaply.
 *
 * Compared on every render. Equal leaves the open sheet alone, which is the
 * point: a reader halfway down a leg list is not interrupted by a redraw that
 * would have produced the same list. Different rebuilds the body in place,
 * because then the journey really has changed under them.
 */
function rowTipSignature(dep: Departure, planned: BoardRoutes['rows'] extends Map<string, infer V> ? V | undefined : never, planning: boolean): string {
  const head = `${dep.realtime}|${dep.delayMin}|${dep.cancelled}|${dep.platform ?? ''}|${dep.realtimeKnown}`;
  if (planning && planned === undefined) return `${head}|planning`;
  const options = planned?.options ?? [];
  return `${head}|${options.map((option) => `${option.exitStop}:${option.arrival}:${option.transfers}:${option.tight}`).join(',')}`;
}

/**
 * The whole of one departure, as the sheet a tap on its row opens.
 *
 * One sheet rather than one per cell. A row is a line badge, a destination, two
 * times, a journey, a platform and a state mark inside about two centimetres of
 * height, and asking a finger to choose between them is asking it to miss: the
 * destination answered a tap by starting a text selection, and the line badge
 * answered by doing nothing at all. Everything a row knows is in here, in the
 * order it is asked about: what it is and when it goes, then how to ride it to
 * where you are going.
 */
function rowSheet(dep: Departure, board: Board, context: BoardContext, usual: Map<string, string>): HTMLElement {
  const body = el('div', 'tip-body tip-sheet-body');

  const head = el('div', 'tip-head');
  head.append(lineBadge(dep));
  head.append(el('span', 'tip-title', dep.destination));
  body.append(head);

  const rows = el('dl', 'tip-rows');
  const row = (term: string, value: Node | string): void => {
    rows.append(el('dt', undefined, term));
    const dd = el('dd');
    if (typeof value === 'string') dd.textContent = value;
    else dd.append(value);
    rows.append(dd);
  };
  row('planned', timeNode(dep.planned, context.now, context.timezone));
  row('expected', timeNode(dep.realtime, context.now, context.timezone));
  row('delay', dep.delayMin === 0 ? 'on time' : `${dep.delayMin > 0 ? '+' : ''}${dep.delayMin} min`);
  if (dep.platform !== null) row('platform', dep.platform);
  row('stop', dep.stopTag ?? stopTagOf(dep.stop, board.stopLabels));
  row('times from', dep.realtimeKnown && !context.planned ? `${dep.backend}, live` : `${dep.backend}, timetable`);
  body.append(rows);

  if (dep.cancelled) body.append(el('p', 'tip-warning', 'This departure is cancelled.'));
  if (dep.sev) body.append(el('p', 'tip-note', 'A replacement service, not the usual vehicle.'));

  const planned = context.routes?.rows.get(rowKey(dep));
  const options = planned?.options ?? [];
  if (options.length > 0) {
    const best = planned?.best ?? options[0];
    const better =
      best !== undefined && usual.get(normaliseLine(dep.line)) !== undefined && usual.get(normaliseLine(dep.line)) !== best.exitStop;
    body.append(el('h4', 'tip-section', `To ${context.destinationName}`));
    body.append(routeTip(dep, board, context, options, { better, origin: context.routes?.origin }));
  } else if (context.planning !== null) {
    // The sheet opens before the plan exists rather than refusing to open. It
    // fills itself in when the plan lands, because the sheet is bound to the
    // departure and not to the element it was opened from.
    const wait = el('p', 'tip-planning');
    wait.append(el('span', 'spinner'));
    wait.append(el('span', undefined, `planning the journey to ${context.destinationName}\u2026`));
    body.append(wait);
  } else if (!dep.cancelled && context.routes !== undefined) {
    body.append(el('p', 'tip-note', `No journey to ${context.destinationName} from this departure.`));
  }

  return body;
}

/**
 * The platform, and whether the time is live, as one mark in the row's corner.
 *
 * Two facts that were two slots and are now one badge, because they were
 * competing for the same scarce thing. The platform was a column two characters
 * wide that cost the destination a quarter of its width; the live mark was a
 * column too, and it overflowed on the reader's phone in every form it took,
 * because a grid track is a promise about width that cannot be kept in a font
 * this machine does not have.
 *
 * So neither is a track. The badge is positioned in the corner, out of the flow,
 * where it costs the row nothing at all, and the colour it is painted carries
 * the state the word used to: green when the operator is reporting this trip,
 * grey when the time comes off the timetable, red and struck when the departure
 * is cancelled. A row with no platform gets the same badge with nothing in it,
 * so the colour still reads and the corner still looks like one thing rather
 * than like something missing. The words for all of it are in the sheet.
 */
function stateBadge(dep: Departure, context: BoardContext): HTMLElement {
  const known = dep.realtimeKnown && !context.planned;
  const state = dep.cancelled ? 'state-cancelled' : known ? 'state-live' : 'state-plan';
  const node = el('span', `platform ${state}${dep.platform === null ? ' platform-empty' : ''}`);
  if (dep.platform !== null) node.textContent = dep.platform;
  const where = dep.platform === null ? '' : `platform ${dep.platform}, `;
  node.title = dep.cancelled
    ? `${where}cancelled`
    : known
      ? `${where}a live time reported by the operator`
      : context.planned
        ? `${where}a timetable for the moment you picked, not a live time`
        : dep.realtime - context.now <= REPORTING_HORIZON_MS
          ? `${where}the operator has not started reporting this trip yet`
          : `${where}timetable: too far ahead for the operator to be reporting it yet`;
  return node;
}

function renderRow(dep: Departure, board: Board, columns: Columns, context: BoardContext, usual: Map<string, string>): HTMLElement {
  const reachable = catchableOnBoard(dep, board, context.now);
  const row = el('li', `row${reachable ? '' : ' unreachable'}${dep.cancelled ? ' row-cancelled' : ''}`);
  row.style.gridTemplateColumns = gridTemplate(columns);

  const minutes = el('span', 'minutes', countdownLabel(dep.realtime, context.now));
  // Registered rather than re-rendered: the tick patches this one text node, so
  // a selection elsewhere in the board survives and nothing else reflows.
  context.ticks.push(() => {
    minutes.textContent = countdownLabel(dep.realtime, Date.now());
  });
  row.append(minutes);
  row.append(lineBadge(dep));

  const main = el('span', 'row-main');
  const destination = el('span', 'destination', dep.destination);
  destination.title = dep.destination;
  main.append(destination);
  const meta = el('span', 'row-times');
  // A clock time that is exactly the countdown plus now says nothing the
  // countdown has not already said, and on a phone it costs a whole second line
  // to say it. So the second line is earned rather than assumed: a delay earns
  // it, because "17:04 (16:58)" is a promise and a fact and the countdown is
  // only the fact; a platform earns it; a replacement service or a cancellation
  // earns it. An on-time departure with none of those is one line, and its
  // clock time is on the sheet with everything else. On a wider screen the line
  // is free, so it is always drawn there and the rows stay aligned.
  const notable = dep.delayMin !== 0 || dep.cancelled || dep.sev;
  if (!columns.terseTimes || notable) meta.append(timeGroup(dep, context.timezone, context.now));
  if (dep.sev) meta.append(el('span', 'flag sev', 'SEV'));
  if (dep.cancelled) meta.append(el('span', 'flag cancelled-flag', 'cancelled'));
  if (meta.childNodes.length > 0) main.append(meta);
  row.append(main);

  if (columns.route) row.append(renderRoute(dep, board, context, usual));

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

  row.append(alarmMarker(dep) ?? slot('alarm'));

  if (columns.stop) {
    const tag = dep.stopTag ?? stopTagOf(dep.stop, board.stopLabels);
    const node = el('span', 'stop-tag', tag);
    node.title = `from ${tag}`;
    row.append(node);
  }

  // Last, and out of the flow: it is placed by the stylesheet in the row's
  // corner rather than by the grid, so where it sits among the children is only
  // a question of what a screen reader says last.
  row.append(stateBadge(dep, context));

  attachLongPress(row, () => openAlarmPopup(dep, board, row));
  // The whole row is the tap target, and only on touch: on a desktop a tip that
  // followed the pointer down two hundred rows would be a cursor trail, and the
  // cells there have their own hover explanations anyway.
  attachTip(row, () => rowSheet(dep, board, context, usual), {
    key: rowTipKey(dep, context),
    signature: rowTipSignature(dep, context.routes?.rows.get(rowKey(dep)), context.planning !== null),
    hoverOpens: false,
  });
  return row;
}

/** How far ahead a departure still counts as one the operator ought to be reporting. */
const REPORTING_HORIZON_MS = 60 * 60_000;

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

function renderStrip(rows: Departure[], board: Board, context: BoardContext, usual: Map<string, string>): HTMLElement {
  const list = el('ul', 'strips');
  const multiStop = board.stops.length > 1;
  // Computed over the whole board rather than per group, so a destination that
  // two lines both serve gets one badge and one colour everywhere on the board.
  const badges = destinationBadges(rows.map((row) => row.destination));
  for (const group of stripGroups(rows)) {
    const item = el('li', 'strip');
    const main = el('span', 'strip-main');
    main.append(lineBadge(group.head));
    const label = stripLabel(group);
    const direction = el('span', 'direction', label.text);
    direction.title = label.title;
    main.append(direction);

    // A platform is a property of the group when every run uses the same one,
    // which is the usual case, and only becomes per-time when it is not.
    const platforms = new Set(group.entries.map((entry) => entry.platform ?? ''));
    const uniform = platforms.size === 1 ? [...platforms][0] : undefined;
    if (uniform !== undefined && uniform !== '') {
      const node = el('span', 'platform', uniform);
      node.title = `platform ${uniform}`;
      main.append(node);
    }
    if (multiStop) {
      const tag = group.head.stopTag ?? stopTagOf(group.head.stop, board.stopLabels);
      const node = el('span', 'stop-tag', tag);
      node.title = `from ${tag}`;
      main.append(node);
    }

    // A run that terminates short of the others is a different journey, and a
    // strip that hides that sends people onto a train that stops before their
    // stop. When the whole group agrees, the header already says where it goes
    // and nothing is repeated; when it does not, every time carries a two or
    // three letter badge and the legend below says what each one stands for.
    const destinations = [...new Set(group.entries.map((entry) => entry.destination))];
    const branching = destinations.length > 1;
    const times = el('span', 'times-strip');
    for (const dep of group.entries) {
      const late = dep.delayMin > 0;
      const early = dep.delayMin < 0;
      const cell = el('span', `time${dep.cancelled ? ' cancelled' : ''}${late ? ' late' : ''}${early ? ' early' : ''}`);
      cell.append(timeNode(dep.realtime, context.now, context.timezone, 'time-clock'));
      if (branching) {
        const badge = badges.get(dep.destination);
        if (badge !== undefined) cell.append(destinationChip(badge, dep.destination));
      }
      // A platform only appears per time when the group's platforms disagree.
      // When they agree it is on the row, once, which is where a reader looks.
      if (uniform === undefined && dep.platform !== null) cell.append(el('span', 'time-note', `pl ${dep.platform}`));
      if (dep.cancelled) {
        const struck = el('span', 'time-note');
        struck.append(icon('close'));
        cell.append(struck);
      }
      // The same sheet a row opens, from the same departure. In the strip a time
      // is the only thing there is to tap, so it is the tap target; the group
      // header is a heading and opens nothing.
      attachTip(cell, () => rowSheet(dep, board, context, usual), {
        label: departureLabel(dep, context.timezone, context.now),
        key: rowTipKey(dep, context),
        signature: rowTipSignature(dep, context.routes?.rows.get(rowKey(dep)), context.planning !== null),
      });
      times.append(cell);
    }
    main.append(times);
    item.append(main);

    if (branching) {
      const legend = el('span', 'strip-legend');
      for (const name of destinations) {
        const badge = badges.get(name);
        if (badge === undefined) continue;
        const entry = el('span', 'strip-legend-entry');
        entry.append(destinationChip(badge, name));
        entry.append(el('span', 'strip-legend-name', compact(name)));
        legend.append(entry);
      }
      item.append(legend);
    }
    list.append(item);
  }
  return list;
}

/** One destination badge: procedural colour, full name on the tooltip. */
function destinationChip(badge: DestinationBadge, name: string): HTMLElement {
  const node = el('span', 'dest-badge', badge.text);
  // Only the hue is set here. Saturation, lightness and the text colour belong
  // to the stylesheet, which is the only place that knows about the colour
  // scheme, and the line's own colour is never reused: the line badge beside it
  // already carries that, and two things in one colour saying different things
  // is worse than no colour at all.
  node.style.setProperty('--dest-hue', String(badge.hue));
  node.title = name;
  return node;
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
    popover.append(el('p', 'filter-build', buildLabel()));
    appendTiming(popover);
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
  // Which build this is, somewhere a reader can find it without a console. An
  // installed app can be several deploys behind while the server is current, so
  // "which version am I looking at" has to be answerable from the screen.
  popover.append(el('p', 'filter-build', buildLabel()));
  appendTiming(popover);
  return popover;
}

/**
 * How long the last refresh took, on this device.
 *
 * Here rather than in a console because the device the question is about is a
 * phone, and a phone has no console. The numbers measured on a laptop answer a
 * different question than the one a reader is asking when they say it feels
 * slow.
 */
function appendTiming(popover: HTMLElement): void {
  const run = lastRun();
  if (run === null) return;
  popover.append(el('p', 'filter-timing', describe(run)));
  for (const line of describeBoards(run)) popover.append(el('p', 'filter-timing filter-timing-board', line));
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

  // What a walked minute is worth. It sits next to the tight window because the
  // two are the same kind of question, "how do I actually want to travel", and
  // like it, it is not saved: it is asked about one board on one evening.
  const weight = document.createElement('label');
  weight.className = 'filter-buffer';
  weight.append(el('span', undefined, 'walking costs'));
  const factor = document.createElement('input');
  factor.type = 'number';
  factor.min = '1';
  factor.max = '5';
  factor.step = '0.5';
  factor.value = String(context.walkWeight);
  factor.title = 'what one minute on foot is worth in minutes on a vehicle. 1 ranks by arrival alone.';
  factor.addEventListener('change', () => {
    const value = Number(factor.value);
    if (!Number.isFinite(value) || value < 1) return;
    context.onWalkWeight(Math.min(5, value));
  });
  weight.append(factor);
  weight.append(el('span', 'filter-buffer-unit', '×'));
  popover.append(weight);
}

/** Which board's filter popover is open, if any. Null when none is. */
let openFilter: string | null = null;

/**
 * Boards that have had departures in them at some point this session.
 *
 * Skeleton rows are a promise that something is coming, and they are the right
 * answer exactly once: the first time a board is drawn with nothing in it. After
 * that the board has real rows, and replacing them with grey bars during every
 * refresh is taking information away to show that work is happening, which the
 * progress line already says without moving anything.
 */
const seenContent = new Set<string>();

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
  const filterButton = button('filter-button', undefined, 'filter the lines on this board');
  filterButton.append(icon('gear'));
  filterButton.setAttribute('aria-label', 'filter the lines on this board');
  filterButton.addEventListener('click', (event) => {
    event.stopPropagation();
    openFilter = openFilter === id ? null : id;
    context.onChange();
  });
  marks.append(filterButton);
  header.append(marks);
  section.append(header);

  // A board that fixes its own destination says so under its title. Without it
  // a hall full of boards all called "platform 11 to 14" would be four boards
  // with no visible difference and four different answers.
  if (context.destinationFixed === true && context.destinationName !== '') {
    section.append(el('p', 'board-destination', `to ${context.destinationName}`));
  }

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

  if (board.departures.length > 0) seenContent.add(id);

  if (context.status.kind === 'loading' && !seenContent.has(id)) {
    const note = context.status.backend === null ? 'loading' : `${context.status.backend} · page ${context.status.page}`;
    section.append(el('p', 'board-progress', note));
    const skeleton = el('ul', 'rows skeleton');
    for (let index = 0; index < 4; index += 1) skeleton.append(el('li', 'row row-skeleton'));
    section.append(skeleton);
    return section;
  }

  if (rows.length === 0) {
    if (board.departures.length > 0) {
      // A filter fact, not a fetch fact: true while a fetch is in flight and
      // true afterwards, and the reader is the one who made it true.
      section.append(el('p', 'empty', 'every line here is hidden'));
      return section;
    }
    // "Nothing in the window" is a claim about a completed answer. Saying it
    // while the answer is still on its way is how a board that is merely slow
    // comes to look like a board that is over for the night.
    if (context.status.kind === 'loading') {
      const note = context.status.backend === null ? 'refreshing' : `${context.status.backend} · page ${context.status.page}`;
      section.append(el('p', 'board-progress', note));
      return section;
    }
    section.append(el('p', 'empty', 'nothing in the window'));
    return section;
  }

  if (context.status.kind === 'loading') {
    const note = context.status.backend === null ? 'refreshing' : `${context.status.backend} · page ${context.status.page}`;
    section.append(el('p', 'board-progress', note));
  }

  // The journeys arrive after the departures and take longer, so a board says
  // so rather than leaving its journey slots empty and unexplained.
  if (context.planning !== null) {
    section.append(
      el('p', 'board-progress board-planning', `planning · Transitous · ${context.planning.position}/${context.planning.total}`),
    );
  } else if (context.routes !== undefined && context.routesStale && context.routesAt !== null) {
    const age = Math.max(0, Math.round((Date.now() - context.routesAt) / 1000));
    const note = el('p', 'board-progress board-stale', `journeys from the last visit, ${age}s old`);
    note.title = 'these arrival times were planned before this page was opened, and are being planned again now';
    section.append(note);
  }

  if (view === 'integrated') {
    section.append(renderStrip(rows, board, context, usualExits(context.routes?.rows ?? new Map())));
    return section;
  }

  const columns = columnsOf(board, context.routes);
  const usual = context.routes === undefined ? new Map<string, string>() : usualExits(context.routes.rows);
  const ordered =
    context.sortByArrival && columns.route
      ? [...rows].sort((a, b) => arrivalOf(a, context.routes?.rows) - arrivalOf(b, context.routes?.rows) || a.realtime - b.realtime)
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
