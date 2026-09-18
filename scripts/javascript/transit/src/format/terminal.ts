import { contrastText, resolveColor, rgb } from '../colors.ts';
import { catchableOnBoard, describeWalk } from '../filter.ts';
import { stopTag } from '../json.ts';
import type { PlannedRow, RouteOption } from '../plan.ts';
import type { Board, Departure, Direction, Message, StopHit } from '../model.ts';

/**
 * Where the board stops being a list of individual departures and becomes a
 * strip of times. Inside this many minutes a person reads one row at a time and
 * cares about the platform and the delay; beyond it they only want to know the
 * rhythm of the line.
 */
export const NEAR_WINDOW_MINUTES = 60;

export interface TerminalOptions {
  now: number;
  timezone: string;
  color: boolean;
}

const RESET = '\x1b[0m';

function fg(hex: string, text: string): string {
  const [r, g, b] = rgb(hex);
  return `\x1b[38;2;${r};${g};${b}m${text}${RESET}`;
}

function badge(hex: string, text: string): string {
  const [r, g, b] = rgb(hex);
  const [tr, tg, tb] = rgb(contrastText(hex));
  return `\x1b[48;2;${r};${g};${b}m\x1b[38;2;${tr};${tg};${tb}m${text}${RESET}`;
}

function dim(text: string): string {
  return `\x1b[2m${text}${RESET}`;
}

function bold(text: string): string {
  return `\x1b[1m${text}${RESET}`;
}

function strike(text: string): string {
  return `\x1b[9m${text}${RESET}`;
}

function padEnd(text: string, width: number): string {
  return text.length >= width ? text.slice(0, width) : text + ' '.repeat(width - text.length);
}

function padStart(text: string, width: number): string {
  return text.length >= width ? text : ' '.repeat(width - text.length) + text;
}

/** Wall clock time of an instant, in the configured zone. */
export function clockTime(epochMs: number, timezone: string): string {
  return new Intl.DateTimeFormat('en-GB', {
    hour: '2-digit',
    minute: '2-digit',
    hour12: false,
    timeZone: timezone,
  }).format(new Date(epochMs));
}

/** Whole minutes from now, floored, never below zero. */
export function minutesUntil(epochMs: number, now: number): number {
  return Math.max(0, Math.floor((epochMs - now) / 60_000));
}

function delayChip(dep: Departure, color: boolean): string {
  if (dep.delayMin === 0) return '';
  const text = dep.delayMin > 0 ? `+${dep.delayMin}` : `${dep.delayMin}`;
  if (!color) return text;
  return fg(dep.delayMin > 0 ? '#D14343' : '#2E9E5B', text);
}

function realtimeMark(dep: Departure, color: boolean): string {
  const mark = dep.realtimeKnown ? '●' : '○';
  if (!color) return mark;
  return dep.realtimeKnown ? fg('#2E9E5B', mark) : dim(mark);
}

function directionLabel(direction: Direction): string {
  return direction ?? '-';
}

/** Width of the badge column, so one wide label does not skew a board's alignment. */
function badgeWidth(rows: Departure[]): number {
  let width = 5;
  for (const row of rows) width = Math.max(width, row.line.length + 2);
  return width;
}

function nearRow(dep: Departure, options: TerminalOptions, board: Board, highlight: boolean, lineWidth: number): string {
  const reachable = catchableOnBoard(dep, board, options.now);
  const minutes = padStart(`${minutesUntil(dep.realtime, options.now)}'`, 5);
  const line = padEnd(` ${dep.line} `, lineWidth);
  const destination = padEnd(dep.destination, 28);
  const clock = clockTime(dep.realtime, options.timezone);
  const chip = padEnd(delayChip(dep, false), 4);
  const platform = padEnd(dep.platform === null ? '' : `Pl ${dep.platform}`, 7);

  const colour = resolveColor(dep);
  const pieces: string[] = [];
  pieces.push(options.color ? (highlight ? bold(minutes) : minutes) : minutes);
  pieces.push(options.color ? badge(colour, line) : line);
  pieces.push(options.color && dep.cancelled ? strike(destination) : destination);
  pieces.push(clock);
  pieces.push(options.color ? padEnd(delayChip(dep, true), chip.length + (dep.delayMin === 0 ? 0 : 11)) : chip);
  pieces.push(platform);
  pieces.push(realtimeMark(dep, options.color));
  if (dep.cancelled) pieces.push(options.color ? fg('#D14343', 'CANCELLED') : 'CANCELLED');
  if (dep.sev) pieces.push(options.color ? fg('#C98A00', 'SEV') : 'SEV');
  if (dep.connection !== undefined) {
    // An empty slot is as informative as a full one here: it says the board has
    // an interchange and nothing at it was catchable from this row.
    const onward =
      dep.connection === null ? '→ -' : `→ ${dep.connection.line} ${clockTime(dep.connection.departure, options.timezone)}`;
    pieces.push(options.color ? dim(onward) : onward);
  }
  if (dep.stopTag !== undefined) pieces.push(options.color ? dim(`@${dep.stopTag}`) : `@${dep.stopTag}`);

  const row = pieces.join(' ').replace(/\s+$/, '');
  if (!reachable && options.color) return dim(row);
  // "unreachable" rather than "tight", which now means something else on a
  // planned board: an onward departure a rider would have to run for. This
  // marker says the opposite thing, that the walk to the stop no longer fits.
  if (!reachable) return `${row} (unreachable)`;
  return row;
}

interface Strip {
  line: string;
  direction: Direction;
  color: string;
  /** Set only on a multi-stop board, where a strip is per stop. */
  stopTag?: string;
  entries: Departure[];
}

function stripGroups(rows: Departure[]): Strip[] {
  const groups = new Map<string, Strip>();
  for (const row of rows) {
    // A multi-stop board strips per stop: the two stops have different walking
    // times, so merging their times into one rhythm row would describe a
    // service nobody can actually take from one doorstep.
    const key = JSON.stringify([row.line, row.direction, row.stopTag ?? null]);
    let group = groups.get(key);
    if (group === undefined) {
      group = { line: row.line, direction: row.direction, color: resolveColor(row), entries: [] };
      if (row.stopTag !== undefined) group.stopTag = row.stopTag;
      groups.set(key, group);
    }
    group.entries.push(row);
  }
  return [...groups.values()].sort((a, b) => {
    const first = a.entries[0]?.realtime ?? 0;
    const second = b.entries[0]?.realtime ?? 0;
    return first - second;
  });
}

function stripRow(group: Strip, options: TerminalOptions, lineWidth: number): string {
  const label = padEnd(` ${group.line} `, lineWidth);
  const head = options.color ? badge(group.color, label) : label;
  const times = group.entries.map((dep) => {
    const text = clockTime(dep.realtime, options.timezone);
    const chip = dep.delayMin === 0 ? '' : dep.delayMin > 0 ? `+${dep.delayMin}` : `${dep.delayMin}`;
    const cell = `${text}${chip}`;
    if (dep.cancelled) return options.color ? strike(cell) : `${cell}(x)`;
    return options.color && chip.length > 0 ? `${text}${fg('#D14343', chip)}` : cell;
  });
  const tag = group.stopTag === undefined ? '' : ` ${options.color ? dim(`@${group.stopTag}`) : `@${group.stopTag}`}`;
  return `${head} ${directionLabel(group.direction)}${tag}  ${times.join(' ')}`;
}

export function renderBoard(board: Board, options: TerminalOptions): string {
  const out: string[] = [];
  const heading = `${board.title}  [${board.backend}]`;
  out.push(options.color ? bold(heading) : heading);
  const subtitle = describeWalk(board.stops, board, (stop) => stopTag(stop, board.stopLabels));
  out.push(options.color ? dim(subtitle) : subtitle);

  if (board.departures.length === 0) {
    out.push(options.color ? dim('  nothing in the window') : '  nothing in the window');
    return out.join('\n');
  }

  const boundary = options.now + NEAR_WINDOW_MINUTES * 60_000;
  const near = board.departures.filter((dep) => dep.realtime <= boundary);
  const far = board.departures.filter((dep) => dep.realtime > boundary);

  const lineWidth = badgeWidth(board.departures);
  let highlighted = false;
  for (const dep of near) {
    const reachable = catchableOnBoard(dep, board, options.now);
    const highlight = reachable && !highlighted;
    if (highlight) highlighted = true;
    out.push(`  ${nearRow(dep, options, board, highlight, lineWidth)}`);
  }

  if (far.length > 0) {
    const label = `  later, to the horizon`;
    out.push(options.color ? dim(label) : label);
    for (const group of stripGroups(far)) out.push(`  ${stripRow(group, options, lineWidth)}`);
  }

  return out.join('\n');
}

export function renderBoards(boards: Board[], options: TerminalOptions): string {
  return boards.map((board) => renderBoard(board, options)).join('\n\n');
}

export function renderStopHits(hits: StopHit[], options: TerminalOptions): string {
  if (hits.length === 0) return 'no matches';
  return hits
    .map((hit) => {
      const place = hit.place ? `, ${hit.place}` : '';
      const modes = hit.modes && hit.modes.length > 0 ? `  ${hit.modes.join(' ')}` : '';
      const id = options.color ? dim(hit.id) : hit.id;
      return `${padEnd(hit.name + place, 40)} ${id}${modes}`;
    })
    .join('\n');
}

export function renderMessages(messages: Message[], options: TerminalOptions): string {
  if (messages.length === 0) return 'no current messages';
  return messages
    .map((message) => {
      const lines = message.lines.length > 0 ? ` (${message.lines.join(' ')})` : '';
      const head = `${message.title}${lines}`;
      return `${options.color ? bold(head) : head}\n  ${message.text.replace(/\s*\n\s*/g, ' ')}`;
    })
    .join('\n\n');
}

/** One row per (line, direction) pair, with a few destinations seen on it. */
export interface DiscoveryGroup {
  line: string;
  direction: Direction;
  count: number;
  destinations: string[];
  mode: Departure['mode'];
}

export function renderDiscovery(stop: string, groups: DiscoveryGroup[], options: TerminalOptions): string {
  const head = `directions seen at ${stop}`;
  const out: string[] = [options.color ? bold(head) : head];
  if (groups.length === 0) {
    out.push('  nothing in the window');
    return out.join('\n');
  }
  let lineWidth = 5;
  for (const group of groups) lineWidth = Math.max(lineWidth, group.line.length + 2);
  for (const group of groups) {
    const colour = resolveColor({ line: group.line, mode: group.mode, color: null });
    const label = padEnd(` ${group.line} `, lineWidth);
    const head2 = options.color ? badge(colour, label) : label;
    out.push(`  ${head2} ${directionLabel(group.direction)}  x${padEnd(String(group.count), 4)} ${group.destinations.join(' | ')}`);
  }
  return out.join('\n');
}

/**
 * How many journeys are printed under one row. The planner offers more than
 * anyone reads standing at a stop, and the ones past the first few arrive later
 * than something already on screen.
 */
export const ROUTE_OPTIONS_SHOWN = 3;

/**
 * One journey in one line: where the first leg drops you, what you catch there
 * and when, and when you arrive.
 *
 * The marker carries the verdict without colour, because the commute view is
 * read on a terminal that may have none and the difference between a change
 * that works and one that needs the train to run early is the whole point:
 * `>` is the recommendation, `-` is another way of getting there, and `~` plus
 * the trailing word is a change that only comes off if something goes right.
 */
export function routeOptionLine(option: RouteOption, best: boolean, options: TerminalOptions): string {
  const marker = option.tight ? '~' : best ? '>' : '-';
  const onward = option.legs.slice(1).map((leg) => `${leg.line} ${clockTime(leg.departure, options.timezone)}`);
  const changes = option.transfers === 0 ? 'direct' : `${option.transfers} change${option.transfers === 1 ? '' : 's'}`;
  const chain = [`off ${option.exitStopName}`, ...onward, `arr ${clockTime(option.arrival, options.timezone)}`].join(' → ');
  const text = `${marker} ${chain}  (${changes}${option.tight ? ', tight' : ''})`;
  if (!options.color) return text;
  return best && !option.tight ? text : dim(text);
}

/** One board of the commute view: its rows, each with the journeys it starts. */
export interface PlannedBoardView {
  board: Board;
  rows: PlannedRow[];
}

export function renderPlannedBoard(view: PlannedBoardView, destination: string, options: TerminalOptions): string {
  const { board, rows } = view;
  const out: string[] = [];
  const heading = `${board.title} → ${destination}  [${board.backend}]`;
  out.push(options.color ? bold(heading) : heading);
  const subtitle = describeWalk(board.stops, board, (stop) => stopTag(stop, board.stopLabels));
  out.push(options.color ? dim(subtitle) : subtitle);

  if (rows.length === 0) {
    out.push(options.color ? dim('  nothing in the window') : '  nothing in the window');
    return out.join('\n');
  }

  const lineWidth = badgeWidth(rows.map((row) => row.departure));
  let highlighted = false;
  for (const row of rows) {
    const dep = row.departure;
    const reachable = catchableOnBoard(dep, board, options.now);
    // The highlight follows the board's rule, not the planner's: it marks the
    // first row a rider can still reach on foot, whether or not a journey was
    // found for it.
    const highlight = reachable && !highlighted;
    if (highlight) highlighted = true;
    out.push(`  ${nearRow(dep, options, board, highlight, lineWidth)}`);
    // A cancelled departure gets no route, however good the planner thinks it
    // is. The planner works from the timetable and does not always know the
    // vehicle has been withdrawn, and a recommendation to take a train that is
    // not running is worse than no recommendation. The row already says
    // CANCELLED, so nothing further needs saying.
    if (dep.cancelled) continue;
    if (row.options.length === 0) {
      out.push(options.color ? dim('      no route found') : '      no route found');
      continue;
    }
    for (const option of row.options.slice(0, ROUTE_OPTIONS_SHOWN)) {
      out.push(`      ${routeOptionLine(option, option === row.best, options)}`);
    }
  }

  return out.join('\n');
}

export function renderPlannedBoards(views: PlannedBoardView[], destination: string, options: TerminalOptions): string {
  return views.map((view) => renderPlannedBoard(view, destination, options)).join('\n\n');
}
