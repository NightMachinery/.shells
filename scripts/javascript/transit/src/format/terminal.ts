import { contrastText, resolveColor, rgb } from '../colors.ts';
import { catchableOnBoard } from '../filter.ts';
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
  if (dep.stopTag !== undefined) pieces.push(options.color ? dim(`@${dep.stopTag}`) : `@${dep.stopTag}`);

  const row = pieces.join(' ').replace(/\s+$/, '');
  if (!reachable && options.color) return dim(row);
  if (!reachable) return `${row} (tight)`;
  return row;
}

interface Strip {
  line: string;
  direction: Direction;
  color: string;
  entries: Departure[];
}

function stripGroups(rows: Departure[]): Strip[] {
  const groups = new Map<string, Strip>();
  for (const row of rows) {
    const key = JSON.stringify([row.line, row.direction]);
    let group = groups.get(key);
    if (group === undefined) {
      group = { line: row.line, direction: row.direction, color: resolveColor(row), entries: [] };
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
  return `${head} ${directionLabel(group.direction)}  ${times.join(' ')}`;
}

export function renderBoard(board: Board, options: TerminalOptions): string {
  const out: string[] = [];
  const heading = `${board.title}  [${board.backend}]`;
  out.push(options.color ? bold(heading) : heading);
  const overrides = Object.keys(board.walkMinutesByStop ?? {}).length;
  const walk = overrides > 0 ? `walk ${board.walkMinutes} min (${overrides} per-stop)` : `walk ${board.walkMinutes} min`;
  const subtitle = `${board.stops.length} stop${board.stops.length === 1 ? '' : 's'}, ${walk}`;
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
