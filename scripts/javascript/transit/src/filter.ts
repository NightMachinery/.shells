import type { BoardConfig, Departure } from './model.ts';

/**
 * Line labels are written inconsistently across backends and by hand in the
 * config: one source separates the category from the number with a space and
 * another does not. Comparison folds case and removes all whitespace, so the
 * spaced and unspaced spellings of the same line are one line.
 */
export function normaliseLine(label: string): string {
  return label.replace(/\s+/g, '').toLowerCase();
}

/**
 * Apply a board's filters, always in this order: modes, then lines, then
 * direction, then destinations. The order is fixed so that a board that
 * produces nothing can be debugged by removing the filters from the bottom up.
 * A board with no filters at all shows everything the stop offers.
 *
 * The direction rule is deliberately blunt: a board that sets `direction`
 * keeps only rows carrying exactly that letter, and a row whose direction is
 * unknown is dropped along with the wrong-way rows. Keeping unknowns would let
 * a backend that supplies no letter quietly turn a one-way board into a
 * both-ways board, which is the failure that is hard to notice.
 */
export function applyFilters(deps: Departure[], board: BoardConfig): Departure[] {
  let rows = deps;

  if (board.modes !== undefined && board.modes.length > 0) {
    const wanted = new Set(board.modes);
    rows = rows.filter((row) => wanted.has(row.mode));
  }

  if (board.lines !== undefined && board.lines.length > 0) {
    const wanted = new Set(board.lines.map(normaliseLine));
    rows = rows.filter((row) => wanted.has(normaliseLine(row.line)));
  }

  if (board.direction !== undefined) {
    const wanted = board.direction;
    rows = rows.filter((row) => row.direction === wanted);
  }

  if (board.destinations !== undefined && board.destinations.length > 0) {
    const patterns = board.destinations.map((source) => new RegExp(source, 'i'));
    rows = rows.filter((row) => patterns.some((pattern) => pattern.test(row.destination)));
  }

  return rows;
}

/**
 * Merge the per-stop results of one board into a single ordered list. The sort
 * is by expected departure and is stable, so two vehicles leaving in the same
 * millisecond keep the stop order the caller supplied.
 */
export function mergeBoards(byStop: Departure[][]): Departure[] {
  const merged: Departure[] = [];
  for (const rows of byStop) merged.push(...rows);
  return merged
    .map((row, index) => ({ row, index }))
    .sort((a, b) => a.row.realtime - b.row.realtime || a.index - b.index)
    .map((entry) => entry.row);
}

/**
 * Whether a departure can still be reached on foot. A board with no walking
 * time declares everything catchable, including something leaving right now or
 * a moment ago, because the intent there is a plain timetable with no dimming.
 */
export function catchable(dep: Departure, walkMinutes: number, now: number): boolean {
  if (walkMinutes <= 0) return true;
  return dep.realtime - now >= walkMinutes * 60_000;
}

/**
 * Just enough of a board to resolve a walking time and name a stop; both `Board`
 * and `BoardConfig` fit.
 */
export interface WalkSource {
  walkMinutes: number;
  walkMinutesByStop?: Record<string, number>;
  stopLabels?: Record<string, string>;
}

/**
 * The walking time that applies to one row: the override for the row's own
 * stop when the board declares one, otherwise the board's figure. Reachability
 * is always computed from this rather than from the board, so on a board that
 * merges a near stop with a far one the near stop's departures stay catchable
 * for longer, which is the whole reason the override exists.
 */
export function walkMinutesFor(board: WalkSource, stop: string): number {
  const override = board.walkMinutesByStop?.[stop];
  return typeof override === 'number' ? override : board.walkMinutes;
}

/** `catchable`, resolving the walking time from the row's own stop. */
export function catchableOnBoard(dep: Departure, board: WalkSource, now: number): boolean {
  return catchable(dep, walkMinutesFor(board, dep.stop), now);
}

/**
 * The one-line walk summary under a board title, in the terminal and on the page.
 *
 * Three cases, because one wording cannot serve all of them. With a single stop
 * there is nothing to distinguish, so the figure stands alone. With several
 * stops that share a walk, saying it once and counting the stops is shorter than
 * repeating it. With several stops that differ, the figures are the whole point
 * and each is named, because that is what explains why a row two minutes sooner
 * is dimmed and a row two minutes later is not.
 *
 * `tagOf` is passed in rather than imported so this module stays free of the
 * JSON layer; both callers hand it the same rule.
 */
export function describeWalk(
  stops: string[],
  source: WalkSource,
  tagOf: (stop: string) => string,
): string {
  if (stops.length <= 1) {
    const only = stops[0];
    const minutes = only === undefined ? source.walkMinutes : walkMinutesFor(source, only);
    return `walk ${minutes} min`;
  }
  const minutes = stops.map((stop) => walkMinutesFor(source, stop));
  const same = minutes.every((value) => value === minutes[0]);
  if (same) return `${stops.length} stops, walk ${minutes[0]} min`;
  const parts = stops.map((stop, index) => `${tagOf(stop)} ${minutes[index]} min`);
  return `walk: ${parts.join(', ')}`;
}
