// Keeping only the departures that actually go past a place.
//
// A direction letter is what a board had to work with, and at a large station
// it does not mean what it looks like it means. The letter belongs to a line,
// not to a compass: it is the line's primary direction as its operator defines
// it, so at one western station the same letter marked the rapid-transit
// services heading into the city and the regional services heading away from
// it, because the city is where one set of lines starts and where the other set
// ends. A board asking for "towards the main station" kept both.
//
// A place is not a letter. This asks the only question the reader is actually
// asking, which is whether the vehicle calls at the place they want, and it
// asks it of the vehicle's own run.

import type { Departure } from './model.ts';
import { normaliseLine } from './filter.ts';
import { callsAtAfter, stationOf, tripCalls, type TripOptions } from './trip.ts';

/** A line and the minute it is timetabled for, folded into one key. */
function key(line: string, epochMs: number): string {
  return `${normaliseLine(line)}|${Math.floor(epochMs / 60_000)}`;
}

/**
 * Trip identifiers for rows that came from a backend that publishes none,
 * taken from the aggregator's own view of the same stop and window.
 *
 * The same key the journey planner matches on, for the same reason: the
 * timetabled minute is the one field two feeds of one timetable must agree
 * about, and the expected minute is the one they disagree about whenever
 * anything runs late.
 */
export function tripIndex(rows: readonly Departure[]): Map<string, string> {
  const index = new Map<string, string>();
  for (const row of rows) {
    if (row.tripId === undefined) continue;
    const scheduled = key(row.line, row.planned);
    if (!index.has(scheduled)) index.set(scheduled, row.tripId);
    const expected = key(row.line, row.realtime);
    if (!index.has(expected)) index.set(expected, row.tripId);
  }
  return index;
}

export interface ViaOptions extends TripOptions {
  /**
   * The stations a kept row's vehicle must still call at, any one of which will
   * do. Several because one station can be several identifiers; see `trip.ts`.
   */
  via: readonly string[];
  /**
   * The aggregator's rows for the same stop and window, for matching rows that
   * carry no trip identifier of their own. A board whose rows all carry one
   * never needs this, and it is a function so that it is never called then.
   */
  aggregatorRows?: () => Promise<Departure[]>;
}

/**
 * Keep the rows whose vehicle still calls at the configured place.
 *
 * A row that cannot be checked is kept and marked, never dropped. The letter
 * that used to do this job is still doing it for those rows, and saying so is
 * the honest report: a board that silently hides what it could not verify
 * teaches a reader to distrust the whole board, and one that silently keeps it
 * teaches them to distrust the filter. The mark is the only version of this
 * that stays truthful when the network is down.
 */
export async function applyVia(rows: Departure[], options: ViaOptions): Promise<Departure[]> {
  if (rows.length === 0) return rows;
  const stations = options.via.map(stationOf);
  if (stations.length === 0) return rows;

  let index: Map<string, string> | null = null;
  if (rows.some((row) => row.tripId === undefined) && options.aggregatorRows !== undefined) {
    try {
      index = tripIndex(await options.aggregatorRows());
    } catch {
      // No aggregator answer means no trip identifiers for the rows that lack
      // one, which means those rows are unverified. It is not a reason to empty
      // the board.
      index = null;
    }
  }

  const verdicts = await Promise.all(
    rows.map(async (row): Promise<Departure | null> => {
      const tripId = row.tripId ?? index?.get(key(row.line, row.planned)) ?? index?.get(key(row.line, row.realtime));
      if (tripId === undefined) return { ...row, viaUnverified: true };
      let calls;
      try {
        calls = await tripCalls(tripId, options);
      } catch {
        return { ...row, viaUnverified: true };
      }
      if (calls.length === 0) return { ...row, viaUnverified: true };
      if (!callsAtAfter(calls, stations, row.planned)) return null;
      const kept = { ...row, tripId };
      delete kept.viaUnverified;
      return kept;
    }),
  );

  return verdicts.filter((row): row is Departure => row !== null);
}
