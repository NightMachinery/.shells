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
import { sameDestinationLabel, samePlatformLabel } from './label.ts';
import { callsAtAfter, departureFrom, stationOf, tripCalls, type TripOptions } from './trip.ts';

/**
 * How far apart two feeds may time the same run and still be recognised as one
 * run, in minutes, when nothing but the timetabled minute disagrees.
 *
 * Wide enough to cover a feed that is running a different timetable version,
 * which is how the disagreement actually shows up: not a minute or two of
 * rounding but a whole revision of the working timetable, where one feed has a
 * train leaving several minutes before the other has it leaving. Narrow enough
 * that a following service on the same line and platform is outside it, and the
 * match is refused anyway unless there is exactly one candidate inside it.
 */
export const NEAR_MINUTES = 10;

function minuteOf(epochMs: number): number {
  return Math.floor(epochMs / 60_000);
}

/**
 * The aggregator's identifiers for the run a board row describes.
 *
 * The same run seen twice, which is the whole problem: a row from the primary
 * backend and a row from the aggregator are two feeds describing one train, and
 * nothing in either of them is a shared identifier. Four fields are available to
 * recognise it by, and each of them is wrong sometimes, so this asks them in
 * order of how often they are right.
 *
 * The timetabled minute is the field two feeds of one timetable usually agree
 * about, and the expected minute is the one they disagree about whenever
 * anything runs late, so a shared minute plus agreement on either the line or
 * the destination identifies the run. That is the common case and it is exact.
 *
 * When the minute itself disagrees, which happens when one feed is running an
 * older timetable version, the line and the platform together identify it, but
 * only while there is exactly one such candidate nearby. A line that comes every
 * few minutes has several, and then this answers nothing rather than guessing.
 *
 * Several identifiers may come back, because two feeds sometimes publish one
 * train under two route names at the same minute. The caller decides what to do
 * with a run it cannot narrow to one; see `applyVia`.
 */
export function matchTrips(row: Departure, aggregator: readonly Departure[]): string[] {
  return matchCandidates(row, aggregator, (candidate) => candidate.tripId);
}

/**
 * The aggregator's own rows for this run, by the same rule.
 *
 * Same tiers, same order, same refusal to guess. What differs is what comes
 * back: the rows themselves rather than their identifiers, for a caller that
 * wants a field the primary feed left empty rather than the run's identity.
 * Rows with no trip identifier count here, because the field being borrowed
 * does not depend on one.
 */
export function matchRows(row: Departure, aggregator: readonly Departure[]): Departure[] {
  return matchCandidates(row, aggregator, (candidate) => candidate);
}

/**
 * The tiers themselves, over whatever the caller wants out of a candidate.
 *
 * `pick` answering undefined drops that candidate before the tiers are
 * compared, which is what keeps `matchTrips` answering exactly as it did when
 * it owned this code: a candidate with no identifier was never a candidate.
 */
function matchCandidates<T>(
  row: Departure,
  aggregator: readonly Departure[],
  pick: (candidate: Departure) => T | undefined,
): T[] {
  // In descending order of how much a tier proves. A timetabled minute that two
  // feeds agree on is the strongest single fact available, and the expected
  // minute is next: two runs of one line can share an expected minute when one
  // of them is late, so agreement there is worth less than agreement on the
  // timetable. The line is worth more than the headsign because two lines can
  // terminate in the same place, and a run is only ever one line.
  const byLinePlanned: T[] = [];
  const byLineExpected: T[] = [];
  const byHeadsignPlanned: T[] = [];
  const byHeadsignExpected: T[] = [];
  const nearby: T[] = [];

  for (const candidate of aggregator) {
    const tripId = pick(candidate);
    if (tripId === undefined) continue;
    const line = normaliseLine(candidate.line) === normaliseLine(row.line);
    const headsign = sameDestinationLabel(row.destination, candidate.destination);
    const planned = minuteOf(candidate.planned) === minuteOf(row.planned);
    const expected = minuteOf(candidate.realtime) === minuteOf(row.realtime);
    if (line && planned) byLinePlanned.push(tripId);
    else if (line && expected) byLineExpected.push(tripId);
    else if (headsign && planned) byHeadsignPlanned.push(tripId);
    else if (headsign && expected) byHeadsignExpected.push(tripId);
    else if (
      line &&
      row.platform !== null &&
      candidate.platform !== null &&
      samePlatformLabel(row.platform, candidate.platform) &&
      Math.abs(candidate.planned - row.planned) <= NEAR_MINUTES * 60_000
    ) {
      nearby.push(tripId);
    }
  }

  const distinct = (ids: readonly T[]): T[] => [...new Set(ids)];
  for (const tier of [byLinePlanned, byLineExpected, byHeadsignPlanned, byHeadsignExpected]) {
    if (tier.length > 0) return distinct(tier);
  }
  // Only ever one. A line that comes every few minutes has several candidates
  // this close, and guessing between them is worse than saying nothing.
  const only = distinct(nearby);
  return only.length === 1 ? only : [];
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

  let aggregator: readonly Departure[] = [];
  if (rows.some((row) => row.tripId === undefined) && options.aggregatorRows !== undefined) {
    try {
      aggregator = await options.aggregatorRows();
    } catch {
      // No aggregator answer means no trip identifiers for the rows that lack
      // one, which means those rows are unverified. It is not a reason to empty
      // the board.
      aggregator = [];
    }
  }

  const verdicts = await Promise.all(
    rows.map(async (row): Promise<Departure | null> => {
      const candidates = row.tripId !== undefined ? [row.tripId] : matchTrips(row, aggregator);
      const unverified = (): Departure => ({ ...row, viaUnverified: true });
      if (candidates.length === 0) return unverified();

      const answers: Array<{ tripId: string; callsThere: boolean }> = [];
      for (const tripId of candidates) {
        let calls;
        try {
          calls = await tripCalls(tripId, options);
        } catch {
          continue;
        }
        if (calls.length === 0) continue;
        // The run's own clock, not the row's: a run matched across a timetable
        // version leaves at a different minute in each feed, and "still ahead"
        // has to mean ahead of the call this row describes.
        const leaves = departureFrom(calls, row.stop, row.planned) ?? row.planned;
        answers.push({ tripId, callsThere: callsAtAfter(calls, stations, leaves) });
      }

      // Nothing answered, or the candidates answered differently: the run was
      // never identified, whatever the count of identifiers suggested.
      if (answers.length === 0) return unverified();
      const first = answers[0] as { tripId: string; callsThere: boolean };
      if (answers.some((answer) => answer.callsThere !== first.callsThere)) return unverified();
      if (!first.callsThere) return null;
      const kept = { ...row, tripId: first.tripId };
      delete kept.viaUnverified;
      return kept;
    }),
  );

  return verdicts.filter((row): row is Departure => row !== null);
}
