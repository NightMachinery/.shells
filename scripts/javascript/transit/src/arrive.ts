import { catchableOnward, matchingOnward } from './connect.ts';
import { normaliseLine } from './filter.ts';
import { sameDestinationLabel } from './label.ts';
import type { Arrival, ArriveAtConfig, ConnectionConfig, Departure } from './model.ts';

// Arrivals further down the line. A board may name one stop its vehicles go on
// to, and every row then carries the time that same vehicle reaches it. That is
// the question a board of several boarding points cannot otherwise answer: the
// same tram leaves three stops at three times, and which of them to walk to
// depends on when it gets where the reader is going, which is the one time no
// departure board prints.
//
// The answer is read off the far stop's own departures rather than worked out,
// so it carries whatever the live feed thinks of the delay by then. What takes
// work is knowing which row there is this row's vehicle. Two ways, in order:
//
// - The live feed's own run identifier, the same at every stop. Exact and free,
//   and measured to exist only for runs in about the next twenty-five minutes:
//   past that the primary feed publishes the field empty, so a tram twenty
//   minutes from the far stop is mostly out of reach of it.
// - The timetable. The aggregator names every run for the whole day, so this
//   row is matched to its aggregator run at the boarding stop, that run gives
//   the scheduled minute at the far stop, and the far stop's live row for that
//   line and minute is the arrival. Each link is "same line, same timetabled
//   minute, same stop" and nothing looser. `matchTrips` goes on to expected
//   minutes and headsigns, which is right for asking whether a train goes
//   somewhere and wrong here: a late tram shares its expected minute with the
//   next one's timetabled minute, and the arrival would be the next tram's.
//   Where the live feed publishes no platform, as for trams, the boarding
//   stops' timetable rows are ones the page fetches anyway to borrow one, so
//   this costs only the far stop's.

/**
 * How far past a row's departure its vehicle may reach the far stop and still
 * count. Past it, a matching run is a later trip that happens to reuse the
 * name, and the window fetched at the far stop is extended by the same amount
 * so the last row on the board has something to match.
 */
export const ARRIVE_REACH_MINUTES = 60;

/** What `attachArrivals` reads the far stop's answer from. */
export interface ArrivalSources {
  /** The far stop's rows from the board's own source, live where it can be. */
  far: readonly Departure[];
  /**
   * The aggregator's timetable at each boarding stop, keyed by stop id, for
   * rows the live source did not name. Absent or empty turns the second way off.
   */
  timetableAt?: ReadonlyMap<string, readonly Departure[]>;
  /** The aggregator's timetable at the far stop. */
  farTimetable?: readonly Departure[];
}

interface ArrivalFilter {
  lines?: readonly string[];
  direction?: Departure['direction'];
}

/** The far stop's rows this board's lines and letter can be matched against. */
function usable(rows: readonly Departure[], filter: ArrivalFilter): Departure[] {
  const wanted = filter.lines === undefined ? null : new Set(filter.lines.map(normaliseLine));
  return rows.filter(
    (row) =>
      !row.cancelled &&
      (wanted === null || wanted.has(normaliseLine(row.line))) &&
      (filter.direction === undefined || filter.direction === null || row.direction === filter.direction),
  );
}

/**
 * The earliest of `candidates` that is this row's line, planned after it and
 * within reach. Scheduled times decide, because a delay moves both ends
 * together and must not be able to move a row out of its own window.
 */
function earliestAfter(row: Departure, candidates: readonly Departure[]): Departure | undefined {
  const reach = ARRIVE_REACH_MINUTES * 60_000;
  return candidates
    .filter((far) => normaliseLine(far.line) === normaliseLine(row.line))
    .filter((far) => far.planned > row.planned && far.planned - row.planned <= reach)
    .sort((a, b) => a.planned - b.planned)[0];
}

function byKey(rows: readonly Departure[], key: (row: Departure) => string | undefined): Map<string, Departure[]> {
  const out = new Map<string, Departure[]>();
  for (const row of rows) {
    const value = key(row);
    if (value === undefined) continue;
    const list = out.get(value) ?? [];
    list.push(row);
    out.set(value, list);
  }
  return out;
}

/**
 * The one row of `candidates` that is this line at this timetabled minute, if
 * exactly one is.
 *
 * Narrowed by direction letter and then by headsign only when the line and
 * minute alone leave more than one, because a line timetabled to cross itself
 * at a stop leaves both ways in the same minute, and the two feeds' letters
 * are worked out differently: they are trusted to split a tie, and not asked
 * to confirm a match nothing contests.
 */
function sameScheduled(row: Departure, candidates: readonly Departure[]): Departure | undefined {
  const minute = Math.floor(row.planned / 60_000);
  let hits = candidates.filter(
    (candidate) => normaliseLine(candidate.line) === normaliseLine(row.line) && Math.floor(candidate.planned / 60_000) === minute,
  );
  if (hits.length > 1 && row.direction !== null) hits = hits.filter((candidate) => candidate.direction === row.direction);
  if (hits.length > 1) hits = hits.filter((candidate) => sameDestinationLabel(row.destination, candidate.destination));
  return hits.length === 1 ? hits[0] : undefined;
}

/** This row's aggregator run: its own identifier, else that of the one timetable row it is. */
function aggregatorTrip(row: Departure, timetable: readonly Departure[] | undefined): string | undefined {
  if (row.tripId !== undefined) return row.tripId;
  return sameScheduled(row, (timetable ?? []).filter((candidate) => candidate.tripId !== undefined))?.tripId;
}

/**
 * What the far stop's row says about this row's vehicle getting there.
 *
 * Its live figure when it has one. When it has none, which the live feed does
 * past about half an hour ahead, the timetabled minute there moved by the delay
 * the vehicle is running at here, provided that delay is itself live: a tram
 * four minutes late where the reader boards it is, until anything says
 * otherwise, four minutes late further on. Otherwise the timetable.
 */
function arrivalFrom(row: Departure, far: Departure): Arrival {
  if (far.realtimeKnown) return { at: far.realtime, realtimeKnown: true };
  if (row.realtimeKnown) return { at: far.planned + (row.realtime - row.planned), realtimeKnown: false, estimated: true };
  return { at: far.realtime, realtimeKnown: false };
}

/**
 * Write each row's arrival at the far stop onto it, in place.
 *
 * A row that finds nothing gets `null`, never a guess. That covers a run that
 * ends short of the far stop, a far stop whose departures did not load, and a
 * row neither the live feed nor the timetable could name.
 *
 * When the timetable finds the run at the far stop and the live feed has no
 * row there to match, the timetable's own time is the answer, marked as not
 * live: the vehicle is known to get there, and when it is scheduled to is
 * worth more than a dash.
 */
export function attachArrivals(rows: Departure[], sources: ArrivalSources, filter: ArrivalFilter): void {
  const far = usable(sources.far, filter);
  const farByRun = byKey(far, (row) => row.runId);
  const farTimetable = byKey(usable(sources.farTimetable ?? [], filter), (row) => row.tripId);
  for (const row of rows) {
    const runs = row.runId === undefined ? undefined : farByRun.get(row.runId);
    const live = runs === undefined ? undefined : earliestAfter(row, runs);
    if (live !== undefined) {
      row.arrival = arrivalFrom(row, live);
      continue;
    }
    const trip = aggregatorTrip(row, sources.timetableAt?.get(row.stop));
    const scheduled = trip === undefined ? undefined : earliestAfter(row, farTimetable.get(trip) ?? []);
    if (scheduled === undefined) {
      row.arrival = null;
      continue;
    }
    row.arrival = arrivalFrom(row, sameScheduled(scheduled, far) ?? scheduled);
  }
}

/**
 * The same, for a board whose rows change onto another service before the far
 * stop: each row's arrival is that of the onward departure it would catch.
 *
 * `onward` is the interchange's rows with their own arrivals already attached,
 * by `attachArrivals` on the connection's lines and letter. The onward
 * departure is picked by the same rule the connection slot uses, so the time in
 * the arrival column is always the arrival of the departure the slot names.
 */
export function attachArrivalsThrough(rows: Departure[], onward: readonly Departure[], connection: ConnectionConfig): void {
  const candidates = matchingOnward([...onward], connection);
  for (const row of rows) row.arrival = catchableOnward(row, candidates, connection)?.arrival ?? null;
}

/**
 * A board's rows in the order an arrival board reads them: soonest arrival
 * first, and the rows of one vehicle side by side in the order it reaches its
 * stops. A row with no arrival sorts by its own departure, which places it
 * roughly where it would be and is the only position that claims nothing.
 */
export function byArrival<T extends Pick<Departure, 'realtime' | 'arrival'>>(rows: readonly T[]): T[] {
  return rows
    .map((row, index) => ({ row, index }))
    .sort(
      (a, b) =>
        (a.row.arrival?.at ?? a.row.realtime) - (b.row.arrival?.at ?? b.row.realtime) ||
        a.row.realtime - b.row.realtime ||
        a.index - b.index,
    )
    .map((entry) => entry.row);
}

/** What the far stop is called on screen: its label, or the last field of its id. */
export function arriveLabel(arriveAt: ArriveAtConfig): string {
  if (arriveAt.label !== undefined && arriveAt.label.length > 0) return arriveAt.label;
  const fields = arriveAt.stop.split(':');
  const last = fields[fields.length - 1];
  return last !== undefined && last.length > 0 ? last : arriveAt.stop;
}
