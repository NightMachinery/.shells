// Where a vehicle goes after it leaves.
//
// A departure board answers "what leaves here"; it says nothing about the rest
// of the run, and the headsign is a poor substitute. A train from a western
// suburb signed for a town further west may still call at the main station on
// the way, and a train signed for the main station may be going the other way
// round the ring. A board that wants to keep only the departures that actually
// pass a place has to look at the run itself, and this is what looks.
//
// One endpoint, one cache, one shape. The aggregator publishes a trip's whole
// stop sequence against the identifier its stop times carry, which is why those
// identifiers are carried on a row at all.

import { TRANSITOUS_DEFAULT_BASE_URL, toRawId } from './aggregator.ts';
import { envOverride, fetchJson, type FetchLike } from './http.ts';
import { share, withLimit } from './inflight.ts';

/**
 * How many trip lookups may be in the air at once.
 *
 * A board with a via filter asks about every row it is thinking of showing, so
 * a first load of a busy station is dozens of these at once against a free
 * service. Same reasoning as the other gates: what is refused for rate limiting
 * looks to a reader exactly like a board that is broken.
 */
export const TRIP_CONCURRENCY = 4;

/** One call on a vehicle's run. */
export interface TripCall {
  /** The aggregator's platform-level identifier, without its source prefix. */
  stopId: string;
  name: string;
  /** Arrival, epoch milliseconds; the departure where a stop publishes only one. */
  arrivalMs: number;
  departureMs: number;
  /**
   * The track this run uses at this call, when the run's own record names one.
   * The last source a platform badge falls back to, and the only one that is
   * about this vehicle rather than about the stop.
   */
  platform: string | null;
}

interface RawTripPlace {
  name?: string;
  stopId?: string;
  track?: string;
  scheduledTrack?: string;
  arrival?: string;
  departure?: string;
  scheduledArrival?: string;
  scheduledDeparture?: string;
}

interface RawTripLeg {
  from?: RawTripPlace;
  to?: RawTripPlace;
  intermediateStops?: RawTripPlace[] | null;
}

interface RawTrip {
  legs?: RawTripLeg[];
}

/**
 * The station a platform-level identifier belongs to.
 *
 * A national identifier names a station in its first three fields and a
 * platform, an edge or a bay in whatever follows. A journey calls at a station;
 * which edge it uses is not something a board should be asked to know, and the
 * two feeds do not agree about it anyway.
 */
export function stationOf(stopId: string): string {
  const raw = toRawId(stopId.trim());
  const fields = raw.split(':');
  return fields.length <= 3 ? raw : fields.slice(0, 3).join(':');
}

function parseTime(...values: Array<string | undefined>): number {
  for (const value of values) {
    if (typeof value !== 'string') continue;
    const parsed = Date.parse(value);
    if (Number.isFinite(parsed)) return parsed;
  }
  return Number.NaN;
}

function toCall(place: RawTripPlace): TripCall | null {
  const stopId = String(place.stopId ?? '').trim();
  if (stopId.length === 0) return null;
  const arrival = parseTime(place.arrival, place.scheduledArrival, place.departure, place.scheduledDeparture);
  const departure = parseTime(place.departure, place.scheduledDeparture, place.arrival, place.scheduledArrival);
  if (!Number.isFinite(arrival) && !Number.isFinite(departure)) return null;
  const track = String(place.track ?? place.scheduledTrack ?? '').trim();
  return {
    stopId: toRawId(stopId),
    name: String(place.name ?? '').trim(),
    arrivalMs: Number.isFinite(arrival) ? arrival : departure,
    departureMs: Number.isFinite(departure) ? departure : arrival,
    platform: track.length > 0 ? track : null,
  };
}

/**
 * A trip's calls, in order, from where it starts to where it ends.
 *
 * The aggregator answers with the run as a single leg: its two ends and the
 * stops between them. They are folded back into one list here because the
 * question every caller has is "does it call here, and when", and the
 * distinction between an end and a middle is the endpoint's business, not
 * theirs.
 */
export function parseTrip(body: unknown): TripCall[] {
  const trip = body as RawTrip;
  const legs = Array.isArray(trip.legs) ? trip.legs : [];
  const calls: TripCall[] = [];
  for (const leg of legs) {
    const from = leg.from === undefined ? null : toCall(leg.from);
    if (from !== null) calls.push(from);
    for (const stop of Array.isArray(leg.intermediateStops) ? leg.intermediateStops : []) {
      const call = toCall(stop);
      if (call !== null) calls.push(call);
    }
    const to = leg.to === undefined ? null : toCall(leg.to);
    if (to !== null) calls.push(to);
  }
  return calls;
}

export interface TripOptions {
  baseUrl?: string | undefined;
  fetchImpl?: FetchLike | undefined;
  onDebug?: ((message: string) => void) | undefined;
  /**
   * A reader is waiting on this one: it goes to the front of the trip gate
   * rather than behind the background checks a board runs on its own rows.
   */
  urgent?: boolean | undefined;
}

/**
 * Trips already fetched, by identifier.
 *
 * A trip identifier names one vehicle on one day, so what it answers cannot
 * change in a way that matters here: the stops a train calls at are set when
 * the day's timetable is published, and a delay moves the times without moving
 * the sequence. That is why this cache has no expiry and needs none. It is
 * cleared between runs of the test suite and never in a browser session, where
 * the alternative is asking the same question every minute for every row on a
 * board that refreshes every minute.
 */
const trips = new Map<string, TripCall[]>();

/** Only for tests, which must not inherit each other's answers. */
export function clearTripCache(): void {
  trips.clear();
}

/** How many trips are held, for the timing record. */
export function cachedTripCount(): number {
  return trips.size;
}

/**
 * One trip's calls, fetched once per run of the program and then remembered.
 *
 * A failure is not remembered: it throws, the caller decides what an unanswered
 * question means, and the next caller asks again. For the via filter that
 * decision is "show the row and say it could not be checked", which is why this
 * function is allowed to be the unreliable part.
 */
export async function tripCalls(tripId: string, options: TripOptions = {}): Promise<TripCall[]> {
  const held = trips.get(tripId);
  if (held !== undefined) return held;
  const baseUrl = (options.baseUrl ?? envOverride('TRANSITOUS_BASE_URL') ?? TRANSITOUS_DEFAULT_BASE_URL).replace(/\/+$/, '');
  const url = `${baseUrl}/trip?tripId=${encodeURIComponent(tripId)}`;
  const calls = await share(`trip|${tripId}`, () =>
    withLimit(
      'trip',
      TRIP_CONCURRENCY,
      async () => {
        options.onDebug?.(`GET ${url}`);
        const body = await fetchJson<unknown>(url, options.fetchImpl ? { fetchImpl: options.fetchImpl } : {});
        return parseTrip(body);
      },
      { front: options.urgent === true },
    ),
  );
  trips.set(tripId, calls);
  return calls;
}

/**
 * Whether a run calls at any of these stations after it has left the one being
 * asked from.
 *
 * Several, because one station can be several identifiers. A main station with
 * an underground rapid-transit hall publishes that hall as its own station: the
 * long-distance trains call at one identifier and the local ones at another,
 * both under the same name on every sign in the building. A filter that knew
 * only one of them threw away exactly the services the board was for.
 *
 * "After" is the whole point. A line that runs out of the city and back through
 * it calls at the main station twice, and a rider boarding at the far end wants
 * to know about the call that is still ahead of them, not the one the train
 * made an hour ago. The departure time is what separates those, and it is taken
 * from the row rather than from the trip, because the row is the thing the
 * reader is looking at.
 */
export function callsAtAfter(calls: TripCall[], stations: readonly string[], afterMs: number): boolean {
  const wanted = new Set(stations.map(stationOf));
  // A minute of slack, because the row's time and the trip's time for the same
  // call come from two responses and need not agree to the second.
  const floor = afterMs - 60_000;
  return calls.some((call) => call.departureMs >= floor && wanted.has(stationOf(call.stopId)));
}

/**
 * When this run leaves a station, by the run's own clock.
 *
 * The board row and the trip are two feeds of one train, and they do not always
 * time it alike: one may be running an older timetable version and have it
 * leaving several minutes earlier or later. Anything that asks "what is still
 * ahead of this departure" has to ask it on one clock, and the run's own clock
 * is the one the rest of its calls are on. Null when the run's list never names
 * the station, which happens when the two feeds disagree about which station an
 * edge belongs to.
 */
export function departureFrom(calls: readonly TripCall[], fromStation: string, nearMs: number): number | null {
  const wanted = stationOf(fromStation);
  let best: number | null = null;
  for (const call of calls) {
    if (stationOf(call.stopId) !== wanted) continue;
    if (best === null || Math.abs(call.departureMs - nearMs) < Math.abs(best - nearMs)) best = call.departureMs;
  }
  return best;
}

/**
 * The calls still ahead of a departure, in order.
 *
 * The first one is dropped when it is the stop being left from: a list of where
 * this train goes should not open with where the reader is standing.
 */
export function callsAfter(calls: TripCall[], fromStation: string, afterMs: number): TripCall[] {
  const wanted = stationOf(fromStation);
  const floor = afterMs - 60_000;
  const out: TripCall[] = [];
  let seenOrigin = false;
  for (const call of calls) {
    if (!seenOrigin && stationOf(call.stopId) === wanted && call.departureMs >= floor) {
      seenOrigin = true;
      continue;
    }
    if (seenOrigin) out.push(call);
  }
  // A trip whose own list never showed the stop being asked from, which happens
  // when the two feeds disagree about which station an edge belongs to. Falling
  // back to the clock is better than answering nothing.
  if (!seenOrigin) return calls.filter((call) => call.arrivalMs >= floor);
  return out;
}
