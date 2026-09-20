// Where a departure goes after it leaves, asked only when someone asks.
//
// A board row says what the train is called and where it ends up, and those two
// facts leave the question a rider at a junction actually has, which is whether
// it stops at the place they are going to. The answer is in the vehicle's own
// stop sequence, which the aggregator publishes against a trip identifier.
//
// Lazily, because that is one request per vehicle and a board is two hundred
// rows. Nothing here runs until something on screen asks about a particular
// departure: opening its sheet, hovering its time, or being a regional service,
// which is the one case where the answer is worth a slot on the row itself.
//
// The result is remembered for the life of the page under the row's key, so the
// several times a minute the board rebuilds itself cost nothing, and an answer
// that could not be found is remembered too: a run nothing could identify at
// half past four is still unidentifiable at half past four, and re-asking every
// render would be a request loop with a spinner on top.

import type { Departure } from '../model.ts';
import { callsAfter, tripCalls, type TripCall, type TripOptions } from '../trip.ts';
import { matchTrips } from '../via.ts';

/**
 * Why a run's onward calls are not on screen.
 *
 * One sentence per cause, because "not published" was covering four different
 * faults and a reader holding a phone could not tell which. Measured on the
 * live configuration: every row of two boards at one station said "not
 * published" for months, and the cause was none of the things that sentence
 * suggests. It was the aggregator answering 404 for the station's parent
 * identifier, which the fan-out over its platforms already knew how to handle
 * and never got the chance to, because the 404 threw first.
 */
export type CallsGap =
  /** The aggregator lists no stop times at this stop at all. */
  | 'no-stop-times'
  /** It lists stop times, and none of them is this departure. */
  | 'no-match'
  /** The run was identified and the aggregator publishes no sequence for it. */
  | 'no-sequence'
  /** A request failed. Says nothing about the run; it will be asked again. */
  | 'unreachable';

/** What is known about one row's onward calls. */
export type Calls =
  | { kind: 'loading' }
  /** The calls still ahead of this departure, in order, possibly none. */
  | { kind: 'ready'; calls: TripCall[] }
  /** Nothing identified the run, or the aggregator would not say. */
  | { kind: 'unknown'; gap: CallsGap };

/** Where the onward calls come from, and who to tell when one lands. */
export interface CallsSource extends TripOptions {
  /**
   * The aggregator's own rows for a stop, for rows whose backend publishes no
   * trip identifier. Shared with the board's timetable fetch where the window
   * and the modes happen to match, and one extra cached request per stop where
   * they do not.
   */
  rows: (stop: string) => Promise<Departure[]>;
  /** Called after an answer lands, to draw it. Coalesced; see `wake`. */
  onLoaded: () => void;
}

const known = new Map<string, Calls>();
/**
 * When a key that failed on the network may be asked again.
 *
 * An unidentifiable run is unidentifiable for good and is remembered for the
 * life of the page, which is the whole reason this module has a memo. A request
 * that never arrived is a different thing: it says nothing about the run, and
 * marking it unknowable for ever means a reader who was on a train in a tunnel
 * when they first opened the sheet is told for the rest of the day that the
 * timetable does not exist. So a network failure is remembered too, but with an
 * expiry, and re-asking is capped by the clock rather than by the render loop.
 */
const retryAt = new Map<string, number>();

/** How long a failed lookup is left alone before anything asks again. */
export const CALLS_RETRY_MS = 30_000;
const expanded = new Set<string>();
let source: CallsSource | null = null;
let waking = false;
/**
 * Bumped whenever an answer lands or a list is expanded, for a board's own
 * signature to include.
 *
 * A board rebuilds only when its signature changes, and that signature is
 * built from `board.departures` and the render context, neither of which
 * knows this module exists: an answer arriving, or a reader tapping "and N
 * more", changes nothing either one tracks. Without this counter in the mix
 * the board would keep the DOM it already drew, an already-open sheet would
 * never learn its spinner became a list or its list grew, and the row itself
 * would never learn a `via` line was now available to show.
 */
let version = 0;

/** The current version, for a board's signature to fold in. */
export function callsVersion(): number {
  return version;
}

/**
 * Point this at whatever answered the last fetch.
 *
 * Called again on every profile fetch rather than once at boot, because the
 * backends and the window are built per fetch and the horizon the reader picked
 * is part of them.
 */
export function configureCalls(next: CallsSource): void {
  source = next;
}

/** Forget everything, for a test that wants a fresh page. */
export function clearCalls(): void {
  known.clear();
  retryAt.clear();
  expanded.clear();
  source = null;
}

/**
 * One redraw per batch of answers, not one per answer.
 *
 * A board of regional rows resolves a dozen runs within a few milliseconds of
 * each other, and a whole-page render for each of them is a dozen renders for
 * one visible change.
 */
function wake(): void {
  if (source === null || waking) return;
  waking = true;
  queueMicrotask(() => {
    waking = false;
    source?.onLoaded();
  });
}

/**
 * Run a request, and run it a second time if the first failed.
 *
 * One retry, not a policy: a single timeout or a single rate-limited response
 * is the common case and costs one more request to survive, and anything that
 * fails twice in a row is a condition rather than a blip.
 */
async function onceMore<T>(attempt: () => Promise<T>): Promise<T> {
  try {
    return await attempt();
  } catch {
    return attempt();
  }
}

async function load(key: string, dep: Departure, from: CallsSource): Promise<void> {
  const settle = (value: Calls): void => {
    known.set(key, value);
    if (value.kind === 'unknown' && value.gap === 'unreachable') retryAt.set(key, Date.now() + CALLS_RETRY_MS);
    else retryAt.delete(key);
    version += 1;
    wake();
  };
  let tripId = dep.tripId;
  if (tripId === undefined) {
    let rows: Departure[];
    try {
      rows = await onceMore(() => from.rows(dep.stop));
    } catch {
      return settle({ kind: 'unknown', gap: 'unreachable' });
    }
    // A stop the aggregator does not carry at all is a different fault from a
    // stop it carries and where this run is not listed, and only the second one
    // is about this departure.
    if (rows.length === 0) return settle({ kind: 'unknown', gap: 'no-stop-times' });
    // Several candidates means the two feeds published one train under two
    // route names, which is the case this was written for; their sequences are
    // the same sequence. `applyVia` is stricter because it is deciding whether
    // to hide a row, and this is only deciding what to print on one.
    tripId = matchTrips(dep, rows)[0];
    if (tripId === undefined) return settle({ kind: 'unknown', gap: 'no-match' });
  }
  let calls: TripCall[];
  try {
    calls = await onceMore(() => tripCalls(tripId, from));
  } catch {
    return settle({ kind: 'unknown', gap: 'unreachable' });
  }
  if (calls.length === 0) return settle({ kind: 'unknown', gap: 'no-sequence' });
  settle({ kind: 'ready', calls: callsAfter(calls, dep.stop, dep.planned) });
}

/**
 * What is known about where this departure goes, starting the lookup if this is
 * the first time anything has asked.
 *
 * Synchronous on purpose: a renderer that had to await would have to be async
 * all the way up, and this page is built around a render that rebuilds
 * everything from state in one pass. The answer arrives by a later render.
 */
export function onwardCalls(key: string, dep: Departure): Calls {
  const hit = known.get(key);
  const due = retryAt.get(key);
  if (hit !== undefined && (due === undefined || Date.now() < due)) return hit;
  const from = source;
  if (from === null) return { kind: 'unknown', gap: 'unreachable' };
  known.set(key, { kind: 'loading' });
  retryAt.delete(key);
  void load(key, dep, from);
  return { kind: 'loading' };
}

/**
 * What is already known, without asking for it.
 *
 * The board computes a signature for every row on every render to decide
 * whether an open sheet still says the right thing, and a signature that
 * started a lookup would start one per row on the board, which is the whole
 * cost this module exists to avoid.
 */
export function peekCalls(key: string): Calls | undefined {
  return known.get(key);
}

/** Whether the reader has asked this row for its whole list rather than the head of it. */
export function callsExpanded(key: string): boolean {
  return expanded.has(key);
}

/** Ask for the whole list. There is no way back, because the sheet is a glance. */
export function expandCalls(key: string): void {
  expanded.add(key);
  version += 1;
}
