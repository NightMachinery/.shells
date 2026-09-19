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

/** What is known about one row's onward calls. */
export type Calls =
  | { kind: 'loading' }
  /** The calls still ahead of this departure, in order, possibly none. */
  | { kind: 'ready'; calls: TripCall[] }
  /** Nothing identified the run, or the aggregator would not say. */
  | { kind: 'unknown' };

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

async function load(key: string, dep: Departure, from: CallsSource): Promise<void> {
  const settle = (value: Calls): void => {
    known.set(key, value);
    version += 1;
    wake();
  };
  try {
    let tripId = dep.tripId;
    if (tripId === undefined) {
      // Several candidates means the two feeds published one train under two
      // route names, which is the case this was written for; their sequences are
      // the same sequence. `applyVia` is stricter because it is deciding whether
      // to hide a row, and this is only deciding what to print on one.
      tripId = matchTrips(dep, await from.rows(dep.stop))[0];
    }
    if (tripId === undefined) return settle({ kind: 'unknown' });
    const calls = await tripCalls(tripId, from);
    if (calls.length === 0) return settle({ kind: 'unknown' });
    settle({ kind: 'ready', calls: callsAfter(calls, dep.stop, dep.planned) });
  } catch {
    settle({ kind: 'unknown' });
  }
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
  if (hit !== undefined) return hit;
  const from = source;
  if (from === null) return { kind: 'unknown' };
  known.set(key, { kind: 'loading' });
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
