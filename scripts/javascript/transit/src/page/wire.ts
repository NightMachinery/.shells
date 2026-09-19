// What the planning server says, and how both ends agree about it.
//
// The server runs the same package the page runs, so the shapes it works with
// are the page's own: `Board`, `PlannedRow`, `ProfileRoutes`. Two of those hold
// `Map`s, and a `Map` does not survive `JSON.stringify` (it becomes `{}`, with
// no error anywhere), so the wire format is the same data with its maps written
// as pairs. Everything else travels as it stands.
//
// This module is the only place that knows the format, and both ends import it
// from here rather than each parsing what the other writes. That is the whole
// point: a server and a page that agree about a format by coincidence stop
// agreeing the first time a field is added.

import type { Board, Message } from '../model.ts';
import type { PlannedRow } from '../plan.ts';
import type { OriginLevel } from '../origin.ts';
import type { BoardResult, BoardRoutes, ProfileRoutes } from './commute.ts';

/** Bumped when a field changes meaning. A page that reads an older one asks upstream itself. */
export const WIRE_VERSION = 1;

/** One board's journeys, as they travel. */
export interface WireBoardRoutes {
  index: number;
  /** Where this board was planning to, which decides what may be carried forward. */
  destinationKey: string;
  /** False when the search failed or found nothing to say: the page keeps what it had. */
  answered: boolean;
  origin: OriginLevel | null;
  rows: Array<[string, PlannedRow]>;
}

/** A whole profile's answer: the boards, and the journeys off them. */
export interface WireProfileAnswer {
  v: number;
  /** When the server composed this, epoch milliseconds. */
  at: number;
  profileKey: string;
  /** The instant the boards were asked about, which the page compares with its own. */
  startMs: number;
  horizonMinutes: number;
  /** Which backends answered, for the provenance line. */
  backends: string[];
  boards: Board[];
  /** Absent when the profile plans nothing, or when every board's search failed. */
  routes: WireBoardRoutes[] | null;
  /** The destination the journeys were planned towards; the empty string for none. */
  destinationKey: string;
}

/** The service messages, which need no transformation but do need a version. */
export interface WireMessages {
  v: number;
  at: number;
  messages: Message[];
}

/** Journeys as the page holds them, written out as pairs. */
export function encodeRoutes(routes: ProfileRoutes | null, wanted: ReadonlyMap<number, string>): WireBoardRoutes[] | null {
  if (routes === null) return null;
  const out: WireBoardRoutes[] = [];
  for (const [index, destinationKey] of wanted) {
    const board = routes.boards.get(index);
    out.push({
      index,
      destinationKey,
      answered: board !== undefined,
      origin: board?.origin ?? null,
      rows: board === undefined ? [] : [...board.rows],
    });
  }
  return out;
}

/**
 * The wire's journeys, as the page holds them.
 *
 * Two things come back rather than one: what the boards said, and what the
 * server set out to plan. The second is what lets the page carry its own
 * previous answers forward for a board the server could not answer for, which
 * is the same rule the page applies to its own searches.
 */
export function decodeRoutes(wire: WireBoardRoutes[] | null): { results: BoardResult[]; wanted: Map<number, string> } {
  const results: BoardResult[] = [];
  const wanted = new Map<number, string>();
  for (const board of wire ?? []) {
    wanted.set(board.index, board.destinationKey);
    if (!board.answered) {
      results.push({ index: board.index, routes: null });
      continue;
    }
    const routes: BoardRoutes = {
      rows: new Map(board.rows),
      origin: board.origin,
      destinationKey: board.destinationKey,
    };
    results.push({ index: board.index, routes });
  }
  return { results, wanted };
}

/** The query one profile answer is asked for by, and cached under. */
export interface ProfileQuery {
  profileKey: string;
  horizonMinutes: number;
  /** Epoch milliseconds. Quantised to the minute by the server's own cache key. */
  startMs: number;
  destinationKey: string | null;
  walkWeight?: number | undefined;
  earlyBufferMinutes?: number | undefined;
}

/** The query as a URL search string, written once so both ends spell it the same. */
export function profileSearch(query: ProfileQuery): string {
  const params = new URLSearchParams();
  params.set('horizon', String(query.horizonMinutes));
  params.set('start', String(query.startMs));
  if (query.destinationKey !== null && query.destinationKey !== '') params.set('to', query.destinationKey);
  if (query.walkWeight !== undefined) params.set('walk_weight', String(query.walkWeight));
  if (query.earlyBufferMinutes !== undefined) params.set('early_buffer', String(query.earlyBufferMinutes));
  return params.toString();
}

/** The same query, read back, with the defaults a missing parameter means. */
export function readProfileQuery(profileKey: string, params: URLSearchParams, now: number, defaultHorizon: number): ProfileQuery {
  const number = (name: string): number | undefined => {
    const raw = params.get(name);
    if (raw === null) return undefined;
    const value = Number(raw);
    return Number.isFinite(value) ? value : undefined;
  };
  const to = params.get('to');
  return {
    profileKey,
    horizonMinutes: number('horizon') ?? defaultHorizon,
    startMs: number('start') ?? now,
    destinationKey: to === null || to === '' ? null : to,
    walkWeight: number('walk_weight'),
    earlyBufferMinutes: number('early_buffer'),
  };
}
