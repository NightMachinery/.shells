import type { Departure } from './model.ts';
import type { RouteOption } from './plan.ts';

// What one journey slot hands to the page that shows it in full.
//
// Everything travels in the URL fragment, as base64url of a small JSON object.
// That is an unusual choice and it is the point: the expanded page then needs no
// network at all. It opens instantly, it opens underground, it opens from a
// link somebody pasted into a message an hour ago, and it cannot disagree with
// the board it came from, because it is not asking anybody a second time. A
// query string would have done the same job and would have been sent to the
// server on every open; a fragment never leaves the browser.
//
// The cost is that a link is a snapshot. The page says when it was planned and
// carries a button to plan again, which is the honest version of that trade.

/** The shape carried in the fragment. `v` is here so an old link can be refused. */
export interface RouteHandoff {
  v: 1;
  /** What the board this came from is called. */
  board: string;
  /** What the destination is called. */
  destination: string;
  /** The place key it was planned to, so the page can plan again. */
  destinationKey: string;
  timezone: string;
  /** When the journeys were planned, epoch milliseconds. */
  plannedAt: number;
  /**
   * The row the journeys start from, whole rather than reduced to what is
   * drawn. It is a few hundred bytes and it is what the "plan again" button
   * needs: a subset would have to be widened the first time that button wanted
   * a field somebody had trimmed.
   */
  row: Departure;
  /** Where the first leg is boarded, for the header. */
  from: string;
  /** Every journey found for that row, best first. */
  options: RouteOption[];
  /** Which of them to show expanded, as an index into `options`. */
  first: number;
}

function toBase64Url(bytes: Uint8Array): string {
  let binary = '';
  for (const byte of bytes) binary += String.fromCharCode(byte);
  return btoa(binary).replace(/\+/g, '-').replace(/\//g, '_').replace(/=+$/, '');
}

function fromBase64Url(text: string): Uint8Array {
  const padded = text.replace(/-/g, '+').replace(/_/g, '/');
  const binary = atob(padded + '='.repeat((4 - (padded.length % 4)) % 4));
  const bytes = new Uint8Array(binary.length);
  for (let index = 0; index < binary.length; index += 1) bytes[index] = binary.charCodeAt(index);
  return bytes;
}

/** The fragment for one row's journeys, without the leading `#`. */
export function encodeRoute(handoff: RouteHandoff): string {
  return toBase64Url(new TextEncoder().encode(JSON.stringify(handoff)));
}

/**
 * The journeys a fragment carries, or null when it is absent, damaged or from a
 * version this page does not know. Null is a state the page renders, not an
 * error it throws: a link that has been mangled by a chat client is a thing
 * that happens, and the page should say so rather than break.
 */
export function decodeRoute(fragment: string): RouteHandoff | null {
  const text = fragment.startsWith('#') ? fragment.slice(1) : fragment;
  if (text.length === 0) return null;
  try {
    const parsed: unknown = JSON.parse(new TextDecoder().decode(fromBase64Url(text)));
    if (typeof parsed !== 'object' || parsed === null) return null;
    const handoff = parsed as RouteHandoff;
    if (handoff.v !== 1 || !Array.isArray(handoff.options)) return null;
    return handoff;
  } catch {
    return null;
  }
}

/** The whole URL of the expanded page for one row's journeys. */
export function routeUrl(handoff: RouteHandoff, base = 'route.html'): string {
  return `${base}#${encodeRoute(handoff)}`;
}

/** The hand-off for one planned row, as the board has it. */
export function handoffFor(input: {
  board: string;
  destination: string;
  destinationKey: string;
  timezone: string;
  plannedAt: number;
  departure: Departure;
  from: string;
  options: RouteOption[];
  first?: number;
}): RouteHandoff {
  return {
    v: 1,
    board: input.board,
    destination: input.destination,
    destinationKey: input.destinationKey,
    timezone: input.timezone,
    plannedAt: input.plannedAt,
    row: input.departure,
    from: input.from,
    options: input.options,
    first: input.first ?? 0,
  };
}
