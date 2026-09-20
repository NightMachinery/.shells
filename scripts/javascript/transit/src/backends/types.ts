import type { Departure, Message, Mode, StopHit } from '../model.ts';

/** A half-open time range in epoch milliseconds. */
export interface Window {
  fromMs: number;
  toMs: number;
}

/**
 * What every data source must be able to do. `chain.ts` composes two of these
 * into one, so anything that consumes a backend works unchanged with a
 * fallback in place.
 */
export interface Backend {
  /** Stable short identifier, copied onto every row this backend produces. */
  name: string;
  search(q: string): Promise<StopHit[]>;
  nearby(lat: number, lon: number): Promise<StopHit[]>;
  departures(stop: string, window: Window, options?: DepartureOptions): Promise<Departure[]>;
  messages(): Promise<Message[]>;
}

/**
 * Per-call narrowing, handed to `departures` by a caller that already knows it
 * will throw most categories away. A board that declares `modes` otherwise
 * spends its page limit on rows the filter deletes a moment later.
 *
 * A backend is allowed to ignore this and return more than was asked for, so
 * the caller's own filtering stays in place either way.
 */
export interface DepartureOptions {
  transportTypes?: readonly Mode[];
  /**
   * Platform identifiers the caller has already seen departures from at this
   * stop, most used first.
   *
   * A hint rather than a request: a backend that can answer for the stop itself
   * ignores it. It exists for the one that cannot, where the rows hang off the
   * platforms and the only way to know which platforms those are is to have
   * seen a row from each. See the origin resolution chain.
   */
  platformIds?: readonly string[];
}

/**
 * The categories one call may actually use: the backend's configured set,
 * intersected with the per-call narrowing when there is one.
 *
 * An intersection rather than a replacement, so a per-call list can only ever
 * shrink what the configuration allows; a board naming a category the defaults
 * exclude is a config question, and answering it here would let a board quietly
 * widen the configured set. An empty result means the call cannot produce a
 * row, and callers return without requesting anything rather than sending an
 * empty list, which is not the same thing as a short one.
 */
export function narrowModes(configured: readonly Mode[], requested?: readonly Mode[]): readonly Mode[] {
  if (requested === undefined) return configured;
  const allowed = new Set<Mode>(configured);
  return requested.filter((mode) => allowed.has(mode));
}

/**
 * One completed upstream request during a `departures` call. There is no total:
 * how many pages a stop needs is only known once the walk has ended, so a
 * caller can show which page it is on and nothing more.
 */
export interface PageProgress {
  /** The concrete backend that made the request, never a chained name. */
  backend: string;
  /** The stop id the caller asked about, not the id the request was made with. */
  stop: string;
  /** One-based, counting the request that has just come back. */
  page: number;
  /** Rows in that response, after whatever narrowing the server applied. */
  rows: number;
}

/** Construction options shared by both concrete backends. */
export interface BackendOptions {
  baseUrl?: string;
  /** Vehicle categories to request and to keep. */
  transportTypes?: readonly Mode[];
  /** Injected for tests; production passes nothing and gets global `fetch`. */
  fetchImpl?: import('../http.ts').FetchLike;
  /** Called with one line of diagnostics per request when `--verbose` is on. */
  onDebug?: (line: string) => void;
  /**
   * Called once per upstream request while a stop is being paged through.
   * `onDebug` is a log line for a human; this is a structured event for a user
   * interface, which is why they are separate. A progress indicator built on
   * the log line would break the next time the line was reworded.
   */
  onProgress?: (event: PageProgress) => void;
  /**
   * The instant paging offsets are measured from. Production leaves this alone
   * and gets the wall clock; tests supply the instant their fixtures are
   * written around, so paging arithmetic is deterministic.
   */
  now?: () => number;
}

/**
 * How many pages a single stop may be walked through before the backend gives
 * up. A horizon of many hours legitimately needs several pages; a stop whose
 * paging never advances would otherwise spin forever against a free API.
 */
export const MAX_PAGES = 8;
