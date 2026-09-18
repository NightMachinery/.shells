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
  departures(stop: string, window: Window): Promise<Departure[]>;
  messages(): Promise<Message[]>;
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
