// The aggregator's address and identifier shape, in a module of their own.
//
// They live here rather than in the backend that uses them most because the
// origin resolver needs both, and the backend needs the resolver: keeping the
// two constants and the two conversions apart is what stops that from being an
// import cycle. The backend re-exports them, so nothing else has to know.

/** Overridable with the `TRANSITOUS_BASE_URL` environment variable or a page global. */
export const TRANSITOUS_DEFAULT_BASE_URL = 'https://api.transitous.org/api/v1';

/**
 * The aggregator carries the same national stop identifiers as the primary
 * backend, under a source prefix.
 */
export const DELFI_ID_PREFIX = 'de-DELFI_';

/** Accept a bare national id or one that already carries the source prefix. */
export function toAggregatorId(stop: string): string {
  return /^[a-z]{2}-[A-Za-z0-9]+_/.test(stop) ? stop : `${DELFI_ID_PREFIX}${stop}`;
}

export function toRawId(stop: string): string {
  return stop.startsWith(DELFI_ID_PREFIX) ? stop.slice(DELFI_ID_PREFIX.length) : stop;
}
