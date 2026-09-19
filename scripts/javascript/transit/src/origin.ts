import { resetInflight, share } from './inflight.ts';
import { toAggregatorId, toRawId, TRANSITOUS_DEFAULT_BASE_URL } from './aggregator.ts';
import { envOverride, fetchJson, HttpError, type FetchLike } from './http.ts';
import { MVG_DEFAULT_BASE_URL } from './backends/mvg.ts';

// Which identifier the aggregator will actually accept for a stop.
//
// The two services carry the same national stop identifiers, so for years the
// answer was "prefix it and send it". It is not. The aggregator is built from
// the national feed, and that feed does not always publish the *parent* of a
// stop area: several stops exist there only as their platforms. Asking about
// the parent then answers `stop_found=false`, and a board configured with the
// obvious identifier cannot be planned at all, while its departures work fine.
//
// So the identifier is resolved rather than assumed, in three steps, cheapest
// first. Each step is a fact about the data rather than about the moment, which
// is why the answer is cached and why only a 404 is allowed to settle it.

/** Which step of the chain answered. Reported so a surprising plan can be explained. */
export type OriginLevel = 'parent' | 'platform' | 'coordinate';

export interface ResolvedOrigin {
  /**
   * What to send as `fromPlace` or `stopId`. For `coordinate` this is the
   * `lat,lon` pair the planner accepts in place of an identifier.
   */
  place: string;
  /**
   * Every place worth asking, most likely first. Journey planning uses the
   * first; a departure fan-out uses them all, because one platform's stop times
   * are not necessarily the whole stop's.
   */
  places: string[];
  level: OriginLevel;
}

/**
 * Somewhere to keep the answer between runs. The shape is a string list so the
 * existing Redis adapter and a small IndexedDB one on the page can both serve
 * it without a second serialiser.
 */
export interface OriginCache {
  get(key: string): Promise<string[] | null>;
  set(key: string, value: string[]): Promise<void>;
}

export interface ResolveOriginOptions {
  /** The stop as configured, with or without the source prefix. */
  stop: string;
  /**
   * Platform identifiers the primary backend already handed us on this stop's
   * own departure rows, most frequent first. Free information: no request is
   * needed to get them, and they are exactly the platforms the board's lines
   * actually use, so they beat anything a search would guess at.
   */
  platformIds?: string[];
  baseUrl?: string;
  /** Where to ask for a station's coordinate, when it comes to that. */
  mvgBaseUrl?: string;
  fetchImpl?: FetchLike;
  cache?: OriginCache;
  onDebug?: (message: string) => void;
}

/** Answers held for this process, so one board does not re-ask for another. */
const memo = new Map<string, ResolvedOrigin>();

/** Stops every step failed for, so the whole chain is walked once and not again. */
const unresolvable = new Set<string>();

export function clearOriginCache(): void {
  memo.clear();
  unresolvable.clear();
  // Requests in flight are cleared too, or a case that started a probe could
  // hand its answer to the next case, which is precisely what clearing is for.
  resetInflight();
}

export class UnresolvableOriginError extends Error {
  readonly stop: string;
  constructor(stop: string) {
    super(`the journey planner does not know this stop: ${stop}`);
    this.name = 'UnresolvableOriginError';
    this.stop = stop;
  }
}

function encode(resolved: ResolvedOrigin): string[] {
  return [resolved.level, ...resolved.places];
}

function decode(raw: string[]): ResolvedOrigin | null {
  const [level, ...places] = raw;
  if (level !== 'parent' && level !== 'platform' && level !== 'coordinate') return null;
  const first = places[0];
  if (first === undefined) return null;
  return { place: first, places, level };
}

function transitousBase(options: ResolveOriginOptions): string {
  return (options.baseUrl ?? envOverride('TRANSITOUS_BASE_URL') ?? TRANSITOUS_DEFAULT_BASE_URL).replace(/\/+$/, '');
}

function mvgBase(options: ResolveOriginOptions): string {
  return (options.mvgBaseUrl ?? envOverride('MVG_BASE_URL') ?? MVG_DEFAULT_BASE_URL).replace(/\/+$/, '');
}

/**
 * Does the aggregator have stop times for this identifier?
 *
 * One row is asked for, because the question is whether the identifier exists
 * and not what leaves from it. A 404 is the answer "no"; every other failure is
 * a bad moment and is thrown, so a network blip cannot be mistaken for a
 * missing stop and remembered as one.
 *
 * This replaces a `/geocode?text=<stop id>` lookup that used to resolve a stop
 * to its platforms and now returns nothing at all for an identifier in either
 * form, though it still works for a name. A silent nothing is the worst kind of
 * upstream change, so the probe here is one that fails loudly instead.
 */
async function stopKnown(id: string, options: ResolveOriginOptions): Promise<boolean> {
  const url = `${transitousBase(options)}/stoptimes?stopId=${encodeURIComponent(id)}&n=1`;
  try {
    await fetchJson<unknown>(url, { ...(options.fetchImpl === undefined ? {} : { fetchImpl: options.fetchImpl }) });
    return true;
  } catch (error) {
    if (error instanceof HttpError && error.status === 404) return false;
    throw error;
  }
}

interface RawStation {
  globalId?: string;
  latitude?: number;
  longitude?: number;
}

/**
 * The station's own coordinate, from the primary backend's per-station lookup.
 *
 * Not its location search: that takes a name and answers nothing at all for an
 * identifier, which is how the previous platform lookup came to fail silently.
 * The per-station path is an exact identifier lookup and either has the station
 * or does not.
 */
async function stationPoint(rawId: string, options: ResolveOriginOptions): Promise<string | null> {
  const url = `${mvgBase(options)}/stations/${encodeURIComponent(rawId)}`;
  try {
    const station = await fetchJson<RawStation>(url, { ...(options.fetchImpl === undefined ? {} : { fetchImpl: options.fetchImpl }) });
    const { latitude, longitude } = station ?? {};
    if (typeof latitude !== 'number' || typeof longitude !== 'number') return null;
    return `${latitude},${longitude}`;
  } catch {
    return null;
  }
}

interface RawReverseHit {
  id?: string;
  type?: string;
}

/**
 * The stops the aggregator has at a coordinate.
 *
 * It answers with the neighbourhood, not with one stop, so the result is
 * narrowed to identifiers inside the stop area we asked about when any are
 * there. Without that narrowing a board would quietly start reporting the
 * departures of the stop across the road, which is a worse failure than
 * reporting none.
 */
async function stopsAt(point: string, rawId: string, options: ResolveOriginOptions): Promise<string[]> {
  const url = `${transitousBase(options)}/reverse-geocode?place=${encodeURIComponent(point)}&type=STOP`;
  try {
    const hits = await fetchJson<RawReverseHit[]>(url, { ...(options.fetchImpl === undefined ? {} : { fetchImpl: options.fetchImpl }) });
    const ids: string[] = [];
    for (const hit of Array.isArray(hits) ? hits : []) {
      const id = typeof hit.id === 'string' ? hit.id : '';
      if (id.length > 0 && !ids.includes(id)) ids.push(id);
    }
    const prefix = `${toAggregatorId(rawId)}:`;
    const inside = ids.filter((id) => id === toAggregatorId(rawId) || id.startsWith(prefix));
    return inside.length > 0 ? inside : ids.slice(0, 1);
  } catch {
    return [];
  }
}

/**
 * Work out which identifier the aggregator will accept for this stop.
 *
 * Throws `UnresolvableOriginError` when every step fails, which is a statement
 * about the configuration rather than about the network: the caller should say
 * so and carry on showing departures.
 */
export async function resolveOrigin(options: ResolveOriginOptions): Promise<ResolvedOrigin> {
  const rawId = toRawId(options.stop);
  const held = memo.get(rawId);
  if (held !== undefined) return held;
  if (unresolvable.has(rawId)) throw new UnresolvableOriginError(rawId);
  // Shared, because every board of a profile starts planning at the same
  // moment and the memo above is only written once a probe has finished. Two
  // boards at the same stop used to probe the aggregator twice, in parallel,
  // for an answer that cannot differ.
  return share(`origin|${rawId}`, () => resolveOriginUncached(options, rawId));
}

async function resolveOriginUncached(options: ResolveOriginOptions, rawId: string): Promise<ResolvedOrigin> {

  const cacheKey = `origin:${rawId}`;
  if (options.cache) {
    const cached = await options.cache.get(cacheKey);
    const decoded = cached === null ? null : decode(cached);
    if (decoded !== null) {
      memo.set(rawId, decoded);
      return decoded;
    }
  }

  const keep = async (resolved: ResolvedOrigin): Promise<ResolvedOrigin> => {
    options.onDebug?.(`origin ${rawId} resolved at ${resolved.level}: ${resolved.place}`);
    memo.set(rawId, resolved);
    if (options.cache) await options.cache.set(cacheKey, encode(resolved));
    return resolved;
  };

  // 1. The obvious identifier, which is right most of the time and costs one
  //    small request to confirm.
  const parent = toAggregatorId(rawId);
  if (await stopKnown(parent, options)) return keep({ place: parent, places: [parent], level: 'parent' });

  // 2. The platforms the primary backend already told us about. No search, no
  //    guessing, and they are the platforms this board's own lines depart from.
  const platforms: string[] = [];
  for (const candidate of options.platformIds ?? []) {
    const id = toAggregatorId(toRawId(candidate));
    if (platforms.includes(id)) continue;
    if (await stopKnown(id, options)) platforms.push(id);
  }
  const firstPlatform = platforms[0];
  if (firstPlatform !== undefined) return keep({ place: firstPlatform, places: platforms, level: 'platform' });

  // 3. The coordinate. The planner takes one in place of an identifier, and a
  //    departure fan-out needs identifiers, so both are worked out here.
  const point = await stationPoint(rawId, options);
  if (point !== null) {
    const nearby = await stopsAt(point, rawId, options);
    // The coordinate leads, because for a journey plan it is the thing that
    // cannot be wrong: it is where the stop is. The identifiers follow it for
    // the callers that cannot use a coordinate.
    return keep({ place: point, places: [point, ...nearby], level: 'coordinate' });
  }

  unresolvable.add(rawId);
  throw new UnresolvableOriginError(rawId);
}

/** The identifiers a departure fan-out should walk, given a resolved origin. */
export function departureIds(resolved: ResolvedOrigin): string[] {
  // A coordinate is not something stop times can be asked for, so it is dropped
  // here while staying the right answer for a journey plan.
  return resolved.places.filter((place) => !/^-?\d/.test(place));
}
