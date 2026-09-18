import { envOverride, fetchJson, type FetchLike } from '../http.ts';
import { ALL_MODES, type Departure, type Direction, type Message, type Mode, type StopHit } from '../model.ts';
import {
  MAX_PAGES,
  narrowModes,
  type Backend,
  type BackendOptions,
  type DepartureOptions,
  type Window,
} from './types.ts';

export const TRANSITOUS_BACKEND_NAME = 'transitous';

/** Overridable with the `TRANSITOUS_BASE_URL` environment variable or a page global. */
export const TRANSITOUS_DEFAULT_BASE_URL = 'https://api.transitous.org/api/v1';

/**
 * The aggregator carries the same national stop identifiers as the primary
 * backend, under a source prefix.
 */
export const DELFI_ID_PREFIX = 'de-DELFI_';

/**
 * Rows fetched per hour of horizon, used to size a page request. The endpoint
 * does not cap the count, so this only trades a little over-fetching against
 * an extra round trip.
 */
export const ROWS_PER_HOUR_ESTIMATE = 60;
export const MIN_ROWS_PER_REQUEST = 50;
export const MAX_ROWS_PER_REQUEST = 500;

/**
 * Whether this backend's direction flag may be published as a direction letter.
 *
 * The mapping below (outbound flag zero to the first letter, one to the second)
 * is *unverified* against the primary backend on the same stop: nobody has
 * confirmed that the aggregator's outbound sense agrees with the letter baked
 * into the primary backend's line identifiers. It is asserted by a fixture test
 * so the behaviour cannot drift unnoticed, but the fixture proves only internal
 * consistency, not agreement upstream.
 *
 * Flip this to `false` if the two are ever found to disagree. Every row this
 * backend emits then carries no direction, a board's `direction` filter drops
 * all of them, and `destinations` filters have to do the narrowing instead.
 */
export const TRANSITOUS_DIRECTION_TRUSTED = true;

/** Aggregator vehicle categories mapped onto this package's vocabulary. */
export const MODE_MAP: Readonly<Record<string, Mode>> = {
  METRO: 'SBAHN',
  SUBWAY: 'UBAHN',
  REGIONAL_RAIL: 'BAHN',
  TRAM: 'TRAM',
  BUS: 'BUS',
};

/**
 * Agency names observed on urban bus trips in the aggregator's feed. The city
 * operator does not brand itself with its own initials there, so a match on
 * those alone finds nothing; these are the names it actually publishes.
 */
export const CITY_BUS_AGENCIES: readonly string[] = ['Bus München', 'ExpressBus'];

/**
 * Heuristic. The aggregator has one bus category where the primary backend has
 * two, and the only thing separating them in the feed is the operating agency:
 * the city transport company runs the urban network, everyone else runs the
 * regional one.
 *
 * Agency naming is not a contract and this list was read off one stop's feed,
 * so a wrong answer here is a mislabelled bus, not a bug worth contorting the
 * code for. The regional brand is excluded explicitly because it shares a
 * prefix with the city operator's initials and would otherwise be caught by a
 * loose match.
 */
export function isCityBusAgency(agencyName: string | null | undefined): boolean {
  if (typeof agencyName !== 'string') return false;
  const name = agencyName.trim().toLowerCase();
  if (name.length === 0) return false;
  if (name.includes('regionalbus')) return false;
  if (/\bmvg\b/.test(name)) return true;
  return CITY_BUS_AGENCIES.some((known) => known.toLowerCase() === name);
}

/** Strip the trailing train number the aggregator appends to rail line names. */
export function normaliseLineName(routeShortName: unknown): string {
  return String(routeShortName ?? '')
    .replace(/\s*\(\d+\)\s*$/, '')
    .trim();
}

/** Map the aggregator's outbound flag onto a direction letter. See the constant above. */
export function directionFromDirectionId(directionId: unknown): Direction {
  if (!TRANSITOUS_DIRECTION_TRUSTED) return null;
  const value = typeof directionId === 'string' ? Number(directionId) : directionId;
  if (value === 0) return 'H';
  if (value === 1) return 'R';
  return null;
}

/** Accept a bare national id or one that already carries the source prefix. */
export function toAggregatorId(stop: string): string {
  return /^[a-z]{2}-[A-Za-z0-9]+_/.test(stop) ? stop : `${DELFI_ID_PREFIX}${stop}`;
}

export function toRawId(stop: string): string {
  return stop.startsWith(DELFI_ID_PREFIX) ? stop.slice(DELFI_ID_PREFIX.length) : stop;
}

function parseIso(value: unknown): number {
  if (typeof value !== 'string') return Number.NaN;
  return Date.parse(value);
}

function normaliseColor(value: unknown): string | null {
  if (typeof value !== 'string' || value.trim().length === 0) return null;
  const text = value.trim();
  return text.startsWith('#') ? text : `#${text}`;
}

interface RawPlace {
  name?: string;
  stopId?: string;
  departure?: string;
  scheduledDeparture?: string;
  track?: string;
  scheduledTrack?: string;
}

interface RawStopTime {
  place?: RawPlace;
  mode?: string;
  realTime?: boolean;
  headsign?: string;
  agencyName?: string;
  routeShortName?: string;
  routeColor?: string;
  directionId?: number | string;
  cancelled?: boolean;
}

interface RawStopTimes {
  stopTimes?: RawStopTime[];
  nextPageCursor?: string;
  previousPageCursor?: string;
}

interface RawGeocodeHit {
  id?: string;
  name?: string;
  type?: string;
  areas?: Array<{ name?: string }>;
}

/** Optional durable cache for the platform-id lookup, injected by the CLI. */
export interface LookupCache {
  get(key: string): Promise<string[] | null>;
  set(key: string, value: string[]): Promise<void>;
}

export interface TransitousOptions extends BackendOptions {
  lookupCache?: LookupCache;
}

export function createTransitousBackend(options: TransitousOptions = {}): Backend {
  const baseUrl = (options.baseUrl ?? envOverride('TRANSITOUS_BASE_URL') ?? TRANSITOUS_DEFAULT_BASE_URL).replace(
    /\/+$/,
    '',
  );
  // No `null` sentinel for "everything": a per-call narrowing has to intersect
  // with something, and spelling the default out as every category keeps the two
  // paths identical.
  const wanted: readonly Mode[] = options.transportTypes ?? ALL_MODES;
  const fetchImpl: FetchLike | undefined = options.fetchImpl;
  const debug = options.onDebug;
  const progress = options.onProgress;
  const lookupCache = options.lookupCache;
  // Per-process memo so a board that merges several stops does not re-resolve
  // the same parent id once per stop.
  const platformMemo = new Map<string, string[]>();

  async function get<T>(path: string): Promise<T> {
    const url = `${baseUrl}${path}`;
    debug?.(`GET ${url}`);
    return fetchJson<T>(url, fetchImpl ? { fetchImpl } : {});
  }

  function toDeparture(row: RawStopTime, stop: string): Departure | null {
    const place = row.place ?? {};
    const realtime = parseIso(place.departure ?? place.scheduledDeparture);
    const planned = parseIso(place.scheduledDeparture ?? place.departure);
    if (!Number.isFinite(realtime) || !Number.isFinite(planned)) return null;
    const mapped = MODE_MAP[String(row.mode ?? '')];
    if (mapped === undefined) return null;
    const mode: Mode = mapped === 'BUS' && !isCityBusAgency(row.agencyName) ? 'REGIONAL_BUS' : mapped;
    const track = (row.place?.track ?? row.place?.scheduledTrack ?? '').toString().trim();
    return {
      line: normaliseLineName(row.routeShortName),
      mode,
      destination: String(row.headsign ?? '').trim(),
      planned,
      realtime,
      delayMin: Math.round((realtime - planned) / 60_000),
      cancelled: row.cancelled === true,
      // The aggregator carries no replacement-service flag.
      sev: false,
      platform: track.length > 0 ? track : null,
      direction: directionFromDirectionId(row.directionId),
      backend: TRANSITOUS_BACKEND_NAME,
      stop,
      realtimeKnown: row.realTime === true,
      color: normaliseColor(row.routeColor),
    };
  }

  function rowsPerRequest(window: Window): number {
    const hours = Math.max(0, window.toMs - window.fromMs) / 3_600_000;
    const estimate = Math.round(hours * ROWS_PER_HOUR_ESTIMATE);
    return Math.min(MAX_ROWS_PER_REQUEST, Math.max(MIN_ROWS_PER_REQUEST, estimate));
  }

  /**
   * `report` is handed in rather than counted here because one `departures`
   * call can walk several platform ids, and a progress indicator wants one
   * run of page numbers for the stop it asked about, not a counter that
   * restarts per platform.
   */
  async function fetchStopRows(
    aggregatorId: string,
    window: Window,
    label: string,
    report: (rows: number) => void,
  ): Promise<Departure[]> {
    const n = rowsPerRequest(window);
    const out: Departure[] = [];
    let cursor: string | null = null;

    for (let page = 0; page < MAX_PAGES; page += 1) {
      const query: string = cursor
        ? `stopId=${encodeURIComponent(aggregatorId)}&n=${n}&pageCursor=${encodeURIComponent(cursor)}`
        : `stopId=${encodeURIComponent(aggregatorId)}&n=${n}&time=${encodeURIComponent(
            new Date(window.fromMs).toISOString(),
          )}`;
      const body: RawStopTimes = await get<RawStopTimes>(`/stoptimes?${query}`);
      const batch = Array.isArray(body.stopTimes) ? body.stopTimes : [];
      debug?.(`transitous page ${page + 1} rows=${batch.length}`);
      report(batch.length);

      let lastTime = Number.NEGATIVE_INFINITY;
      for (const row of batch) {
        const departure = toDeparture(row, label);
        if (departure === null) continue;
        if (departure.realtime > lastTime) lastTime = departure.realtime;
        out.push(departure);
      }

      if (batch.length === 0) break;
      if (!Number.isFinite(lastTime) || lastTime >= window.toMs) break;
      const next: string = typeof body.nextPageCursor === 'string' ? body.nextPageCursor : '';
      if (next.length === 0 || next === cursor) break;
      cursor = next;
    }
    return out;
  }

  /** Resolve a parent stop to the platform ids underneath it. */
  async function platformIds(rawId: string): Promise<string[]> {
    const memo = platformMemo.get(rawId);
    if (memo !== undefined) return memo;
    const cacheKey = `${TRANSITOUS_BACKEND_NAME}:geocode:${rawId}`;
    if (lookupCache) {
      const cached = await lookupCache.get(cacheKey);
      if (cached !== null) {
        platformMemo.set(rawId, cached);
        return cached;
      }
    }
    const hits = await get<RawGeocodeHit[]>(`/geocode?text=${encodeURIComponent(rawId)}`);
    const parent = toAggregatorId(rawId);
    const ids: string[] = [];
    for (const hit of Array.isArray(hits) ? hits : []) {
      const id = typeof hit.id === 'string' ? hit.id : '';
      if (id.length === 0 || id === parent) continue;
      if (!id.includes(rawId)) continue;
      if (!ids.includes(id)) ids.push(id);
    }
    platformMemo.set(rawId, ids);
    if (lookupCache) await lookupCache.set(cacheKey, ids);
    return ids;
  }

  function toStopHit(hit: RawGeocodeHit): StopHit | null {
    const id = typeof hit.id === 'string' ? hit.id : '';
    if (id.length === 0) return null;
    const area = Array.isArray(hit.areas) ? hit.areas.find((a) => typeof a?.name === 'string') : undefined;
    return {
      id,
      name: String(hit.name ?? '').trim(),
      place: area?.name ?? null,
      backend: TRANSITOUS_BACKEND_NAME,
    };
  }

  return {
    name: TRANSITOUS_BACKEND_NAME,

    async search(q: string): Promise<StopHit[]> {
      const hits = await get<RawGeocodeHit[]>(`/geocode?text=${encodeURIComponent(q)}`);
      const out: StopHit[] = [];
      for (const hit of Array.isArray(hits) ? hits : []) {
        const mapped = toStopHit(hit);
        if (mapped !== null) out.push(mapped);
      }
      return out;
    },

    async nearby(lat: number, lon: number): Promise<StopHit[]> {
      const hits = await get<RawGeocodeHit[]>(`/reverse-geocode?place=${lat},${lon}`);
      const out: StopHit[] = [];
      for (const hit of Array.isArray(hits) ? hits : []) {
        const mapped = toStopHit(hit);
        if (mapped !== null) out.push(mapped);
      }
      return out;
    },

    async departures(stop: string, window: Window, call?: DepartureOptions): Promise<Departure[]> {
      // The stoptimes endpoint as used here carries no category parameter, so
      // a narrowing can only shrink what is kept, never what is asked for. The
      // request volume is unchanged; what it buys is a board that does not pay
      // twice for the same filtering.
      const keep = narrowModes(wanted, call?.transportTypes);
      if (keep.length === 0) return [];
      const keepSet = new Set<Mode>(keep);

      let pages = 0;
      const report = (rows: number): void => {
        pages += 1;
        progress?.({ backend: TRANSITOUS_BACKEND_NAME, stop, page: pages, rows });
      };

      const aggregatorId = toAggregatorId(stop);
      let rows = await fetchStopRows(aggregatorId, window, stop, report);

      // Some parent stops carry no departures of their own; the rows hang off
      // the platform ids underneath. One extra lookup, then fan out and merge.
      if (rows.length === 0) {
        const children = await platformIds(toRawId(stop));
        const merged: Departure[] = [];
        for (const child of children) {
          merged.push(...(await fetchStopRows(child, window, stop, report)));
        }
        rows = merged;
      }

      const out = rows.filter((row) => {
        if (!keepSet.has(row.mode)) return false;
        return row.realtime >= window.fromMs && row.realtime <= window.toMs;
      });
      out.sort((a, b) => a.realtime - b.realtime);
      return out;
    },

    async messages(): Promise<Message[]> {
      // The aggregator exposes no service-message endpoint in this API version.
      // Disruption text comes from the primary backend; returning nothing here
      // keeps a fallback chain from pretending otherwise.
      return [];
    },
  };
}
