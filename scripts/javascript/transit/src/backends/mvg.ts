import { envOverride, fetchJson, type FetchLike } from '../http.ts';
import { ALL_MODES, isMode, type Departure, type Direction, type Message, type Mode, type StopHit } from '../model.ts';
import {
  MAX_PAGES,
  narrowModes,
  type Backend,
  type BackendOptions,
  type DepartureOptions,
  type Window,
} from './types.ts';

export const MVG_BACKEND_NAME = 'mvg';

/** Overridable with the `MVG_BASE_URL` environment variable or a page global. */
export const MVG_DEFAULT_BASE_URL = 'https://www.mvg.de/api/bgw-pt/v3';

/**
 * Rows requested per page. The API refuses to return more, and it applies this
 * cap *before* the transport-type filter, which is why a board that wants a
 * rarely-served category still has to over-fetch and then narrow client-side.
 *
 * The same ordering is why the number of rows in a page says nothing about
 * whether the stop has more to offer: a page filled to this limit with raw rows
 * can arrive as a handful once the categories are applied. The paging loop below
 * therefore reads times, never counts.
 */
export const MVG_PAGE_LIMIT = 100;

interface RawDeparture {
  label?: string;
  transportType?: string;
  destination?: string;
  plannedDepartureTime?: number;
  realtimeDepartureTime?: number;
  delayInMinutes?: number;
  cancelled?: boolean;
  sev?: boolean;
  platform?: string | number | null;
  lineId?: string;
  realtime?: boolean;
  stopPointGlobalId?: string;
}

/**
 * Pull the direction letter out of a line identifier. The identifier is a
 * colon-joined tuple whose fourth field is the letter; anything else (a short
 * identifier, a missing one, an unexpected letter) yields `null` rather than a
 * guess, because a wrong letter silently empties a board.
 */
export function directionFromLineId(lineId: unknown): Direction {
  if (typeof lineId !== 'string') return null;
  const field = lineId.split(':')[3];
  return field === 'H' || field === 'R' ? field : null;
}

function normalisePlatform(value: RawDeparture['platform']): string | null {
  if (value === null || value === undefined) return null;
  const text = String(value).trim();
  return text.length > 0 ? text : null;
}

function rowTime(row: RawDeparture): number {
  const realtime = typeof row.realtimeDepartureTime === 'number' ? row.realtimeDepartureTime : undefined;
  const planned = typeof row.plannedDepartureTime === 'number' ? row.plannedDepartureTime : undefined;
  return realtime ?? planned ?? Number.NaN;
}

function toDeparture(row: RawDeparture, stop: string): Departure | null {
  const planned = typeof row.plannedDepartureTime === 'number' ? row.plannedDepartureTime : rowTime(row);
  const realtime = rowTime(row);
  if (!Number.isFinite(realtime) || !Number.isFinite(planned)) return null;
  if (!isMode(row.transportType)) return null;
  return {
    line: String(row.label ?? '').trim(),
    mode: row.transportType,
    destination: String(row.destination ?? '').trim(),
    planned,
    realtime,
    delayMin: typeof row.delayInMinutes === 'number' ? row.delayInMinutes : 0,
    cancelled: row.cancelled === true,
    sev: row.sev === true,
    platform: normalisePlatform(row.platform),
    direction: directionFromLineId(row.lineId),
    backend: MVG_BACKEND_NAME,
    stop,
    realtimeKnown: row.realtime === true,
  };
}

function dedupeKey(row: RawDeparture): string {
  return [row.lineId ?? '', row.plannedDepartureTime ?? '', row.destination ?? '', row.label ?? ''].join('|');
}

interface RawLocation {
  globalId?: string;
  name?: string;
  place?: string;
  transportTypes?: string[];
  type?: string;
}

interface RawMessage {
  title?: string;
  text?: string;
  description?: string;
  lines?: Array<{ label?: string } | string>;
  validFrom?: number;
  validTo?: number;
}

/**
 * Message bodies arrive as fragments of HTML. The terminal cannot render it and
 * the page builds its DOM without ever assigning markup, so the tags are turned
 * into whitespace here, at the edge, rather than carried through the model.
 */
export function stripMarkup(html: string): string {
  return html
    .replace(/<br\s*\/?>/gi, ' ')
    .replace(/<\/p\s*>/gi, ' ')
    .replace(/<[^>]*>/g, '')
    .replace(/&nbsp;/g, ' ')
    .replace(/&amp;/g, '&')
    .replace(/&lt;/g, '<')
    .replace(/&gt;/g, '>')
    .replace(/\s+/g, ' ')
    .trim();
}

function toStopHits(rows: unknown): StopHit[] {
  if (!Array.isArray(rows)) return [];
  const hits: StopHit[] = [];
  for (const entry of rows as RawLocation[]) {
    const id = typeof entry.globalId === 'string' ? entry.globalId : '';
    if (id.length === 0) continue;
    const modes = (entry.transportTypes ?? []).filter(isMode) as Mode[];
    hits.push({
      id,
      name: String(entry.name ?? '').trim(),
      place: typeof entry.place === 'string' ? entry.place : null,
      modes,
      backend: MVG_BACKEND_NAME,
    });
  }
  return hits;
}

export function createMvgBackend(options: BackendOptions = {}): Backend {
  const baseUrl = (options.baseUrl ?? envOverride('MVG_BASE_URL') ?? MVG_DEFAULT_BASE_URL).replace(/\/+$/, '');
  const wanted: readonly Mode[] = options.transportTypes ?? ALL_MODES;
  const fetchImpl: FetchLike | undefined = options.fetchImpl;
  const debug = options.onDebug;
  const progress = options.onProgress;
  const clock = options.now ?? (() => Date.now());

  async function get<T>(path: string): Promise<T> {
    const url = `${baseUrl}${path}`;
    debug?.(`GET ${url}`);
    return fetchJson<T>(url, fetchImpl ? { fetchImpl } : {});
  }

  return {
    name: MVG_BACKEND_NAME,

    async search(q: string): Promise<StopHit[]> {
      const rows = await get<unknown>(`/locations?query=${encodeURIComponent(q)}`);
      return toStopHits(rows);
    },

    async nearby(lat: number, lon: number): Promise<StopHit[]> {
      const rows = await get<unknown>(`/stations/nearby?latitude=${lat}&longitude=${lon}`);
      return toStopHits(rows);
    },

    async departures(stop: string, window: Window, call?: DepartureOptions): Promise<Departure[]> {
      // The transport-type list is never omitted. With no explicit list the
      // API silently drops regional rail and regional buses from the response,
      // so a board configured for them would come back plausibly populated and
      // quietly wrong. A per-call narrowing is still an explicit list, just a
      // shorter one, so it is passed straight through; an intersection that
      // comes out empty is the one case that would send no list at all, and it
      // returns without asking rather than risking the silent-drop behaviour.
      const keep = narrowModes(wanted, call?.transportTypes);
      if (keep.length === 0) return [];
      const keepSet = new Set<Mode>(keep);
      const types = keep.join(',');
      const now = clock();
      const collected: RawDeparture[] = [];
      const seen = new Set<string>();
      let offset = Math.max(0, Math.floor((window.fromMs - now) / 60_000));

      for (let page = 0; page < MAX_PAGES; page += 1) {
        const path =
          `/departures?globalId=${encodeURIComponent(stop)}` +
          `&limit=${MVG_PAGE_LIMIT}&offsetInMinutes=${offset}&transportTypes=${encodeURIComponent(types)}`;
        const body = await get<unknown>(path);
        const batch: RawDeparture[] = Array.isArray(body) ? (body as RawDeparture[]) : [];
        debug?.(`mvg page ${page + 1} offset=${offset} rows=${batch.length}`);
        progress?.({ backend: MVG_BACKEND_NAME, stop, page: page + 1, rows: batch.length });

        let newest = Number.NEGATIVE_INFINITY;
        for (const row of batch) {
          const time = rowTime(row);
          if (Number.isFinite(time) && time > newest) newest = time;
          const key = dedupeKey(row);
          if (seen.has(key)) continue;
          seen.add(key);
          collected.push(row);
        }

        // There is deliberately no "the page was short, so we are done" test.
        // The row cap is applied before the category filter, so a request for
        // one category at a busy interchange returns a fraction of a page while
        // the stop has hours more to give. Counting rows would end the walk
        // there and truncate the board to whatever its first page reached.
        //
        // What ends the walk instead is time: an empty page, a page whose
        // newest row already reaches the horizon, or an offset that will not
        // advance. The cost of dropping the count test is one extra request
        // against a stop that really is exhausted, which its final page pays
        // for by leaving the offset where it was.
        if (batch.length === 0) break;
        if (!Number.isFinite(newest) || newest >= window.toMs) break;
        // Rounded down, not up. `offsetInMinutes` is a whole minute, and
        // rounding up starts the next page strictly after `newest`, discarding
        // whatever the row cap cut off later in that same minute. The dedupe
        // set is no help there: it removes rows that arrived twice, and cannot
        // conjure back a row the server was never asked for. Rounding down
        // re-requests the minute `newest` falls in, and the dedupe set absorbs
        // the overlap.
        const nextOffset = Math.floor((newest - now) / 60_000);
        if (nextOffset <= offset) break; // paging is not advancing; stop rather than spin
        offset = nextOffset;
      }

      const out: Departure[] = [];
      for (const row of collected) {
        const departure = toDeparture(row, stop);
        if (departure === null) continue;
        if (!keepSet.has(departure.mode)) continue;
        if (departure.realtime < window.fromMs || departure.realtime > window.toMs) continue;
        out.push(departure);
      }
      // Rows do not arrive in time order, so the sort is not cosmetic.
      out.sort((a, b) => a.realtime - b.realtime);
      return out;
    },

    async messages(): Promise<Message[]> {
      const rows = await get<unknown>('/messages');
      if (!Array.isArray(rows)) return [];
      return (rows as RawMessage[]).map((entry) => ({
        title: stripMarkup(String(entry.title ?? '')),
        text: stripMarkup(String(entry.text ?? entry.description ?? '')),
        // One notice often repeats a line once per affected direction or
        // section; the reader wants the set, not the multiset.
        lines: [
          ...new Set(
            (entry.lines ?? [])
              .map((line) => (typeof line === 'string' ? line : String(line?.label ?? '')))
              .map((label) => label.trim())
              .filter((label) => label.length > 0),
          ),
        ],
        validFrom: typeof entry.validFrom === 'number' ? entry.validFrom : null,
        validTo: typeof entry.validTo === 'number' ? entry.validTo : null,
        backend: MVG_BACKEND_NAME,
      }));
    },
  };
}
