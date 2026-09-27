// Where the page's data comes from, and what happens when that changes.
//
// There are two answers. The DIRECT source is the page doing the work itself:
// a request per stop to the realtime backend, a journey search per commute
// board, all of it from the phone. It is what this page has always done, it
// needs nothing but the two public APIs, and it is what the tailnet copy and a
// page opened off a memory stick still do.
//
// The SERVER source is the same package doing the same work once, on the
// machine that hosts the page, with a warm cache. The phone then makes one
// request for the boards and one for the journeys, and gets answers that are
// usually already computed. On a slow connection that is the difference
// between dozens of round trips and two.
//
// The page does not choose between them at build time, because the same bytes
// are served from both the mirror, which has the server, and the tailnet, which
// does not. It probes once at startup, uses the server if there is one, and
// falls back to doing the work itself the moment the server stops answering.
// Falling back is not an error state: it is the page working the way it always
// did, and nothing on screen changes except the provenance line.

import { fetchMessages as fetchDirectMessages, fetchProfile as fetchDirect, prepareCalls } from './data.ts';
import type { FetchProfileOptions, FetchProfileResult } from './data.ts';
import { carryBoards, planKey, planProfile as planDirect } from './commute.ts';
import type { BoardResult, PlanProfileOptions, ProfileRoutes } from './commute.ts';
import {
  boardsSearch,
  decodeRoutes,
  profileSearch,
  WIRE_VERSION,
  type ProfileQuery,
  type WireBoardsAnswer,
  type WireMessages,
  type WireProfileAnswer,
  type WireTranslationPut,
  type WireTranslations,
  type WireTranslationsAnswer,
} from './wire.ts';
import type { Message } from '../model.ts';
import type { ExportedConfig } from './types.ts';
import type { JourneyNote } from './timing.ts';

export interface SourceFetchResult extends FetchProfileResult {
  /**
   * True when the server handed over an answer past its freshness and is
   * fetching a new one behind it. The page asks once more a moment later, by
   * which time the new one is usually there. Never set by the direct source.
   */
  stale?: boolean;
}

/**
 * The shared translation store, which is the one thing here that is a WRITE.
 *
 * Every browser reading this page translates the same dozen notices, and the
 * ones using the paid provider pay for it each time. The planning server keeps
 * what any of them produced and hands it to the next, so the work happens once.
 * It does not translate: it holds no key and never will, and the key it would
 * need lives in the reader's browser by design.
 *
 * Both calls are best effort by construction. With no server there is nothing
 * to ask and nowhere to offer, which is what the tailnet copy of this page does
 * all day, and the panel behaves exactly as it did before any of this existed.
 */
export interface TranslationShare {
  /** What the store already has, in this language, for these hashes. */
  fetchTranslations(lang: string, hashes: string[]): Promise<WireTranslations>;
  /**
   * Offer one translation for the next reader.
   *
   * Resolves whether the store is still taking offers: false once it has
   * refused one (a 4xx, a rate limit included), which tells the caller to stop
   * offering for the rest of the session rather than keep being refused.
   */
  shareTranslation(entry: WireTranslationPut): Promise<boolean>;
}

export interface DataSource extends TranslationShare {
  /** Which of the two this is, for the provenance line. */
  readonly kind: 'direct' | 'server';
  fetchProfile(options: FetchProfileOptions): Promise<SourceFetchResult>;
  planProfile(options: PlanProfileOptions): Promise<ProfileRoutes | null>;
  fetchMessages(config: ExportedConfig): Promise<Message[]>;
}

/** How many hashes one lookup may ask about in all, across its batches. */
const MAX_LOOKUP_HASHES = 200;

/**
 * How many hashes go in one request.
 *
 * The hashes travel in the query string, sixty-four characters and an escaped
 * comma each, and everything in front of the server adds its own headers and
 * cookies to the same request. Two hundred of them made a request line of
 * thirteen kilobytes, which with the access cookie on top was more than the
 * server's sixteen-kilobyte header limit, and it answered 431. Forty is under
 * three kilobytes, far from any limit anything in the chain is likely to have.
 */
const LOOKUP_BATCH = 40;

/** What the last answer cost and where it came from, for the timing line. */
export interface SourceNote {
  kind: 'direct' | 'server';
  /** How old the server's answer was when it was handed over, or null for direct. */
  ageMs: number | null;
  /** Whether the server handed it over past its freshness. */
  stale?: boolean;
  /** How long the server spent on the request, from its Server-Timing header. */
  serverMs?: number | null;
  /** How long the request took from here, the connection included. */
  wallMs?: number;
}

/**
 * One named duration out of a Server-Timing header, in whole milliseconds.
 *
 * The header is how the server says how much of a wait was its own, which is
 * the half of the question a phone cannot measure: two seconds to the first row
 * is a slow connection when the server took twenty milliseconds of it, and a
 * cold server when it took all of it.
 */
export function serverTimingMs(header: string | null, name: string): number | null {
  if (header === null) return null;
  for (const entry of header.split(',')) {
    const [metric, ...params] = entry.trim().split(';');
    if (metric?.trim() !== name) continue;
    for (const param of params) {
      const [key, value] = param.trim().split('=');
      if (key !== 'dur') continue;
      const ms = Number(value);
      return Number.isFinite(ms) ? Math.round(ms) : null;
    }
  }
  return null;
}

/**
 * The page doing its own work: the behaviour this page shipped with.
 *
 * It shares nothing and finds nothing, because there is nobody to share with:
 * this is the source a page opened off the tailnet, or off a memory stick, is
 * using, and it has no server at all.
 */
export const directSource: DataSource = {
  kind: 'direct',
  fetchProfile: (options) => fetchDirect(options),
  planProfile: (options) => planDirect(options),
  fetchMessages: (config) => fetchDirectMessages(config),
  fetchTranslations: async () => ({}),
  shareTranslation: async () => true,
};

/** Thrown when the server answered, but not with an answer. */
class ServerError extends Error {
  constructor(message: string, readonly status: number) {
    super(message);
    this.name = 'ServerError';
  }
}

interface Held {
  query: string;
  answer: WireProfileAnswer;
  ageMs: number;
  at: number;
}

/**
 * How long a held journeys answer may serve the same question again.
 *
 * A change of an early buffer or a walking weight re-plans at once, and the
 * page may ask the same question twice inside a second; past a few seconds it
 * is a different moment and is asked again.
 */
const HELD_MS = 5_000;

/** How old the server says an answer is, and whether it is past its freshness. */
function answerAge(response: Response): { ageMs: number; stale: boolean } {
  const ageHeader = response.headers.get('x-answer-age');
  const ageMs = ageHeader === null ? 0 : Math.max(0, Number(ageHeader) * 1000);
  return { ageMs: Number.isFinite(ageMs) ? ageMs : 0, stale: response.headers.get('x-answer-stale') === '1' };
}

export interface ServerSourceOptions {
  /** Where the API is, relative to the page. The mount, as the manifest spells it. */
  base?: string;
  /** Called with the age of each answer, so the timing line can say it. */
  onNote?: (note: SourceNote) => void;
  /** Told where each journeys answer came from, board by board. */
  onJourneys?: (note: JourneyNote) => void;
  fetchImpl?: typeof fetch;
  /**
   * How to plan, here, a board the server set out to plan and could not.
   * The page's own planner unless a test says otherwise; null plans nothing.
   */
  fallbackPlan?: ((options: PlanProfileOptions) => Promise<ProfileRoutes | null>) | null;
}

/**
 * The server doing the work.
 *
 * Journeys are merged onto the page's own previous answer here rather than
 * there, with the same rule the page uses for its own searches, because the
 * server has no idea what is on this phone's screen.
 */
export function createServerSource(options: ServerSourceOptions = {}): DataSource {
  const base = options.base ?? 'api';
  const call = options.fetchImpl ?? ((input: RequestInfo | URL, init?: RequestInit) => fetch(input, init));
  const fallbackPlan = options.fallbackPlan === undefined ? planDirect : options.fallbackPlan;
  let held: Held | null = null;

  async function ask(query: ProfileQuery): Promise<Held> {
    const search = profileSearch(query);
    const now = Date.now();
    if (held !== null && held.query === search && now - held.at < HELD_MS) return held;
    const response = await call(`${base}/profile/${encodeURIComponent(query.profileKey)}?${search}`, {
      headers: { Accept: 'application/json' },
    });
    if (!response.ok) throw new ServerError(`the planning server said ${response.status}`, response.status);
    const answer = (await response.json()) as WireProfileAnswer;
    if (answer.v !== WIRE_VERSION) throw new ServerError(`the planning server speaks version ${answer.v}`, 500);
    held = { query: search, answer, ageMs: answerAge(response).ageMs, at: Date.now() };
    return held;
  }

  /**
   * Plan here the boards the server set out to plan and could not.
   *
   * The server's searches can fail for reasons that have nothing to do with
   * this phone: the planner slowing one busy address down is the one that has
   * actually happened. The phone asks from its own address, so for the boards
   * that came back unanswered it does what it would have done with no server
   * at all, and only for those. A failure here leaves them unanswered, which
   * is what they were.
   */
  async function planUnanswered(planOptions: PlanProfileOptions, results: BoardResult[]): Promise<BoardResult[]> {
    const missing = results.filter((result) => result.routes === null).map((result) => result.index);
    if (missing.length === 0 || fallbackPlan === null) return results;
    let own: ProfileRoutes | null;
    try {
      own = await fallbackPlan({ ...planOptions, previous: undefined, onProgress: undefined, onlyIndexes: new Set(missing) });
    } catch {
      return results;
    }
    if (own === null) return results;
    const planned = own.boards;
    return results.map((result) => (result.routes !== null ? result : { index: result.index, routes: planned.get(result.index) ?? null }));
  }

  return {
    kind: 'server',
    async fetchProfile(fetchOptions) {
      // The boards alone, which the server has usually already fetched. The
      // journeys are a separate question, asked by `planProfile` once these are
      // on screen, so no row waits for a journey search.
      const search = boardsSearch({ horizonMinutes: fetchOptions.horizonMinutes, startMs: fetchOptions.startMs });
      const started = Date.now();
      const response = await call(`${base}/boards/${encodeURIComponent(fetchOptions.profile.key)}?${search}`, {
        headers: { Accept: 'application/json' },
      });
      if (!response.ok) throw new ServerError(`the planning server said ${response.status}`, response.status);
      const answer = (await response.json()) as WireBoardsAnswer;
      if (answer.v !== WIRE_VERSION) throw new ServerError(`the planning server speaks version ${answer.v}`, 500);
      const { ageMs, stale } = answerAge(response);
      options.onNote?.({
        kind: 'server',
        ageMs,
        stale,
        serverMs: serverTimingMs(response.headers.get('server-timing'), 'wait'),
        wallMs: Date.now() - started,
      });
      // Every board arrives at once, so the per-board progress the page draws
      // its skeletons from is reported as finished rather than as never having
      // happened: a board left `idle` reads as a board that is still loading.
      for (let index = 0; index < answer.boards.length; index += 1) fetchOptions.onStatus(index, { kind: 'ready' });
      // The server answered the boards; the sheets' onward calls are still the
      // browser's to look up, and they have to be told which window. See
      // `prepareCalls`.
      prepareCalls(fetchOptions);
      return { boards: answer.boards, backends: answer.backends, stale };
    },
    async planProfile(planOptions) {
      const previous = planOptions.previous;
      const key = planKey(planOptions);
      if (previous !== undefined && !previous.stale && previous.key === key) return previous;
      // A function rather than a read, because the signal is asked again after
      // each await and a narrowed property would say it could not have moved.
      const aborted = (): boolean => planOptions.signal?.aborted === true;
      const { answer } = await ask({
        profileKey: planOptions.profileKey,
        horizonMinutes: planOptions.horizonMinutes ?? 0,
        startMs: planOptions.startMs,
        destinationKey: planOptions.destinationKey,
        walkWeight: planOptions.walkWeight,
        earlyBufferMinutes: planOptions.earlyBufferMinutes,
      });
      if (aborted()) return previous ?? null;
      const decoded = decodeRoutes(answer.routes);
      const results = await planUnanswered(planOptions, decoded.results);
      if (aborted()) return previous ?? null;
      const fromServer = decoded.results.filter((result) => result.routes !== null).length;
      const missing = results.filter((result) => result.routes === null).length;
      options.onJourneys?.({ fromServer, plannedHere: results.length - fromServer - missing, missing });
      planOptions.onProgress?.(results.length, results.length);
      const at = Date.now();
      const boards = carryBoards(previous, decoded.wanted, results, at);
      if (boards.size === 0) return null;
      return { boards, at, destinationKey: planOptions.destinationKey ?? '', stale: false, key };
    },
    async fetchMessages(config) {
      void config;
      const response = await call(`${base}/messages`, { headers: { Accept: 'application/json' } });
      if (!response.ok) throw new ServerError(`the planning server said ${response.status}`, response.status);
      const answer = (await response.json()) as WireMessages;
      if (answer.v !== WIRE_VERSION) throw new ServerError(`the planning server speaks version ${answer.v}`, 500);
      return answer.messages;
    },
    async fetchTranslations(lang, wanted) {
      // Batched rather than one request per notice, because the point of this
      // is to save a phone round trips and a lookup per message would spend
      // more of them than translating locally ever cost. Batched rather than
      // all in one, because the hashes travel in the URL; see `LOOKUP_BATCH`.
      // The page asks only about the notices it shows, so this is nearly
      // always a single request.
      const hashes = wanted.slice(0, MAX_LOOKUP_HASHES);
      if (hashes.length === 0) return {};
      const batches: string[][] = [];
      for (let index = 0; index < hashes.length; index += LOOKUP_BATCH) batches.push(hashes.slice(index, index + LOOKUP_BATCH));
      const settled = await Promise.allSettled(
        batches.map(async (batch) => {
          const query = new URLSearchParams({ lang, hashes: batch.join(',') });
          const response = await call(`${base}/translations?${query.toString()}`, {
            headers: { Accept: 'application/json' },
          });
          if (!response.ok) throw new ServerError(`the planning server said ${response.status}`, response.status);
          const answer = (await response.json()) as WireTranslationsAnswer;
          // The envelope's other field says why an answer is thin, which is
          // worth nothing to this page: with or without a budget on the other
          // side, the hashes that did not come back are the ones to translate
          // here.
          return answer.translations ?? {};
        }),
      );
      // One batch failing costs only its own hashes. Only when every batch
      // failed is it the lookup that failed, and the caller hears about it.
      const found: WireTranslations = {};
      let failure: unknown = null;
      for (const outcome of settled) {
        if (outcome.status === 'fulfilled') Object.assign(found, outcome.value);
        else failure ??= outcome.reason;
      }
      if (failure !== null && settled.every((outcome) => outcome.status === 'rejected')) throw failure;
      return found;
    },
    async shareTranslation(entry) {
      const response = await call(`${base}/translations`, {
        method: 'PUT',
        headers: { 'content-type': 'application/json', Accept: 'application/json' },
        body: JSON.stringify(entry),
      });
      if (response.ok) return true;
      // A refusal, as distinct from a fault: the store looked at the offer and
      // said no, and the next offer will get the same answer. Offering on
      // regardless is how one page load once collected two hundred 429s.
      if (response.status >= 400 && response.status < 500) return false;
      throw new ServerError(`the planning server said ${response.status}`, response.status);
    },
  };
}

/**
 * The two sources, with the rule for moving between them.
 *
 * It starts on the direct source, because that one always works, and probes for
 * a server once. A probe that answers within its window moves the page over; a
 * request that then fails moves it straight back, answers the question the old
 * way, and tries the probe again later. The reader is never shown an error for
 * this: a page that has fallen back is a working page.
 */
export interface SwitchingSource extends DataSource {
  /** Ask now whether there is a server. Resolves to which source is in use after. */
  probe(): Promise<'direct' | 'server'>;
  /** Why the page is where it is, for the provenance line and for a test. */
  state(): { kind: 'direct' | 'server'; probedAt: number | null; fellBackAt: number | null; reason: string | null };
}

export interface SwitchingOptions {
  base?: string;
  /** How long a health probe may take before the page gives up on it. */
  probeMs?: number;
  /** How long after falling back to ask again whether the server is there. */
  reprobeMs?: number;
  onNote?: (note: SourceNote) => void;
  onJourneys?: (note: JourneyNote) => void;
  fetchImpl?: typeof fetch;
  now?: () => number;
}

declare global {
  interface Window {
    /** Which source the page settled on and why, for a test driving it from outside. */
    __transitSource?: () => { kind: string; probedAt: number | null; fellBackAt: number | null; reason: string | null };
  }
}

export function createSwitchingSource(options: SwitchingOptions = {}): SwitchingSource {
  const base = options.base ?? 'api';
  const probeMs = options.probeMs ?? 1_500;
  const reprobeMs = options.reprobeMs ?? 5 * 60_000;
  const now = options.now ?? (() => Date.now());
  const call = options.fetchImpl ?? ((input: RequestInfo | URL, init?: RequestInit) => fetch(input, init));
  const server = createServerSource({
    base,
    ...(options.onNote === undefined ? {} : { onNote: options.onNote }),
    ...(options.onJourneys === undefined ? {} : { onJourneys: options.onJourneys }),
    fetchImpl: call,
  });
  let kind: 'direct' | 'server' = 'direct';
  let probedAt: number | null = null;
  let fellBackAt: number | null = null;
  let reason: string | null = null;
  let probing: Promise<'direct' | 'server'> | null = null;

  async function probe(): Promise<'direct' | 'server'> {
    if (probing !== null) return probing;
    probing = (async () => {
      try {
        // A probe that has not answered in a moment is a probe whose answer is
        // no: the point of the server is speed, and a slow one is not it.
        const response = await call(`${base}/health`, {
          headers: { Accept: 'application/json' },
          signal: AbortSignal.timeout(probeMs),
        });
        probedAt = now();
        if (response.ok) {
          kind = 'server';
          reason = null;
        } else {
          kind = 'direct';
          fellBackAt = now();
          reason = `health said ${response.status}`;
        }
      } catch (error) {
        probedAt = now();
        kind = 'direct';
        fellBackAt = now();
        reason = error instanceof Error ? error.message : String(error);
      } finally {
        probing = null;
      }
      return kind;
    })();
    return probing;
  }

  /** Run against the server if there is one, and against ourselves if that fails. */
  async function through<T>(run: (source: DataSource) => Promise<T>): Promise<T> {
    if (kind === 'direct' && fellBackAt !== null && now() - fellBackAt > reprobeMs) void probe();
    if (kind === 'server') {
      try {
        return await run(server);
      } catch (error) {
        kind = 'direct';
        fellBackAt = now();
        reason = error instanceof Error ? error.message : String(error);
      }
    }
    options.onNote?.({ kind: 'direct', ageMs: null });
    return run(directSource);
  }

  /**
   * The translation store, asked only when there is a server, and never
   * through `through`.
   *
   * Deliberately not: `through` treats a failure as evidence that the server
   * has gone, and demotes the whole page to doing its own work for the next
   * five minutes. A translation lookup that 404s or a share that is rate
   * limited says nothing about whether the server can still plan a journey,
   * and letting it cost the page its boards would be a bad trade for a
   * feature whose entire value is saving somebody else a translation.
   */
  async function shared<T>(run: (source: DataSource) => Promise<T>, fallback: T): Promise<T> {
    if (kind !== 'server') return fallback;
    try {
      return await run(server);
    } catch {
      return fallback;
    }
  }

  const source: SwitchingSource = {
    get kind() {
      return kind;
    },
    probe,
    state: () => ({ kind, probedAt, fellBackAt, reason }),
    fetchProfile: (fetchOptions) => through((chosen) => chosen.fetchProfile(fetchOptions)),
    planProfile: (planOptions) => through((chosen) => chosen.planProfile(planOptions)),
    fetchMessages: (config) => through((chosen) => chosen.fetchMessages(config)),
    fetchTranslations: (lang, hashes) => shared((chosen) => chosen.fetchTranslations(lang, hashes), {}),
    // A server that has gone is not a refusal: there is nowhere to offer to
    // right now, and the offer after the next probe may well be taken.
    shareTranslation: (entry) => shared((chosen) => chosen.shareTranslation(entry), true),
  };
  if (typeof window !== 'undefined') window.__transitSource = source.state;
  return source;
}
