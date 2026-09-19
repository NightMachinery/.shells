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
// request and gets an answer that is usually already computed. On a slow
// connection that is the difference between a dozen round trips and one.
//
// The page does not choose between them at build time, because the same bytes
// are served from both the mirror, which has the server, and the tailnet, which
// does not. It probes once at startup, uses the server if there is one, and
// falls back to doing the work itself the moment the server stops answering.
// Falling back is not an error state: it is the page working the way it always
// did, and nothing on screen changes except the provenance line.

import { fetchMessages as fetchDirectMessages, fetchProfile as fetchDirect } from './data.ts';
import type { FetchProfileOptions, FetchProfileResult } from './data.ts';
import { carryBoards, planKey, planProfile as planDirect } from './commute.ts';
import type { PlanProfileOptions, ProfileRoutes } from './commute.ts';
import { decodeRoutes, profileSearch, WIRE_VERSION, type ProfileQuery, type WireMessages, type WireProfileAnswer } from './wire.ts';
import type { Message } from '../model.ts';
import type { ExportedConfig } from './types.ts';

/** The planning knobs a profile answer depends on that the boards do not. */
export interface PlanView {
  destinationKey: string | null;
  walkWeight?: number | undefined;
  earlyBufferMinutes?: number | undefined;
}

export interface SourceFetchOptions extends FetchProfileOptions {
  /**
   * What the journeys would be planned for, sent with the boards.
   *
   * The page asks for boards first and journeys second, which is right when it
   * is doing both itself. A server answers both in one request, so it has to be
   * told the second question while being asked the first one.
   */
  plan?: PlanView | undefined;
}

export interface DataSource {
  /** Which of the two this is, for the provenance line. */
  readonly kind: 'direct' | 'server';
  fetchProfile(options: SourceFetchOptions): Promise<FetchProfileResult>;
  planProfile(options: PlanProfileOptions): Promise<ProfileRoutes | null>;
  fetchMessages(config: ExportedConfig): Promise<Message[]>;
}

/** What the last answer cost and where it came from, for the timing line. */
export interface SourceNote {
  kind: 'direct' | 'server';
  /** How old the server's answer was when it was handed over, or null for direct. */
  ageMs: number | null;
}

/** The page doing its own work: the behaviour this page shipped with. */
export const directSource: DataSource = {
  kind: 'direct',
  fetchProfile: (options) => fetchDirect(options),
  planProfile: (options) => planDirect(options),
  fetchMessages: (config) => fetchDirectMessages(config),
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
 * How long a held answer may serve the plan half of the same refresh.
 *
 * The page asks for boards and then asks for journeys, a few milliseconds
 * apart, about the same instant. The server sent both in one answer, so the
 * second question is already answered; past a few seconds it is a different
 * question and is asked again.
 */
const HELD_MS = 5_000;

export interface ServerSourceOptions {
  /** Where the API is, relative to the page. The mount, as the manifest spells it. */
  base?: string;
  /** Called with the age of each answer, so the timing line can say it. */
  onNote?: (note: SourceNote) => void;
  fetchImpl?: typeof fetch;
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
    const ageHeader = response.headers.get('x-answer-age');
    const ageMs = ageHeader === null ? 0 : Math.max(0, Number(ageHeader) * 1000);
    held = { query: search, answer, ageMs: Number.isFinite(ageMs) ? ageMs : 0, at: Date.now() };
    options.onNote?.({ kind: 'server', ageMs: held.ageMs });
    return held;
  }

  return {
    kind: 'server',
    async fetchProfile(fetchOptions) {
      const view = fetchOptions.plan;
      const { answer } = await ask({
        profileKey: fetchOptions.profile.key,
        horizonMinutes: fetchOptions.horizonMinutes,
        startMs: fetchOptions.startMs,
        destinationKey: view?.destinationKey ?? null,
        walkWeight: view?.walkWeight,
        earlyBufferMinutes: view?.earlyBufferMinutes,
      });
      // Every board arrives at once, so the per-board progress the page draws
      // its skeletons from is reported as finished rather than as never having
      // happened: a board left `idle` reads as a board that is still loading.
      for (let index = 0; index < answer.boards.length; index += 1) fetchOptions.onStatus(index, { kind: 'ready' });
      return { boards: answer.boards, backends: answer.backends };
    },
    async planProfile(planOptions) {
      const previous = planOptions.previous;
      const key = planKey(planOptions);
      if (previous !== undefined && !previous.stale && previous.key === key) return previous;
      const { answer } = await ask({
        profileKey: planOptions.profileKey,
        horizonMinutes: planOptions.horizonMinutes ?? 0,
        startMs: planOptions.startMs,
        destinationKey: planOptions.destinationKey,
        walkWeight: planOptions.walkWeight,
        earlyBufferMinutes: planOptions.earlyBufferMinutes,
      });
      if (planOptions.signal?.aborted === true) return previous ?? null;
      const { results, wanted } = decodeRoutes(answer.routes);
      planOptions.onProgress?.(results.length, results.length);
      const at = Date.now();
      const boards = carryBoards(previous, wanted, results, at);
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

  const source: SwitchingSource = {
    get kind() {
      return kind;
    },
    probe,
    state: () => ({ kind, probedAt, fellBackAt, reason }),
    fetchProfile: (fetchOptions) => through((chosen) => chosen.fetchProfile(fetchOptions)),
    planProfile: (planOptions) => through((chosen) => chosen.planProfile(planOptions)),
    fetchMessages: (config) => through((chosen) => chosen.fetchMessages(config)),
  };
  if (typeof window !== 'undefined') window.__transitSource = source.state;
  return source;
}
