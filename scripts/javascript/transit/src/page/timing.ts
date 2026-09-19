// How long a refresh took, and where it went.
//
// This exists because "is it slow" is not a question anybody can answer from a
// phone, and the numbers that were measured on the machine this was written on
// are the wrong numbers: a laptop on a wired connection is not a phone on a
// train. So the page measures itself, on the device it is running on, and says
// so somewhere a reader can look.
//
// It records what a reader would notice rather than what a profiler would
// report: how long until there were rows on screen, how long until the journey
// slots filled, and per board where that time went. Nothing here is sampled or
// averaged; it is the last refresh, because that is the one somebody is asking
// about.

/** One board's share of a refresh. */
export interface BoardTiming {
  title: string;
  /** Wall time in the departures fetch, milliseconds. */
  departuresMs: number;
  /** How many upstream pages that fetch cost, across every stop on the board. */
  pages: number;
  /** How many of this board's stop requests were answered by one already in flight. */
  shared: number;
}

/** One refresh, as a whole. */
export interface RunTiming {
  profileKey: string;
  startedAt: number;
  /** From the start of the refresh to rows being renderable. */
  toRowsMs: number | null;
  /** From the start of the refresh to the journey slots being filled. */
  toRoutesMs: number | null;
  /** Wall time inside the planner, and how many boards it planned. */
  planMs: number | null;
  planBoards: number;
  /** How many distinct plan targets the destination resolved to. */
  planTargets: number;
  /** Wall time in the last render pass. */
  renderMs: number | null;
  boards: BoardTiming[];
}

let current: RunTiming | null = null;
let last: RunTiming | null = null;

/** Start a new record. The previous one stays readable until this one finishes. */
export function beginRun(profileKey: string): void {
  current = {
    profileKey,
    startedAt: Date.now(),
    toRowsMs: null,
    toRoutesMs: null,
    planMs: null,
    planBoards: 0,
    planTargets: 0,
    renderMs: null,
    boards: [],
  };
}

/**
 * Add one board's numbers to the run in progress.
 *
 * The profile is named because a prefetch of a different profile finishes
 * inside the open run of the one on screen, and charging its boards to that run
 * makes the visible refresh look slower than it was. It is dropped instead: the
 * reader is not waiting for it.
 */
export function recordBoard(profileKey: string, entry: BoardTiming): void {
  if (current === null || current.profileKey !== profileKey) return;
  current.boards.push(entry);
}

/** Note that rows could be drawn, which is the moment the page stops being empty. */
export function markRows(): void {
  if (current !== null && current.toRowsMs === null) current.toRowsMs = Date.now() - current.startedAt;
}

/** Note that the journey slots are filled, with what the planner cost to do it. */
export function markRoutes(planMs: number, boards: number, targets: number): void {
  if (current === null) return;
  current.toRoutesMs = Date.now() - current.startedAt;
  current.planMs = planMs;
  current.planBoards = boards;
  current.planTargets = targets;
}

/** Note how long the last render pass took. Charged to the run in progress or the last one. */
export function markRender(ms: number): void {
  const target = current ?? last;
  if (target !== null) target.renderMs = Math.round(ms);
}

/**
 * Close the record and publish it.
 *
 * Logged as well as shown, because a reader who is willing to open a console is
 * usually the one asking why it is slow, and the console keeps every run rather
 * than only the last.
 */
export function endRun(): void {
  if (current === null) return;
  last = current;
  current = null;
  // eslint-disable-next-line no-console
  console.info(`[transit] ${describe(last)}`);
}

/** The last completed run, or null before the first one finishes. */
export function lastRun(): RunTiming | null {
  return last;
}

declare global {
  interface Window {
    /** The last run, for a test driving this page from outside it. */
    __transitTiming?: () => RunTiming | null;
  }
}

/** Expose the record to anything driving the page, which is how the harness reads it. */
export function publishTiming(): void {
  if (typeof window !== 'undefined') window.__transitTiming = lastRun;
}

/**
 * Where the last answer came from, and how old it was when it arrived.
 *
 * Worth a line in the timing note because it changes what every other number
 * in it means: two hundred milliseconds to rows is a fast connection when the
 * page did the work, and a warm cache when it did not.
 */
let sourceKind: 'direct' | 'server' = 'direct';
let sourceAgeMs: number | null = null;

export function noteSource(kind: 'direct' | 'server', ageMs: number | null): void {
  sourceKind = kind;
  sourceAgeMs = ageMs;
}

/** What the page would say about where its data came from. */
export function describeSource(): string {
  if (sourceKind === 'direct') return 'worked out here';
  const age = sourceAgeMs === null ? null : Math.round(sourceAgeMs / 1000);
  return age === null ? 'via server' : `via server, ${age} s old`;
}

/** One line per board plus a total, short enough for a popover. */
export function describe(run: RunTiming): string {
  const parts: string[] = [];
  parts.push(describeSource());
  parts.push(`rows ${run.toRowsMs ?? '-'} ms`);
  if (run.toRoutesMs !== null) parts.push(`routes ${run.toRoutesMs} ms`);
  if (run.planMs !== null) parts.push(`plan ${run.planMs} ms for ${run.planBoards} board${run.planBoards === 1 ? '' : 's'}`);
  if (run.renderMs !== null) parts.push(`render ${run.renderMs} ms`);
  const pages = run.boards.reduce((sum, board) => sum + board.pages, 0);
  const shared = run.boards.reduce((sum, board) => sum + board.shared, 0);
  parts.push(`${pages} page${pages === 1 ? '' : 's'}`);
  if (shared > 0) parts.push(`${shared} shared`);
  return parts.join(', ');
}

/** The per-board detail, for the console and for anyone reading the popover twice. */
export function describeBoards(run: RunTiming): string[] {
  return run.boards.map((board) => `${board.title}: ${board.departuresMs} ms, ${board.pages} page${board.pages === 1 ? '' : 's'}`);
}

/**
 * Journey searches, counted.
 *
 * A search takes seconds and the page refreshes every half minute, so "how
 * many were running at once" is a correctness question rather than a
 * curiosity: two runs writing the same slots is how a journey that is already
 * on screen gets replaced by a blank. The page runs one search per profile and
 * remembers a refresh that lands mid-flight instead of starting a second one,
 * and these counters are how that rule is observed from outside the page.
 */
export interface PlanRunStats {
  /** Searches begun since the page loaded. */
  started: number;
  /** Running right now, across every profile. */
  inFlight: number;
  /** The worst overlap seen across every profile at once. */
  peak: number;
  /** The worst overlap seen within one profile, which is the rule that matters. */
  peakByProfile: Record<string, number>;
  /** Refreshes that landed mid-search and were folded into one follow-up. */
  queued: number;
  /** Searches cancelled because the reader had asked a different question. */
  aborted: number;
}

const planRuns = { started: 0, inFlight: 0, peak: 0, queued: 0, aborted: 0 };
const planInFlightByProfile = new Map<string, number>();
const planPeakByProfile = new Map<string, number>();

/** A search is starting for this profile. */
export function notePlanStart(profileKey: string): void {
  planRuns.started += 1;
  planRuns.inFlight += 1;
  if (planRuns.inFlight > planRuns.peak) planRuns.peak = planRuns.inFlight;
  const mine = (planInFlightByProfile.get(profileKey) ?? 0) + 1;
  planInFlightByProfile.set(profileKey, mine);
  if (mine > (planPeakByProfile.get(profileKey) ?? 0)) planPeakByProfile.set(profileKey, mine);
}

/** That search has finished, whether it answered or threw. */
export function notePlanEnd(profileKey: string): void {
  planRuns.inFlight = Math.max(0, planRuns.inFlight - 1);
  planInFlightByProfile.set(profileKey, Math.max(0, (planInFlightByProfile.get(profileKey) ?? 1) - 1));
}

/** A refresh arrived while a search was running and will be answered once, at the end. */
export function notePlanQueued(): void {
  planRuns.queued += 1;
}

/** A search was cancelled because its question is no longer the one being asked. */
export function notePlanAborted(): void {
  planRuns.aborted += 1;
}

/** The counters as they stand. */
export function planRunStats(): PlanRunStats {
  return { ...planRuns, peakByProfile: Object.fromEntries(planPeakByProfile) };
}

declare global {
  interface Window {
    /** Search accounting, for a test driving this page from outside it. */
    __transitPlanRuns?: () => PlanRunStats;
  }
}

/** Expose the search accounting the same way the timing record is exposed. */
export function publishPlanRunStats(): void {
  if (typeof window !== 'undefined') window.__transitPlanRuns = planRunStats;
}
