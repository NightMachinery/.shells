import type { Board, Departure, Message, Mode } from '../model.ts';

// The shapes the browser page reads and holds. Everything that crosses the wire
// from `config-export` is snake_case and lives in the `Exported*` types;
// everything the page computes for itself is camelCase, so a glance at a field
// name says which side of that boundary it came from.

export interface ExportedConnection {
  stop: string;
  lines: string[];
  direction: 'H' | 'R' | null;
  ride_minutes: number;
  transfer_minutes: number;
}

export interface ExportedBoard {
  title: string;
  stops: string[];
  modes: Mode[] | null;
  lines: string[] | null;
  direction: 'H' | 'R' | null;
  /** Stops a kept row's vehicle must still call at; see `BoardConfig`. */
  via: string[] | null;
  destinations: string[] | null;
  walk_minutes: number;
  walk_minutes_by_stop: Record<string, number> | null;
  stop_labels: Record<string, string> | null;
  commute: boolean;
  /** The place this board always plans towards, overriding the picker. */
  destination?: string | null;
  /** The identifier the planner is given for this board's stop, when it differs. */
  plan_stop?: string | null;
  connection: ExportedConnection | null;
}

/**
 * Somewhere a journey can end: a doorstep with coordinates, or a stop the
 * reader is travelling to. Exactly one of the two is set.
 */
export interface ExportedPlace {
  name: string;
  label: string | null;
  /** A single glyph drawn in front of it in the picker. */
  emoji?: string | null;
  lat: number | null;
  lon: number | null;
  stop: string | null;
}

export interface ExportedProfile {
  key: string;
  title: string;
  /** A single glyph drawn in front of the tab's label. */
  emoji?: string | null;
  /** What the tab says where the full title does not fit. */
  short?: string | null;
  /** Place keys this profile offers, in order; null for the default order. */
  destinations?: string[] | null;
  boards: ExportedBoard[];
}

export interface ExportedConfig {
  schema_version: number;
  defaults: {
    horizon_minutes: number;
    backend: string;
    fallback: string | null;
    transport_types: Mode[];
    /** Transit modes a journey plan may use, in the journey planner's vocabulary. */
    plan_modes?: string[];
    /** What a walked minute costs in ridden minutes when journeys are ranked. */
    walk_weight?: number;
    /**
     * How far a stop may be from the place it is offered as a way of reaching,
     * in minutes on foot. A guard against a mistyped walk in the configuration.
     */
    target_max_walk_minutes?: number;
    timezone: string;
    home: string | null;
  };
  backends: { mvg_base_url: string; transitous_base_url: string };
  profiles: ExportedProfile[];
  /** Absent unless the exporter was asked for coordinates. */
  places?: ExportedPlace[];
}

/**
 * How much of a board is on screen.
 *
 * `integrated` is the default and the reason the three-way choice exists at
 * all: a strip of times answers "when is the next one, and how often" in one
 * line per direction, which is what a glance at a phone is for. `full` is the
 * same data as one row per departure, for when the platform, the delay and the
 * onward connection matter. `collapsed` is for a board that is not relevant
 * today but would be annoying to delete from the configuration.
 */
export type BoardView = 'integrated' | 'full' | 'collapsed';

export const BOARD_VIEWS: readonly BoardView[] = ['integrated', 'full', 'collapsed'];

/** What a board is doing right now, for the skeleton and the error state. */
export type BoardStatus =
  | { kind: 'idle' }
  | { kind: 'loading'; backend: string | null; page: number }
  | { kind: 'ready' }
  | { kind: 'error'; detail: string };

/** One profile's fetched data, as held in memory and mirrored to IndexedDB. */
export interface ProfileData {
  boards: Board[];
  /** When the fetch that produced these boards finished, epoch milliseconds. */
  fetchedAtMs: number;
  /**
   * The start instant the boards were fetched for. A cached result is only
   * reusable when the reader is still asking about the same moment, so this is
   * stored beside the data rather than derived from it.
   */
  startMs: number;
  horizonMinutes: number;
}

/**
 * Whether the page is showing what is leaving now, or what is scheduled at a
 * moment the reader picked. The distinction runs all the way down: with a
 * picked start there is no live data to speak of, so every row is labelled as
 * a plan and the minute counts stop being counts of anything.
 */
export type StartMode = 'now' | 'picked';

export interface PageState {
  config: ExportedConfig | null;
  profileKey: string | null;
  /** Fetched data per profile key, including profiles not currently on screen. */
  data: Map<string, ProfileData>;
  /** Per-profile board status, keyed by profile key then board index. */
  status: Map<string, BoardStatus[]>;
  messages: Message[];
  /** How many fetches are in flight, which drives the refresh button's ring. */
  inFlight: number;
  startMode: StartMode;
  /** The picked start instant, meaningful only while `startMode` is `picked`. */
  startMs: number;
  horizonMinutes: number;
  /** Set when the most recent refresh of the visible profile failed outright. */
  lastError: string | null;
  /**
   * The place the commute view plans towards, named by the profile key whose
   * place it is. Null means the commute view is off, which is also what a
   * configuration with no places gets.
   */
  destinationKey: string | null;
  /** Whether a commute board orders its rows by arrival rather than departure. */
  sortByArrival: boolean;
  /**
   * How far before the feasible moment an onward departure may still be offered
   * as a tight option. Held in memory and deliberately not persisted: it is a
   * "show me what I would have to run for" knob for one look at one board, not
   * a standing preference, and a persisted one would quietly widen every plan
   * for weeks after the reader forgot they had touched it.
   */
  earlyBufferMinutes: number;
  /**
   * What one walked minute costs in ridden minutes when journeys are ranked.
   * Held in memory and not persisted, for the same reason as the tight window:
   * it is a "what if I would rather walk" question about one look at one board.
   */
  walkWeight: number;
  /**
   * Journeys per profile. Mirrored to IndexedDB, but a restored plan is always
   * marked stale and drawn dimmed with its age, because a stale arrival time
   * looks exactly like a fresh one and it is the number a reader acts on.
   */
  routes: Map<string, import('./commute.ts').ProfileRoutes>;
  /** How far the current planning run has got, per profile. */
  planning: Map<string, { done: number; total: number }>;
  /** How many planning runs are in flight, which the refresh ring counts too. */
  planInFlight: number;
}

/** A row as the renderer needs it: the departure plus what the board adds. */
export interface RowContext {
  dep: Departure;
  board: Board;
  now: number;
  timezone: string;
  /** True when the whole page is showing a picked moment rather than now. */
  planned: boolean;
}
