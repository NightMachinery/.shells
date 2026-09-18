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
  destinations: string[] | null;
  walk_minutes: number;
  walk_minutes_by_stop: Record<string, number> | null;
  stop_labels: Record<string, string> | null;
  connection: ExportedConnection | null;
}

export interface ExportedProfile {
  key: string;
  title: string;
  boards: ExportedBoard[];
}

export interface ExportedConfig {
  schema_version: number;
  defaults: {
    horizon_minutes: number;
    backend: string;
    fallback: string | null;
    transport_types: Mode[];
    timezone: string;
    home: string | null;
  };
  backends: { mvg_base_url: string; transitous_base_url: string };
  profiles: ExportedProfile[];
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
