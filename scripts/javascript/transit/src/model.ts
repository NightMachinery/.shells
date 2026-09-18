// The domain vocabulary. Everything else in the package speaks these types:
// backends normalise into them, filters narrow them, the terminal formatter and
// the browser page render them, and `src/json.ts` serialises them.

/** Vehicle categories, spelled the way the primary backend spells them. */
export type Mode = 'SBAHN' | 'UBAHN' | 'TRAM' | 'BUS' | 'BAHN' | 'REGIONAL_BUS';

/** Every mode, in the order the CLI and the config example list them. */
export const ALL_MODES: readonly Mode[] = ['SBAHN', 'UBAHN', 'TRAM', 'BUS', 'BAHN', 'REGIONAL_BUS'];

export function isMode(value: unknown): value is Mode {
  return typeof value === 'string' && (ALL_MODES as readonly string[]).includes(value);
}

/**
 * Which way along a line a vehicle runs. The letter comes out of the primary
 * backend's line identifier and is the only trustworthy way to group a board by
 * direction: a single direction can carry several headsigns, so grouping by
 * destination splits one direction into two and merges nothing back.
 * `null` means the backend gave no usable letter.
 */
export type Direction = 'H' | 'R' | null;

/** One vehicle leaving one stop, normalised across backends. */
export interface Departure {
  /** Public line label, e.g. an S-Bahn or bus line as printed on the vehicle. */
  line: string;
  mode: Mode;
  destination: string;
  /** Scheduled departure, epoch milliseconds. */
  planned: number;
  /** Expected departure, epoch milliseconds; equals `planned` with no live data. */
  realtime: number;
  delayMin: number;
  cancelled: boolean;
  /** Replacement service (bus instead of rail, typically). */
  sev: boolean;
  platform: string | null;
  direction: Direction;
  /** Which backend produced this row. */
  backend: string;
  /** The stop id this row was fetched for. */
  stop: string;
  /** Short human tag for the stop, set only when a board merges several stops. */
  stopTag?: string;
  /**
   * The onward departure a rider leaving on this row could catch, when the
   * board declares a connection and one was found. Absent means no connection
   * is configured; null means one is and nothing was catchable in the window.
   */
  connection?: { line: string; departure: number } | null;
  /** False when only a scheduled time exists; the page draws a hollow dot. */
  realtimeKnown: boolean;
  color?: string | null;
}

/** A rendered board: one titled panel of one or more stops. */
export interface Board {
  title: string;
  stops: string[];
  /** The backend that answered, or `"mixed"` when a board's stops disagree. */
  backend: string;
  departures: Departure[];
  walkMinutes: number;
  /** Per-stop overrides of `walkMinutes`, keyed by stop id. */
  walkMinutesByStop?: Record<string, number>;
  /** Short human names for the stops, keyed by stop id. */
  stopLabels?: Record<string, string>;
  /** The onward service this board's rows point at, when one is configured. */
  connection?: ConnectionConfig;
  /** Whether this board takes part in the commute view. */
  commute?: boolean;
}

/** A board as the config declares it, before anything is fetched. */
export interface BoardConfig {
  title: string;
  stops: string[];
  modes?: Mode[];
  lines?: string[];
  direction?: Exclude<Direction, null>;
  /** Regular expression sources; a row passes when any of them matches. */
  destinations?: string[];
  /** Walking time to this board's stops, used when no per-stop value applies. */
  walkMinutes: number;
  /**
   * Per-stop walking times, keyed by stop id. A board that merges two stops of
   * the same area can have genuinely different walks to each, so a single
   * board-wide figure would dim the near stop's departures too early or leave
   * the far stop's looking reachable when they are not.
   */
  walkMinutesByStop?: Record<string, number>;
  /**
   * Short human names for the stops, keyed by stop id, for the tag a merged
   * board puts on each row. A stop id is unreadable and a stop's full name is
   * too long to repeat on every row, so this is the one place a person gets to
   * choose the three or four letters they will actually recognise. Absent, the
   * last field of the id is used, which is a number and tells you very little.
   */
  stopLabels?: Record<string, string>;
  /** An onward service to show alongside every row of this board. */
  connection?: ConnectionConfig;
  /**
   * Whether this board takes part in the commute view. A board opts in because
   * the journey planner is a much heavier question than a departure board, one
   * request per board per refresh against a free service, and most boards are
   * not journeys anyone plans: they are "is there a bus soon".
   */
  commute?: boolean;
}

/**
 * An onward service a rider changes to after this board's departure. The board
 * shows, on every row, the first departure at `stop` that a rider leaving on
 * that row could still catch.
 *
 * The two figures are static and that is a deliberate first cut: `rideMinutes`
 * is how long this board's vehicle takes to reach the interchange and
 * `transferMinutes` is the walk between platforms plus a little slack. A real
 * per-departure figure needs a trip lookup per row, which is a different order
 * of request volume against a free API, so it is left for later. The static
 * pair is right within a minute or two for a fixed pair of stops, which is
 * enough to answer "do I make the connection or do I wait".
 */
export interface ConnectionConfig {
  /** Stop id where the onward service is boarded. */
  stop: string;
  /** Line labels to keep at that stop. */
  lines: string[];
  /** Direction letter of the onward service, when it matters. */
  direction?: Exclude<Direction, null>;
  rideMinutes: number;
  transferMinutes: number;
}

export interface Profile {
  key: string;
  title: string;
  boards: BoardConfig[];
}

/** A stop as a search or nearby lookup returns it. */
export interface StopHit {
  id: string;
  name: string;
  /** Free-form locality string when the backend supplies one. */
  place?: string | null;
  /** Modes served, when the backend supplies them. */
  modes?: Mode[];
  backend: string;
}

/** A service disruption notice. */
export interface Message {
  title: string;
  text: string;
  /** Line labels the notice concerns, empty when it is network-wide. */
  lines: string[];
  /** Epoch milliseconds, when the backend dates the notice. */
  validFrom?: number | null;
  validTo?: number | null;
  backend: string;
}

/**
 * Version of the `--json` and `config-export` documents. Bump it whenever a
 * consumer that reads the old shape would misread the new one.
 */
export const SCHEMA_VERSION = 1;
