import type { Config } from './config.ts';
import { catchable, walkMinutesFor, type WalkSource } from './filter.ts';
import { SCHEMA_VERSION, type Board, type ConnectionConfig, type Departure } from './model.ts';

// The machine-readable surface of the package, in one file. Everything here is
// snake_case, because these documents are consumed by shell pipelines and by
// the browser page rather than by this codebase.

export function toIso(epochMs: number): string {
  return new Date(epochMs).toISOString();
}

/**
 * A short human tag for the stop a row came from, used only when a board merges
 * several stops and the reader needs to tell them apart. The last field of the
 * identifier is the stop's own number within its area, which is what a person
 * comparing two platforms of the same interchange actually wants to see.
 */
export function stopTag(stop: string, labels?: Record<string, string>): string {
  const label = labels?.[stop];
  if (label !== undefined && label.length > 0) return label;
  const fields = stop.split(':');
  const last = fields[fields.length - 1];
  return last !== undefined && last.length > 0 ? last : stop;
}

/** The connection a board declares, in the snake_case the page reads. */
export function connectionJson(connection: ConnectionConfig | undefined): unknown {
  if (connection === undefined) return null;
  return {
    stop: connection.stop,
    lines: connection.lines,
    direction: connection.direction ?? null,
    ride_minutes: connection.rideMinutes,
    transfer_minutes: connection.transferMinutes,
  };
}

export function departureJson(dep: Departure, board: WalkSource, now: number, multiStop: boolean): unknown {
  const walkMinutes = walkMinutesFor(board, dep.stop);
  return {
    line: dep.line,
    mode: dep.mode,
    destination: dep.destination,
    planned: toIso(dep.planned),
    realtime: toIso(dep.realtime),
    delay_min: dep.delayMin,
    cancelled: dep.cancelled,
    sev: dep.sev,
    platform: dep.platform,
    direction: dep.direction,
    catchable: catchable(dep, walkMinutes, now),
    // The walking time actually used for this row, which is the board's figure
    // unless this stop overrides it.
    walk_minutes: walkMinutes,
    backend: dep.backend,
    // Present only when a board filters by a place the vehicle must call at, and
    // only on the rows that could not be checked against it. A board that looks
    // too permissive is answered by this field and by nothing else.
    ...(dep.viaUnverified === true ? { via_unverified: true } : {}),
    ...(dep.tripId === undefined ? {} : { trip_id: dep.tripId }),
    stop: dep.stop,
    stop_tag: multiStop ? (dep.stopTag ?? stopTag(dep.stop, board.stopLabels)) : null,
    connection:
      dep.connection === undefined || dep.connection === null
        ? null
        : { line: dep.connection.line, departure: toIso(dep.connection.departure) },
  };
}

export function boardJson(board: Board, now: number): unknown {
  const multiStop = board.stops.length > 1;
  return {
    title: board.title,
    stops: board.stops,
    walk_minutes: board.walkMinutes,
    walk_minutes_by_stop: board.walkMinutesByStop ?? null,
    stop_labels: board.stopLabels ?? null,
    connection: connectionJson(board.connection),
    backend: board.backend,
    departures: board.departures.map((dep) => departureJson(dep, board, now, multiStop)),
  };
}

/**
 * The `--json` document produced by `board`.
 *
 * `profile` is the key that was actually rendered and `requested_profile` is
 * what the caller typed. They differ when an alias was followed, which is the
 * only way a consumer can tell that happened. Both are null for an ad-hoc board
 * built from raw stop ids, where no profile was involved at all.
 */
export function boardsDocument(
  profile: string | null,
  requestedProfile: string | null,
  boards: Board[],
  now: number,
): unknown {
  return {
    schema_version: SCHEMA_VERSION,
    profile,
    requested_profile: requestedProfile,
    generated_at: toIso(now),
    boards: boards.map((board) => boardJson(board, now)),
  };
}

export interface ConfigExportOptions {
  withPlaces?: boolean;
  mvgBaseUrl: string;
  transitousBaseUrl: string;
}

/**
 * The document the browser page loads as `data/config.json`.
 *
 * It deliberately carries no `generated_at`. The page repository refreshes this
 * file by re-running the exporter and committing the result; a timestamp would
 * make every run produce a different file and turn an unchanged config into a
 * commit. Byte-identical output for identical input is the point.
 *
 * Coordinates are omitted unless explicitly asked for. The page builds boards
 * from stop ids and needs no geography, and this file ends up in a repository.
 */
export function configExportDocument(config: Config, options: ConfigExportOptions): unknown {
  const document: Record<string, unknown> = {
    schema_version: SCHEMA_VERSION,
    defaults: {
      horizon_minutes: config.defaults.horizonMinutes,
      backend: config.defaults.backend,
      fallback: config.defaults.fallback,
      transport_types: config.defaults.transportTypes,
      plan_modes: config.defaults.planModes,
      walk_weight: config.defaults.walkWeight,
      timezone: config.defaults.timezone,
      // The profile key the `home` alias resolves to, so the page can follow
      // the same alias without parsing the TOML itself.
      home: config.defaults.home,
    },
    backends: {
      mvg_base_url: options.mvgBaseUrl,
      transitous_base_url: options.transitousBaseUrl,
    },
    profiles: config.profiles.map((profile) => ({
      key: profile.key,
      title: profile.title,
      destinations: profile.destinations ?? null,
      boards: profile.boards.map((board) => ({
        title: board.title,
        stops: board.stops,
        modes: board.modes ?? null,
        lines: board.lines ?? null,
        direction: board.direction ?? null,
        via: board.via ?? null,
        destinations: board.destinations ?? null,
        walk_minutes: board.walkMinutes,
        walk_minutes_by_stop: board.walkMinutesByStop ?? null,
        stop_labels: board.stopLabels ?? null,
        commute: board.commute === true,
        destination: board.destinationPlace ?? null,
        plan_stop: board.planStop ?? null,
        connection: connectionJson(board.connection),
      })),
    })),
  };
  // Coordinates are geography and are withheld unless asked for. A stop place
  // is not: it is a stop identifier, and this document is already a list of
  // stop identifiers, so withholding it would buy no privacy and would cost the
  // picker the destinations a reader can always reach.
  const places = Object.values(config.places).filter((place) => options.withPlaces === true || place.stop !== null);
  if (places.length > 0) {
    document.places = places.map((place) => ({
      name: place.name,
      label: place.label,
      lat: place.lat,
      lon: place.lon,
      stop: place.stop,
    }));
  }
  return document;
}
