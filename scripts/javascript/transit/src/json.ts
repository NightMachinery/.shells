import type { Config } from './config.ts';
import { catchable, walkMinutesFor, type WalkSource } from './filter.ts';
import { SCHEMA_VERSION, type Board, type Departure } from './model.ts';

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
    stop: dep.stop,
    stop_tag: multiStop ? (dep.stopTag ?? stopTag(dep.stop, board.stopLabels)) : null,
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
      boards: profile.boards.map((board) => ({
        title: board.title,
        stops: board.stops,
        modes: board.modes ?? null,
        lines: board.lines ?? null,
        direction: board.direction ?? null,
        destinations: board.destinations ?? null,
        walk_minutes: board.walkMinutes,
        walk_minutes_by_stop: board.walkMinutesByStop ?? null,
        stop_labels: board.stopLabels ?? null,
      })),
    })),
  };
  if (options.withPlaces === true) {
    document.places = Object.values(config.places).map((place) => ({
      name: place.name,
      lat: place.lat,
      lon: place.lon,
    }));
  }
  return document;
}
