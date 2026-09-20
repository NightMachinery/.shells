import { DEFAULT_TARGET_MAX_WALK_MINUTES } from './config.ts';
import { WALK_METRES_PER_MINUTE, type PlanTarget } from './plan.ts';

// Which places one journey plan is asked about.
//
// This is the smallest module in the package and it exists because the rule it
// carries is subtle enough that having two copies of it, one for the terminal
// and one for the browser, would guarantee the two drift apart. Both sides hand
// it the same two things in the same shape and get back the same list.

/** A board of the profile a journey is heading towards. */
export interface DestinationBoard {
  stops: readonly string[];
  walkMinutes: number;
  walkMinutesByStop?: Record<string, number> | null | undefined;
  /**
   * The place this board always plans towards, when it fixes one.
   *
   * Load-bearing here, and not obviously so. A board that fixes a destination
   * other than the place its own profile sits at is a board for LEAVING: it
   * exists to show what departs from somewhere on the way, and its stops say
   * nothing about where the profile is. Counting them as targets is how a
   * station on the far side of the city came to be three minutes from an
   * office, because the board that names it carries the walk from the OTHER
   * end of the journey it describes.
   */
  destinationPlace?: string | null | undefined;
}

/** Somewhere on the map, in the spelling every caller here already has. */
export interface Point {
  lat: number;
  lon: number;
}

// Re-exported so the rule and its bound are read from one module: the bound
// itself is a configuration default, so it is declared with the others.
export { DEFAULT_TARGET_MAX_WALK_MINUTES };

/** Great-circle metres between two points. */
export function metresBetween(a: Point, b: Point): number {
  const R = 6_371_000;
  const toRad = (degrees: number): number => (degrees * Math.PI) / 180;
  const dLat = toRad(b.lat - a.lat);
  const dLon = toRad(b.lon - a.lon);
  const lat1 = toRad(a.lat);
  const lat2 = toRad(b.lat);
  const h = Math.sin(dLat / 2) ** 2 + Math.cos(lat1) * Math.cos(lat2) * Math.sin(dLon / 2) ** 2;
  return 2 * R * Math.asin(Math.min(1, Math.sqrt(h)));
}

/**
 * Whether a stop is near enough to a place to be one of its targets.
 *
 * Straight-line, at the package's one walking pace, which understates a real
 * walk and is meant to: this rejects the impossible rather than judging the
 * plausible.
 */
export function withinTargetWalk(place: Point, stop: Point, maxWalkMinutes: number): boolean {
  return metresBetween(place, stop) <= maxWalkMinutes * WALK_METRES_PER_MINUTE;
}

/** A declared place, in the neutral spelling both callers can produce. */
export interface DestinationPlace {
  name: string;
  label: string | null;
  lat: number | null;
  lon: number | null;
  stop: string | null;
}

/** What a place is called on screen: its label, or the key it is declared under. */
export function destinationLabel(place: DestinationPlace): string {
  return place.label ?? place.name;
}

/**
 * Every place a plan towards `place` should be asked about.
 *
 * A stop place is one target and nothing else: the journey ends where the
 * vehicle does.
 *
 * A coordinate place is the coordinate *plus every stop of the profile that
 * lives there*, each with the walk the configuration records for it. That is
 * not redundancy. The planner answers with a Pareto set over arrival, changes
 * and departure, and a walk is in none of those, so an itinerary that arrives a
 * few minutes later at a stop six minutes from the door is dominated by one
 * that arrives sooner at a stop fourteen minutes away and is never returned at
 * all. Asking about the near stop by name is what makes that journey exist;
 * ranking it against the others is then `optionScore`'s job.
 *
 * A stop named by two boards with different walks takes the shorter one, which
 * is the true answer to "how far is that stop from here".
 */
export function planTargets(place: DestinationPlace, boards: readonly DestinationBoard[]): PlanTarget[] {
  const name = destinationLabel(place);
  if (place.stop !== null) return [{ place: { id: place.stop }, name, walkMinutes: 0 }];
  if (place.lat === null || place.lon === null) return [];

  const targets: PlanTarget[] = [{ place: { lat: place.lat, lon: place.lon }, name, walkMinutes: null }];
  const walks = new Map<string, number>();
  for (const board of boards) {
    // A board that plans somewhere else is a board for leaving, not evidence
    // of where this profile is. See `destinationPlace`.
    const fixed = board.destinationPlace ?? null;
    if (fixed !== null && fixed !== place.name) continue;
    for (const stop of board.stops) {
      const walk = board.walkMinutesByStop?.[stop] ?? board.walkMinutes;
      const known = walks.get(stop);
      if (known === undefined || walk < known) walks.set(stop, walk);
    }
  }
  for (const [stop, walk] of walks) targets.push({ place: { id: stop }, name, walkMinutes: walk });
  return targets;
}
