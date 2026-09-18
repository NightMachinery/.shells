import type { PlanTarget } from './plan.ts';

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
    for (const stop of board.stops) {
      const walk = board.walkMinutesByStop?.[stop] ?? board.walkMinutes;
      const known = walks.get(stop);
      if (known === undefined || walk < known) walks.set(stop, walk);
    }
  }
  for (const [stop, walk] of walks) targets.push({ place: { id: stop }, name, walkMinutes: walk });
  return targets;
}
