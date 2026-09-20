// Which platform a departure leaves from, and which one a strip group does.
//
// Three feeds know something about this and none of them knows it always. The
// primary feed publishes a track for most services and none at all for some
// categories at some stations; the aggregator publishes one for the same run at
// the same stop under its own identifier; and the run's own record names the
// track it uses at each call. A reader on a platform wants the number, and does
// not care which of the three said it.

import type { Departure } from '../model.ts';
import type { TripCall } from '../trip.ts';

/**
 * The platform to print for one departure, or nothing when none is published.
 *
 * In order of how much each source knows about THIS row. The row's own feed
 * comes first because it is the feed the row is from. The aggregator's matched
 * row is next: it is a different description of the same departure at the same
 * stop, borrowed in the data layer and carried on the row. The run's own record
 * is last, because it is about the vehicle rather than about this stop's
 * timetable, and it is only ever available once something has asked where the
 * run goes.
 */
export function platformOf(dep: Departure, call?: TripCall | null): string | null {
  if (dep.platform !== null && dep.platform.length > 0) return dep.platform;
  if (dep.platformGuess !== undefined && dep.platformGuess.length > 0) return dep.platformGuess;
  const fromRun = call?.platform ?? null;
  return fromRun !== null && fromRun.length > 0 ? fromRun : null;
}

/**
 * The platform a strip group uses, which is the one most of its times use.
 *
 * A group is one line in one direction from one stop, and such a group almost
 * always leaves from one platform. Almost: a diversion, a works closure or a
 * single late-evening run can move one departure, and that one departure is
 * exactly what a reader standing on the usual platform needs to be told about.
 * So the group carries the common platform once and only the exceptions are
 * marked, which is one badge instead of one per time for the ordinary case and
 * still says everything.
 *
 * Ties go to the earliest, because the times are in departure order and the
 * next one is the one a reader is about to take.
 */
export function groupPlatform(rows: readonly Departure[], calls?: (row: Departure) => TripCall | null): string | null {
  const counts = new Map<string, number>();
  for (const row of rows) {
    const platform = platformOf(row, calls?.(row));
    if (platform === null) continue;
    counts.set(platform, (counts.get(platform) ?? 0) + 1);
  }
  let best: string | null = null;
  let seen = 0;
  for (const [platform, count] of counts) {
    if (count > seen) {
      best = platform;
      seen = count;
    }
  }
  return best;
}
