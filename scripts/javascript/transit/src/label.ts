// Deciding that two feeds are naming the same thing.
//
// Two timetables of one railway disagree about spelling far more often than
// they disagree about facts. One writes a line as "RE 89" and the other as
// "RB89"; one signs a train for the town it terminates in and the other for the
// junction where the portion it is carrying splits off; one writes a platform
// as "6" and the other as "86". Any code that has to recognise one vehicle in
// two feeds needs to fold those spellings, and more than one place needs it:
// the journey planner matching an itinerary leg to a board row, and the via
// filter matching a board row to the aggregator's view of the same run.

/**
 * Whether two feeds are naming the same platform.
 *
 * Deliberately strict. The spellings that differ between feeds at the same
 * station differ by more than punctuation, so a loose comparison here would
 * pair two trains standing on genuinely different platforms, and that is the
 * mistake this is used to avoid rather than to make.
 */
export function samePlatformLabel(a: string, b: string): boolean {
  return a.trim().toLowerCase() === b.trim().toLowerCase();
}

/**
 * Whether two feeds are naming the same place as a destination.
 *
 * Folded rather than compared, because the two spell a terminus differently
 * often enough that equality would answer no to trains that are plainly the
 * same: punctuation, the bracketed district a long-distance feed likes to add,
 * and the spacing around it.
 */
export function sameDestinationLabel(destination: string, headsign: string | null): boolean {
  if (headsign === null) return false;
  const fold = (value: string): string =>
    value
      .toLowerCase()
      .replace(/\(.*?\)/g, ' ')
      .replace(/[^a-z0-9äöüß]+/g, ' ')
      .trim();
  const a = fold(destination);
  const b = fold(headsign);
  if (a.length === 0 || b.length === 0) return false;
  return a === b || a.startsWith(b) || b.startsWith(a);
}
