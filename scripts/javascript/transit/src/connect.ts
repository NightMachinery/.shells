import { normaliseLine } from './filter.ts';
import type { ConnectionConfig, Departure } from './model.ts';

// Onward connections. A board may declare one interchange it feeds into, and
// every row then carries the first onward departure a rider leaving on that row
// could still catch. That is the question a board cannot otherwise answer: two
// trams four minutes apart are interchangeable until you know that only one of
// them makes the next underground train.

/**
 * How much later than a row's own departure the onward service must leave for
 * the change to be possible: the time on board plus the walk between platforms.
 * Both come from the configuration and are static, so this is arithmetic rather
 * than a lookup.
 */
export function earliestOnward(dep: Departure, connection: ConnectionConfig): number {
  return dep.realtime + (connection.rideMinutes + connection.transferMinutes) * 60_000;
}

/** Keep only the onward rows the connection actually names. */
export function matchingOnward(rows: Departure[], connection: ConnectionConfig): Departure[] {
  const wanted = new Set(connection.lines.map(normaliseLine));
  return rows
    .filter((row) => wanted.has(normaliseLine(row.line)))
    .filter((row) => connection.direction === undefined || row.direction === connection.direction)
    .filter((row) => !row.cancelled)
    .sort((a, b) => a.realtime - b.realtime);
}

/**
 * Write the first catchable onward departure onto each row, in place.
 *
 * A row whose connection is `null` is not the same as a row with no connection
 * at all: null says the board has an interchange and nothing was leaving late
 * enough within the fetched window, which is worth showing as an empty slot
 * rather than silently omitting.
 */
export function attachConnections(rows: Departure[], onward: Departure[], connection: ConnectionConfig): void {
  const candidates = matchingOnward(onward, connection);
  for (const row of rows) {
    const earliest = earliestOnward(row, connection);
    const hit = candidates.find((candidate) => candidate.realtime >= earliest);
    row.connection = hit === undefined ? null : { line: hit.line, departure: hit.realtime };
  }
}
