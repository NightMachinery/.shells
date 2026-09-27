import { describe, expect, test } from 'bun:test';
import { beginRun, describe as describeRun, endRun, lastRun, markRoutes, markRows, noteJourneys, noteSource } from '../src/page/timing.ts';

// The console line a reader reads when the page is slow. It has to separate the
// things that were once indistinguishable from a phone: a slow connection from
// a slow server, and a server whose journey searches failed from one that had
// no journeys to find.

function run(): string {
  beginRun('home');
  markRows();
  markRoutes(5, 2, 1);
  const original = console.info;
  console.info = () => {};
  try {
    endRun();
  } finally {
    console.info = original;
  }
  const last = lastRun();
  if (last === null) throw new Error('no run was recorded');
  return describeRun(last);
}

describe('the timing line', () => {
  test("says how much of a server answer's wait was the server's own", () => {
    noteSource('server', 4_000, { stale: true, serverMs: 20, wallMs: 1_800 });
    noteJourneys(null);
    expect(run()).toContain('via server, 4 s old, stale, server 20 of 1800 ms');
  });

  test("says where each board's journeys came from", () => {
    noteSource('server', 0, { serverMs: 3, wallMs: 90 });
    noteJourneys({ fromServer: 1, plannedHere: 1, missing: 1 });
    expect(run()).toContain('journeys 1 from server, 1 planned here, 1 missing');
  });

  test('a page doing its own work says so, and nothing about a server', () => {
    noteSource('server', 0);
    noteJourneys({ fromServer: 2, plannedHere: 0, missing: 0 });
    noteSource('direct', null);
    const line = run();
    expect(line).toContain('worked out here');
    expect(line).not.toContain('journeys');
  });
});
