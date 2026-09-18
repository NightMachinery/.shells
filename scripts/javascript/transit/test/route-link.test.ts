import { describe, expect, test } from 'bun:test';
import { decodeRoute, encodeRoute, handoffFor, routeUrl } from '../src/route-link.ts';
import type { RouteOption } from '../src/plan.ts';
import type { Departure } from '../src/model.ts';

const DAY = 24 * 60 * 60 * 1000;
const base = Date.parse('2024-03-05T22:00:00Z');

function option(index: number): RouteOption {
  return {
    exitStop: `de:00000:${index}`,
    exitStopName: 'Gießing – Süd (Straße)',
    destinationName: 'Zuhause',
    arrival: base + DAY + 9 * 60_000,
    transfers: 2,
    tight: index === 2,
    tightBy: index === 2 ? 2 : 0,
    walkMinutes: 13.5,
    legs: [
      { kind: 'transit', line: 'S8', mode: 'SBAHN', from: 'Süd', to: 'Gießing', departure: base, arrival: base + 600_000 },
      { kind: 'walk', line: '', mode: 'WALK', from: 'Gießing', to: 'Gießing Nord', departure: base + 600_000, arrival: base + 780_000 },
      { kind: 'transit', line: 'U4', mode: 'UBAHN', from: 'Gießing Nord', to: 'Zuhause', departure: base + 900_000, arrival: base + DAY },
    ],
  };
}

const departure: Departure = {
  line: 'S8',
  mode: 'SBAHN',
  destination: 'Flughafen München',
  planned: base,
  realtime: base + 120_000,
  delayMin: 2,
  cancelled: false,
  sev: false,
  platform: '2',
  direction: 'H',
  backend: 'mvg',
  stop: 'de:00000:1',
  realtimeKnown: true,
};

describe('route hand-off', () => {
  test('round-trips unicode names and next-day times', () => {
    const handoff = handoffFor({
      board: 'Straßenbahn 16 → Hbf',
      destination: 'Zuhause',
      destinationKey: 'home',
      timezone: 'Europe/Berlin',
      plannedAt: base,
      departure,
      from: 'Süd',
      options: [option(0)],
    });
    const back = decodeRoute(`#${encodeRoute(handoff)}`);
    expect(back).toEqual(handoff);
    expect(back?.options[0]?.exitStopName).toBe('Gießing – Süd (Straße)');
    expect(back?.options[0]?.arrival).toBe(base + DAY + 9 * 60_000);
  });

  test('a link for three options stays well under eight kilobytes', () => {
    const handoff = handoffFor({
      board: 'Straßenbahn 16 → Hbf',
      destination: 'Zuhause',
      destinationKey: 'home',
      timezone: 'Europe/Berlin',
      plannedAt: base,
      departure,
      from: 'Süd',
      options: [option(0), option(1), option(2)],
    });
    expect(routeUrl(handoff).length).toBeLessThan(8192);
  });

  test('refuses a fragment it cannot read rather than throwing', () => {
    expect(decodeRoute('')).toBeNull();
    expect(decodeRoute('#not-base64-$$$')).toBeNull();
    expect(decodeRoute(`#${encodeRoute({ ...handoffFor({ board: 'a', destination: 'b', destinationKey: 'b', timezone: 'UTC', plannedAt: 0, departure, from: 'c', options: [] }), v: 2 as unknown as 1 })}`)).toBeNull();
  });
});
