import { describe, expect, test } from 'bun:test';
import {
  DEFAULT_TARGET_MAX_WALK_MINUTES,
  destinationLabel,
  metresBetween,
  planTargets,
  withinTargetWalk,
} from '../src/targets.ts';

const HOME = { name: 'home', label: null, lat: 48.1, lon: 11.5, stop: null };
const STATION = { name: 'station', label: 'Marienplatz', lat: null, lon: null, stop: 'de:00000:7' };

describe('planTargets', () => {
  test('a stop place is one target, reached on foot in no time at all', () => {
    expect(planTargets(STATION, [])).toEqual([{ place: { id: 'de:00000:7' }, name: 'Marienplatz', walkMinutes: 0 }]);
  });

  test('a doorstep is asked about together with every stop that serves it', () => {
    const targets = planTargets(HOME, [
      { stops: ['de:00000:1'], walkMinutes: 5 },
      { stops: ['de:00000:2', 'de:00000:3'], walkMinutes: 9, walkMinutesByStop: { 'de:00000:3': 14 } },
    ]);
    expect(targets).toEqual([
      { place: { lat: 48.1, lon: 11.5 }, name: 'home', walkMinutes: null },
      { place: { id: 'de:00000:1' }, name: 'home', walkMinutes: 5 },
      { place: { id: 'de:00000:2' }, name: 'home', walkMinutes: 9 },
      { place: { id: 'de:00000:3' }, name: 'home', walkMinutes: 14 },
    ]);
  });

  test('a stop two boards both name takes the shorter of the two walks', () => {
    const targets = planTargets(HOME, [
      { stops: ['de:00000:1'], walkMinutes: 12 },
      { stops: ['de:00000:1'], walkMinutes: 4 },
    ]);
    expect(targets).toHaveLength(2);
    expect(targets[1]?.walkMinutes).toBe(4);
  });

  test('a board that plans somewhere else says nothing about where this place is', () => {
    // The shape of the real fault: a board at a distant interchange, put in
    // this profile so its departures show up here, planning towards another
    // place entirely. Its stops are not three minutes from this doorstep.
    const targets = planTargets(HOME, [
      { stops: ['de:00000:1'], walkMinutes: 5 },
      { stops: ['de:00000:8'], walkMinutes: 3, destinationPlace: 'work' },
    ]);
    expect(targets.map((target) => ('id' in target.place ? target.place.id : 'doorstep'))).toEqual([
      'doorstep',
      'de:00000:1',
    ]);
  });

  test('a board that names this very place is still evidence of where it is', () => {
    const targets = planTargets(HOME, [{ stops: ['de:00000:1'], walkMinutes: 5, destinationPlace: 'home' }]);
    expect(targets).toHaveLength(2);
  });

  test('a place that declares neither is no target at all', () => {
    expect(planTargets({ name: 'nowhere', label: null, lat: null, lon: null, stop: null }, [])).toEqual([]);
  });

  test('the label is what it is called, and the key when it has none', () => {
    expect(destinationLabel(STATION)).toBe('Marienplatz');
    expect(destinationLabel(HOME)).toBe('home');
  });
});

/**
 * One degree of latitude is 111_320 metres, so these offsets are distances
 * written in the units the check actually compares.
 */
const PLACE = { lat: 48.1, lon: 11.5 };
const metresNorth = (metres: number) => ({ lat: PLACE.lat + metres / 111_320, lon: PLACE.lon });

describe('withinTargetWalk', () => {
  test('a stop three kilometres away is not a walk from the door', () => {
    const far = metresNorth(3000);
    expect(Math.round(metresBetween(PLACE, far))).toBeCloseTo(3000, -1);
    expect(withinTargetWalk(PLACE, far, DEFAULT_TARGET_MAX_WALK_MINUTES)).toBe(false);
  });

  test('a stop four hundred metres away is', () => {
    expect(withinTargetWalk(PLACE, metresNorth(400), DEFAULT_TARGET_MAX_WALK_MINUTES)).toBe(true);
  });
});
