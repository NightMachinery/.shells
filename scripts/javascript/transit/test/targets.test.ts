import { describe, expect, test } from 'bun:test';
import { destinationLabel, planTargets } from '../src/targets.ts';

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

  test('a place that declares neither is no target at all', () => {
    expect(planTargets({ name: 'nowhere', label: null, lat: null, lon: null, stop: null }, [])).toEqual([]);
  });

  test('the label is what it is called, and the key when it has none', () => {
    expect(destinationLabel(STATION)).toBe('Marienplatz');
    expect(destinationLabel(HOME)).toBe('home');
  });
});
