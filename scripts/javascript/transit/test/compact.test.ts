import { describe, expect, test } from 'bun:test';
import { compact } from '../src/page/dom.ts';

// The abbreviations exist for slots too narrow to hold a full place name. The
// rule they have to obey is that they only ever remove characters a local
// reader supplies from memory, never characters that distinguish one place
// from another.

describe('compact place names', () => {
  test('shortens the station words, longest compound first', () => {
    expect(compact('Hauptbahnhof')).toBe('Hbf');
    expect(compact('Hauptbahnhof Nord')).toBe('Hbf Nord');
    expect(compact('Grafing-Bahnhof')).toBe('Grafing-Bf');
  });

  test('drops a leading city name in either of the forms it is written', () => {
    expect(compact('München, Somewhere')).toBe('Somewhere');
    expect(compact('München-Elsewhere')).toBe('Elsewhere');
  });

  test('leaves a name that distinguishes nothing else alone', () => {
    expect(compact('Somewhere (Something)')).toBe('Somewhere (Something)');
    expect(compact('Ostbahnhof')).toBe('Ostbahnhof');
  });

  test('is idempotent, so a name cannot be shortened twice into nonsense', () => {
    const once = compact('München, Hauptbahnhof');
    expect(once).toBe('Hbf');
    expect(compact(once)).toBe(once);
  });
});
