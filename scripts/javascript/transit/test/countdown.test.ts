import { describe, expect, test } from 'bun:test';
import { formatCountdown } from '../src/page/dom.ts';

// The row's headline is the one cell whose width is decided by this function,
// so what it can produce is not a cosmetic question: a form nobody budgeted for
// is a number that paints across the cell beside it, silently, on a device
// nobody here is holding.
describe('the countdown headline', () => {
  test('an hour and less is a plain minute count', () => {
    expect(formatCountdown(0)).toBe('0');
    expect(formatCountdown(9)).toBe('9');
    expect(formatCountdown(45)).toBe('45');
    expect(formatCountdown(59)).toBe('59');
    expect(formatCountdown(60)).toBe('60');
  });

  test('past an hour it is hours and minutes', () => {
    expect(formatCountdown(61)).toBe('1:01');
    expect(formatCountdown(119)).toBe('1:59');
    expect(formatCountdown(120)).toBe('2:00');
  });

  test('the far end of the longest horizon still fits five characters', () => {
    expect(formatCountdown(1439)).toBe('23:59');
    expect(formatCountdown(1439).length).toBe(5);
  });

  test('the minutes are always two digits, so the colon does not move', () => {
    for (const minutes of [61, 65, 70, 119, 121, 601]) {
      expect(formatCountdown(minutes)).toMatch(/^\d+:\d\d$/);
    }
  });
});
