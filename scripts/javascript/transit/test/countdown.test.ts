import { describe, expect, test } from 'bun:test';
import { countdownWidthClass, formatCountdown } from '../src/page/dom.ts';

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

// The column is budgeted for the common form and the rest are drawn smaller to
// fit it, so the pairing that matters is between what the formatter can emit
// and what the stylesheet knows how to shrink. Driven off the formatter rather
// than off a list of strings, because a list is what goes stale when the
// formatter changes.
describe('fitting a countdown to the column it has', () => {
  test('the common form is drawn at full size', () => {
    for (const minutes of [0, 1, 9, 10, 45, 60]) {
      expect(countdownWidthClass(formatCountdown(minutes))).toBeNull();
    }
  });

  test('every longer form names a step down', () => {
    // Every minute the longest horizon on offer can reach, plus the extension
    // past it, so nothing the board can draw is left without a class.
    for (let minutes = 61; minutes <= 26 * 60; minutes += 1) {
      const text = formatCountdown(minutes);
      if (text.length <= 2) throw new Error(`${minutes} formatted to "${text}", which the column is sized for but should not be`);
      expect(countdownWidthClass(text)).not.toBeNull();
    }
  });

  test('the five character form gets the smaller of the two steps', () => {
    expect(countdownWidthClass(formatCountdown(61))).toBe('minutes-long');
    expect(countdownWidthClass(formatCountdown(9 * 60 + 59))).toBe('minutes-long');
    expect(countdownWidthClass(formatCountdown(10 * 60))).toBe('minutes-longest');
    expect(countdownWidthClass(formatCountdown(26 * 60))).toBe('minutes-longest');
  });
});
