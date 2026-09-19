import { describe, expect, test } from 'bun:test';
import { countdownNeedsStep, exactCountdown, formatCountdown, HOURS_ONLY_FROM_MINUTES } from '../src/page/dom.ts';

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
    expect(formatCountdown(599)).toBe('9:59');
  });

  test('past ten hours the minutes go and the hours stay', () => {
    // Where the two forms meet, to the minute, because an off-by-one here is a
    // headline that changes shape an hour early or an hour late and nothing
    // else in the page would notice.
    expect(formatCountdown(HOURS_ONLY_FROM_MINUTES - 1)).toBe('9:59');
    expect(formatCountdown(HOURS_ONLY_FROM_MINUTES)).toBe('10h');
    expect(formatCountdown(600)).toBe('10h');
    expect(formatCountdown(1439)).toBe('23h');
  });

  test('nothing it can produce is wider than four characters', () => {
    // The column is budgeted for two and steps down once for the rest, and one
    // step only reaches so far. This is the property that keeps the two in
    // agreement, walked rather than sampled.
    let widest = '';
    for (let minutes = 0; minutes <= 26 * 60; minutes += 1) {
      const text = formatCountdown(minutes);
      if (text.length > widest.length) widest = text;
    }
    expect(widest.length).toBe(4);
  });

  test('the minutes are always two digits, so the colon does not move', () => {
    for (const minutes of [61, 65, 70, 119, 121, 599]) {
      expect(formatCountdown(minutes)).toMatch(/^\d+:\d\d$/);
    }
  });
});

// The headline rounds past ten hours and the sheet does not, so the two have to
// agree everywhere else; a second formatter that quietly disagreed at, say, the
// hour boundary would show a reader two different answers for one departure.
describe('the exact countdown behind the headline', () => {
  test('it keeps the minutes where the headline drops them', () => {
    expect(exactCountdown(600)).toBe('10:00');
    expect(exactCountdown(1439)).toBe('23:59');
  });

  test('below the rounding they are the same string', () => {
    for (let minutes = 0; minutes < HOURS_ONLY_FROM_MINUTES; minutes += 1) {
      expect(exactCountdown(minutes)).toBe(formatCountdown(minutes));
    }
  });
});

// The column is budgeted for the common form and the rest is drawn smaller to
// fit it, so the pairing that matters is between what the formatter can emit
// and what the stylesheet knows how to shrink. Driven off the formatter rather
// than off a list of strings, because a list is what goes stale when the
// formatter changes.
describe('fitting a countdown to the column it has', () => {
  test('the common form is drawn at full size', () => {
    for (const minutes of [0, 1, 9, 10, 45, 59, 60]) {
      expect(countdownNeedsStep(formatCountdown(minutes))).toBe(false);
    }
  });

  test('every longer form is stepped down', () => {
    // Every minute the longest horizon on offer can reach, plus the extension
    // past it, so nothing the board can draw is left at a size that does not
    // fit.
    for (let minutes = 61; minutes <= 26 * 60; minutes += 1) {
      const text = formatCountdown(minutes);
      if (text.length <= 2) throw new Error(`${minutes} formatted to "${text}", which the column is sized for but should not be`);
      expect(countdownNeedsStep(text)).toBe(true);
    }
  });

  test('both long shapes are stepped, the hour count as well as the pair', () => {
    expect(countdownNeedsStep(formatCountdown(61))).toBe(true);
    expect(countdownNeedsStep(formatCountdown(599))).toBe(true);
    expect(countdownNeedsStep(formatCountdown(600))).toBe(true);
    expect(countdownNeedsStep(formatCountdown(1439))).toBe(true);
  });
});
