import { describe, expect, test } from 'bun:test';
import {
  LINE_COLORS,
  MODE_COLORS,
  contrastText,
  cssCustomProperties,
  resolveColor,
  resolveTextColor,
} from '../src/colors.ts';
import type { Departure, Mode } from '../src/model.ts';

/** The three fields both resolvers read, which is all a badge needs. */
function badge(overrides: Partial<Pick<Departure, 'line' | 'mode' | 'color'>> = {}): Pick<
  Departure,
  'line' | 'mode' | 'color'
> {
  return { line: 'S1', mode: 'SBAHN' as Mode, color: null, ...overrides };
}

/**
 * A label the generated table cannot know, used to exercise the two fallbacks.
 * Asserted absent rather than assumed so a future feed that adds it fails here
 * instead of quietly turning those tests into tests of the table.
 */
const UNKNOWN_LINE = 'ZZ99';

describe('resolution order', () => {
  test('the generated table outranks the colour a backend supplied', () => {
    expect(LINE_COLORS['S1']).toBeDefined();
    expect(resolveColor(badge({ line: 'S1', color: '#123456' }))).toBe(LINE_COLORS['S1']);
  });

  test('a backend colour outranks the category default', () => {
    expect(LINE_COLORS[UNKNOWN_LINE]).toBeUndefined();
    const row = badge({ line: UNKNOWN_LINE, mode: 'BUS' as Mode, color: '#123456' });
    expect(resolveColor(row)).toBe('#123456');
    expect(resolveColor(row)).not.toBe(MODE_COLORS.BUS);
  });

  test('the category default applies when neither the table nor the backend has one', () => {
    expect(resolveColor(badge({ line: UNKNOWN_LINE, mode: 'TRAM' as Mode }))).toBe(MODE_COLORS.TRAM);
  });

  test('a malformed backend colour is ignored rather than drawn', () => {
    const row = badge({ line: UNKNOWN_LINE, mode: 'BUS' as Mode, color: 'teal' });
    expect(resolveColor(row)).toBe(MODE_COLORS.BUS);
  });

  test('the table is keyed on the normalised label, so spacing and case do not matter', () => {
    expect(resolveColor(badge({ line: 'u 6', mode: 'UBAHN' as Mode }))).toBe(LINE_COLORS['U6']);
  });
});

describe('official colours', () => {
  // Hand-checked against the operators' own published artwork. The feed itself
  // carries one colour per agency, so these would otherwise all collapse onto
  // the suburban green and the underground's filler black; the refresh script
  // reapplies them, and this is what catches a regeneration that drops them.
  test('rapid transit lines carry their own colours, not one shared network colour', () => {
    expect(resolveColor(badge({ line: 'S1', mode: 'SBAHN' as Mode }))).toBe('#16BAE7');
    expect(resolveColor(badge({ line: 'S3', mode: 'SBAHN' as Mode }))).toBe('#951B81');
    expect(resolveColor(badge({ line: 'S8', mode: 'SBAHN' as Mode }))).toBe('#000000');
  });

  test('underground lines carry their own colours', () => {
    expect(resolveColor(badge({ line: 'U2', mode: 'UBAHN' as Mode }))).toBe('#C20831');
    expect(resolveColor(badge({ line: 'U6', mode: 'UBAHN' as Mode }))).toBe('#0065AE');
  });
});

describe('text colour', () => {
  test('null when the table names no foreground for the line', () => {
    expect(resolveTextColor(badge({ line: 'S1', mode: 'SBAHN' as Mode }))).toBeNull();
  });

  test('null when the table does not know the line at all', () => {
    expect(resolveTextColor(badge({ line: UNKNOWN_LINE, mode: 'BUS' as Mode, color: '#123456' }))).toBeNull();
  });

  test('the table wins over contrast when it names a foreground', () => {
    const row = badge({ line: 'RE72', mode: 'BAHN' as Mode });
    const named = resolveTextColor(row);
    expect(named).not.toBeNull();
    expect(named).toMatch(/^#[0-9A-Fa-f]{6}$/);
    // The caller only reaches contrastText on null, so the two may disagree and
    // the table still wins; this pins that the table did answer.
    expect(named ?? contrastText(resolveColor(row))).toBe(named);
  });
});

describe('css custom properties', () => {
  const css = cssCustomProperties();

  test('one property per vehicle category', () => {
    for (const mode of Object.keys(MODE_COLORS)) {
      expect(css).toContain(`--mode-${mode.toLowerCase()}:`);
    }
  });

  test('no per-line properties, however many lines the table holds', () => {
    expect(Object.keys(LINE_COLORS).length).toBeGreaterThan(100);
    expect(css).not.toContain('--line-');
  });
});
