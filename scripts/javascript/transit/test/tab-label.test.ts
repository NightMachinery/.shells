import { describe, expect, test } from 'bun:test';
import { tabLabel } from '../src/page/bar.ts';
import type { ExportedProfile } from '../src/page/types.ts';

function profile(extra: Partial<ExportedProfile>): ExportedProfile {
  return { key: 'h51', title: 'Home H51', boards: [], ...extra };
}

// The tab is the narrowest label on the page and the only one that has to hold
// four of itself on a phone. What it draws is therefore a function of how wide
// the screen is, and the rules are here rather than inline so they can be read
// without a browser.
describe('what a tab says', () => {
  test('a wide screen gets the full title, with the glyph in front', () => {
    expect(tabLabel(profile({ emoji: '🏠' }), false)).toBe('🏠 Home H51');
  });

  test('a narrow screen gets the short form, with the same glyph', () => {
    expect(tabLabel(profile({ emoji: '🏠', short: 'H51' }), true)).toBe('🏠 H51');
  });

  test('with no short form the key stands in, uppercased', () => {
    expect(tabLabel(profile({}), true)).toBe('H51');
    expect(tabLabel(profile({ key: 'work', title: 'Work' }), true)).toBe('WORK');
  });

  test('with no glyph the label is the label, and nothing is prefixed', () => {
    expect(tabLabel(profile({}), false)).toBe('Home H51');
    // An empty string is not a glyph, and must not leave a leading space that
    // would shift the label away from the tabs beside it.
    expect(tabLabel(profile({ emoji: '' }), false)).toBe('Home H51');
    expect(tabLabel(profile({ emoji: null }), false)).toBe('Home H51');
  });

  test('the short form is never used on a wide screen', () => {
    expect(tabLabel(profile({ short: 'H51' }), false)).toBe('Home H51');
  });
});
