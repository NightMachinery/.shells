import generated from '../data/line-colors.json';
import type { Departure, Mode } from './model.ts';

// One source of line colour for both renderers. The terminal formatter turns
// these into 24-bit escape sequences and the browser page turns them into CSS
// custom properties, so a badge is the same colour in both places.

/** One line's artwork: a background, and a foreground when the operator names one. */
interface LineColor {
  bg: string;
  fg?: string;
}

interface ColorTable {
  source: string;
  feed_version: string;
  attribution: string;
  lines: Record<string, LineColor>;
}

/**
 * The operator's own colours, generated from its published GTFS feed by
 * `scripts/colors-refresh.ts` and committed as `data/line-colors.json`. Run
 * `bun run colors:refresh` to regenerate it.
 *
 * The cast is needed because TypeScript types an imported JSON document as the
 * literal shape of that one file, which cannot be indexed by an arbitrary line
 * label. The generator is the thing that guarantees the shape.
 */
const TABLE = generated as unknown as ColorTable;

/**
 * Background colour per line label, keyed by upper-case label with no
 * whitespace, which is the same normalisation the line filter uses.
 *
 * Derived from the generated table rather than hand-written. Kept as an export
 * because it is this module's long-standing surface; nothing else in the
 * package reads it today.
 */
export const LINE_COLORS: Readonly<Record<string, string>> = Object.fromEntries(
  Object.entries(TABLE.lines).map(([key, value]) => [key, value.bg]),
);

/** Fallback colour per vehicle category, for everything without its own. */
export const MODE_COLORS: Readonly<Record<Mode, string>> = {
  SBAHN: '#008D4F',
  UBAHN: '#0065AE',
  TRAM: '#D6001C',
  BUS: '#00586A',
  REGIONAL_BUS: '#4F6C8C',
  BAHN: '#5A5A5A',
};

function lineKey(label: string): string {
  return label.replace(/\s+/g, '').toUpperCase();
}

/**
 * Resolve the colour of one departure's badge: the generated official table if
 * it knows the line, else the colour the backend supplied, else the category
 * default.
 *
 * This order *inverts* the one this function used to apply, which let a
 * backend's colour win. The official table is the operator's own artwork and is
 * stable across feed releases; a backend's colour varies by feed and has been
 * seen to disagree with the printed colour for the same line. When the two
 * differ, the printed one is the one a rider recognises on a platform sign, so
 * it goes first.
 */
export function resolveColor(dep: Pick<Departure, 'line' | 'mode' | 'color'>): string {
  const official = TABLE.lines[lineKey(dep.line)];
  if (official !== undefined) return official.bg;
  if (typeof dep.color === 'string' && /^#[0-9a-fA-F]{6}$/.test(dep.color)) return dep.color;
  return MODE_COLORS[dep.mode];
}

/**
 * The foreground the operator prints on that badge, or null when it names none.
 *
 * Null is not "black": it means the table has no opinion, and the caller is
 * expected to fall back to `contrastText` of whatever background `resolveColor`
 * returned. Only the table can answer this, because a backend colour or a mode
 * default never carries a foreground of its own, and the table is consulted
 * first, so an entry's `fg` always belongs to the `bg` actually being drawn.
 */
export function resolveTextColor(dep: Pick<Departure, 'line' | 'mode' | 'color'>): string | null {
  return TABLE.lines[lineKey(dep.line)]?.fg ?? null;
}

/** Parse `#rrggbb` into its three channels. */
export function rgb(hex: string): [number, number, number] {
  const value = hex.replace('#', '');
  return [
    Number.parseInt(value.slice(0, 2), 16),
    Number.parseInt(value.slice(2, 4), 16),
    Number.parseInt(value.slice(4, 6), 16),
  ];
}

/** Black or white, whichever stays legible on the given background. */
export function contrastText(hex: string): string {
  const [r, g, b] = rgb(hex);
  // Rec. 601 luma, which is close enough for a two-way choice.
  const luma = (0.299 * r + 0.587 * g + 0.114 * b) / 255;
  return luma > 0.6 ? '#000000' : '#ffffff';
}

/**
 * The category colours as CSS custom property declarations, for the browser
 * page, which injects them at boot so the shell HTML and this module can never
 * drift apart.
 *
 * Per-*line* properties are deliberately not emitted. The generated table runs
 * to hundreds of lines, so one property each would be tens of kilobytes of CSS
 * built and parsed on every load, and nothing reads them: the page sets a
 * badge's colour from `resolveColor` on the element itself. The two remaining
 * readers of a line colour, the badge and the terminal formatter, both call
 * this module directly.
 */
export function cssCustomProperties(): string {
  const lines: string[] = [];
  for (const [mode, value] of Object.entries(MODE_COLORS)) {
    lines.push(`  --mode-${mode.toLowerCase()}: ${value};`);
  }
  return `:root {\n${lines.join('\n')}\n}\n`;
}
