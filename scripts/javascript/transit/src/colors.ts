import type { Departure, Mode } from './model.ts';

// One source of line colour for both renderers. The terminal formatter turns
// these into 24-bit escape sequences and the browser page turns them into CSS
// custom properties, so a badge is the same colour in both places.

/**
 * Official colours for the two rail networks whose lines are numbered and
 * colour-coded. Keys are upper-case line labels with no whitespace, which is
 * the same normalisation the line filter uses.
 *
 * Two of the rapid-transit lines are drawn as a pair of stripes in the official
 * artwork; a single colour is not expressible, so each takes one of its pair.
 * One suburban number is not currently in service and takes a neutral tone so
 * the table stays contiguous.
 */
export const LINE_COLORS: Readonly<Record<string, string>> = {
  S1: '#16BAE7',
  S2: '#76B82A',
  S3: '#951B81',
  S4: '#E30613',
  S5: '#8A8A8A',
  S6: '#00975F',
  S7: '#963833',
  S8: '#000000',
  U1: '#52822F',
  U2: '#C20831',
  U3: '#EC6725',
  U4: '#00A984',
  U5: '#BC7A00',
  U6: '#0065AE',
  U7: '#C20831',
  U8: '#EC6725',
};

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
 * Resolve the colour of one departure's badge: the colour the backend supplied
 * if it supplied one, else the static line colour, else the category default.
 */
export function resolveColor(dep: Pick<Departure, 'line' | 'mode' | 'color'>): string {
  if (typeof dep.color === 'string' && /^#[0-9a-fA-F]{6}$/.test(dep.color)) return dep.color;
  const byLine = LINE_COLORS[lineKey(dep.line)];
  if (byLine !== undefined) return byLine;
  return MODE_COLORS[dep.mode];
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

function cssName(key: string): string {
  return `--line-${key.toLowerCase()}`;
}

/**
 * The same table as CSS custom property declarations, for the browser page.
 * The page injects this at boot so the shell HTML and this module can never
 * drift apart.
 */
export function cssCustomProperties(): string {
  const lines: string[] = [];
  for (const [key, value] of Object.entries(LINE_COLORS)) {
    lines.push(`  ${cssName(key)}: ${value};`);
  }
  for (const [mode, value] of Object.entries(MODE_COLORS)) {
    lines.push(`  --mode-${mode.toLowerCase()}: ${value};`);
  }
  return `:root {\n${lines.join('\n')}\n}\n`;
}
