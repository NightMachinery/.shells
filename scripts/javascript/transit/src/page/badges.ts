import { compact } from './dom.ts';

// Destination badges for the strip view.
//
// A strip is one line per (line, direction) and one cell per departure, and it
// is the view the page opens in because it answers "when is the next one, and
// how often" in a glance. Branching services broke that: when a line's runs
// terminate in different places, each time had the destination spelled out
// after it, the row wrapped, and the rhythm the strip exists to show was gone.
//
// A badge is two or three letters standing for one destination, defined once in
// a legend under the group header. It costs the reader one lookup the first time
// and nothing afterwards, and it keeps the row one line tall, which is the whole
// point of the view.

/**
 * Words that carry no information about *which* place this is.
 *
 * `bf` rather than `bahnhof` is the one worth explaining: `compact` has already
 * turned "Hauptbahnhof" into "Hbf" and "Marienplatz Bahnhof" into "Marienplatz Bf" by
 * time this runs, so dropping the standalone token removes the second and
 * leaves the first, which is what a reader means by both.
 */
const FILLER = new Set(['via', 'bahnhof', 'bf', 'und', 'am', 'an', 'der', 'die', 'das', '-', '–']);

/** How many words a badge is built from. */
const BADGE_WORDS = 3;

/** The words of a destination that say which place it is. */
export function significantWords(name: string): string[] {
  return compact(name)
    .split(/[\s,./()]+/u)
    .map((word) => word.replace(/^[^\p{L}\p{N}]+/gu, '').replace(/[^\p{L}\p{N}]+$/gu, ''))
    .filter((word) => word.length > 0 && !FILLER.has(word.toLowerCase()));
}

/** The first word shortened to `length`, capitalised, then the other initials. */
function badgeText(words: readonly string[], length: number): string {
  const first = words[0] ?? '';
  const head = length <= 1 ? first.slice(0, 1).toUpperCase() : first.slice(0, 1).toUpperCase() + first.slice(1, length).toLowerCase();
  const rest = words.slice(1, BADGE_WORDS).map((word) => word.slice(0, 1).toUpperCase());
  return `${head}${rest.join('')}`;
}

/** One destination's badge: what it says and where its colour sits on the wheel. */
export interface DestinationBadge {
  text: string;
  /** Degrees on the colour wheel; saturation and lightness belong to the theme. */
  hue: number;
}

/**
 * A badge per destination, unique within the board.
 *
 * Collisions are resolved by lengthening the *first* word rather than by adding
 * a digit, because the extra letters still say something: two services ending at
 * "Marienplatz" and at "Maxmonument" both start out as "M" and separate into
 * "Mar" and "Max", which is a reader recognising a place rather than memorising
 * an index. A pair that stays identical to the end of the first word falls back to
 * a digit, which is the honest answer when the names really do agree that far.
 *
 * The hue comes from the destination's alphabetical rank within the board, so it
 * is stable across refreshes and independent of which runs happen to be in the
 * window. Saturation, lightness and the text colour are the stylesheet's, per
 * colour scheme. A badge never borrows the line's own colour: the line badge
 * beside it already carries that, and two things in one colour saying different
 * things is worse than no colour at all.
 */
export function destinationBadges(names: readonly string[]): Map<string, DestinationBadge> {
  const unique = [...new Set(names)].sort((a, b) => a.localeCompare(b));
  const words = new Map(unique.map((name) => [name, significantWords(name)] as const));

  const lengths = new Map<string, number>(unique.map((name) => [name, 1]));
  for (let round = 0; round < 8; round += 1) {
    const byText = new Map<string, string[]>();
    for (const name of unique) {
      const text = badgeText(words.get(name) ?? [], lengths.get(name) ?? 1);
      const bucket = byText.get(text);
      if (bucket === undefined) byText.set(text, [name]);
      else bucket.push(name);
    }
    let changed = false;
    for (const [, clashing] of byText) {
      if (clashing.length < 2) continue;
      for (const name of clashing) {
        const first = (words.get(name) ?? [])[0] ?? '';
        const length = lengths.get(name) ?? 1;
        if (length >= first.length) continue;
        lengths.set(name, length + 1);
        changed = true;
      }
    }
    if (!changed) break;
  }

  const out = new Map<string, DestinationBadge>();
  const used = new Map<string, number>();
  unique.forEach((name, index) => {
    let text = badgeText(words.get(name) ?? [], lengths.get(name) ?? 1);
    const seen = used.get(text);
    if (seen !== undefined) {
      used.set(text, seen + 1);
      text = `${text}${seen + 1}`;
    } else {
      used.set(text, 1);
    }
    out.set(name, { text, hue: Math.round((index * 360) / Math.max(1, unique.length)) });
  });
  return out;
}
