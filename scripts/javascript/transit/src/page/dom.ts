// Small DOM helpers, shared by every renderer on the page.
//
// The page never assigns markup: no `innerHTML`, no `insertAdjacentHTML`.
// Upstream service messages arrive as HTML fragments and destinations are
// operator-supplied strings, so building nodes and setting `textContent` is the
// only construction rule that cannot be turned into an injection by a feed.

export function el(tag: string, className?: string, text?: string): HTMLElement {
  const node = document.createElement(tag);
  if (className !== undefined) node.className = className;
  if (text !== undefined) node.textContent = text;
  return node;
}

/**
 * A button, because every interactive control on this page is one.
 *
 * Buttons rather than clickable `div`s throughout: the page is used one-handed
 * on a phone, where the browser's own tap target sizing and focus ring are
 * worth more than the styling freedom, and a real button is reachable by
 * keyboard on the desktop without any extra work.
 */
export function button(className: string, text?: string, title?: string): HTMLButtonElement {
  const node = document.createElement('button');
  node.type = 'button';
  node.className = className;
  if (text !== undefined) node.textContent = text;
  if (title !== undefined) node.title = title;
  return node;
}

/** An empty grid cell, so a slot that has no value still holds its column. */
export function slot(className: string): HTMLElement {
  const node = el('span', `${className} slot-empty`);
  node.setAttribute('aria-hidden', 'true');
  return node;
}

export function clockTime(epochMs: number, timezone: string): string {
  return new Intl.DateTimeFormat('en-GB', {
    hour: '2-digit',
    minute: '2-digit',
    hour12: false,
    timeZone: timezone,
  }).format(new Date(epochMs));
}

/** The calendar date in the configured zone, as `YYYY-MM-DD`. */
function isoDate(epochMs: number, timezone: string): string {
  return new Intl.DateTimeFormat('en-CA', {
    year: 'numeric',
    month: '2-digit',
    day: '2-digit',
    timeZone: timezone,
  }).format(new Date(epochMs));
}

/**
 * How many calendar days after the reference instant a departure falls.
 *
 * A board with a long horizon runs past midnight, and `00:14` sorted below
 * `23:52` reads as an error until you notice the day changed. The marker is
 * computed from calendar dates in the configured zone rather than from a
 * millisecond difference, because the question is whether the date rolled over,
 * not whether twenty-four hours have passed.
 */
export function dayOffset(epochMs: number, referenceMs: number, timezone: string): number {
  const a = isoDate(referenceMs, timezone);
  const b = isoDate(epochMs, timezone);
  if (a === b) return 0;
  return Math.round((Date.parse(`${b}T12:00:00Z`) - Date.parse(`${a}T12:00:00Z`)) / 86_400_000);
}

/** A `+1` marker for a departure that falls on a later calendar day. */
export function dayMarker(epochMs: number, referenceMs: number, timezone: string): HTMLElement | null {
  const offset = dayOffset(epochMs, referenceMs, timezone);
  if (offset <= 0) return null;
  const node = el('sup', 'day-marker', `+${offset}`);
  node.title = offset === 1 ? 'the next day' : `${offset} days later`;
  return node;
}

/**
 * A clock time with its day marker: the one way this page draws an instant.
 *
 * One function rather than a convention, because a convention does not hold. The
 * `+1` was a real superscript element in the rows and a `+1` glued onto a string
 * in the sticky bar, and the two looked different on the same screen for as long
 * as nobody compared them. Anything that draws a time calls this; anything that
 * needs a time inside a longer sentence calls `timeLabel`, which is the same
 * answer in plain text.
 */
export function timeNode(epochMs: number, referenceMs: number, timezone: string, className = 'clock'): HTMLElement {
  const node = el('span', className, clockTime(epochMs, timezone));
  const marker = dayMarker(epochMs, referenceMs, timezone);
  if (marker !== null) node.append(marker);
  return node;
}

/** The same instant as plain text, for a tooltip, a title or an aria-label. */
export function timeLabel(epochMs: number, referenceMs: number, timezone: string): string {
  const offset = dayOffset(epochMs, referenceMs, timezone);
  return `${clockTime(epochMs, timezone)}${offset > 0 ? `+${offset}` : ''}`;
}

/** Whole minutes from now until an instant, floored at zero. */
export function minutesUntil(epochMs: number, now: number): number {
  return Math.max(0, Math.floor((epochMs - now) / 60_000));
}

/**
 * The countdown as the row's headline draws it.
 *
 * Minutes up to an hour, and hours and minutes past that. The longer horizons
 * run to a whole day, and "1438" is four digits that nobody reads as a length of
 * time: the question a reader is asking at that range is "how long", and the
 * answer to that is an hour count with the minutes still attached rather than a
 * number they have to divide. The column is right-aligned, so a "9", a "45" and
 * a "1:23" end under each other down the board.
 *
 * The widest this can be is five characters, at the far end of the longest
 * horizon on offer, and the narrowest is one. The column is budgeted for the
 * narrow end, because that is what almost every row is; the long forms are
 * drawn smaller to fit it. See `fitCountdown`, which is what puts them there.
 */
export function countdownLabel(epochMs: number, now: number): string {
  return formatCountdown(minutesUntil(epochMs, now));
}

/** The same, from a plain minute count, which is the part worth testing. */
export function formatCountdown(minutes: number): string {
  if (minutes <= 60) return String(minutes);
  const hours = Math.floor(minutes / 60);
  return `${hours}:${String(minutes - hours * 60).padStart(2, '0')}`;
}

/**
 * Put a countdown into its node, at a size that fits the column.
 *
 * The column is budgeted for the common form, which is one or two digits, so
 * the forms that do not fit carry a class that steps the size down until they
 * do. Which class is a question about the string's length and not about its
 * punctuation: a colon is what makes the long forms long today, but the reason
 * to shrink is that the string is wide, and a rule written about the colon
 * would quietly stop covering any other long form that ever appears.
 *
 * The text node is patched rather than replaced when there is one to patch.
 * This runs once a second for every row on the page, and replacing the node
 * throws away anything the browser had attached to it.
 */
export function fitCountdown(node: HTMLElement, text: string): void {
  const only = node.childNodes.length === 1 ? node.firstChild : null;
  if (only !== null && only.nodeType === Node.TEXT_NODE) only.nodeValue = text;
  else node.textContent = text;
  const wanted = countdownWidthClass(text);
  node.classList.toggle('minutes-long', wanted === 'minutes-long');
  node.classList.toggle('minutes-longest', wanted === 'minutes-longest');
}

/**
 * Which step down a countdown of this length needs, or null for none.
 *
 * Separated from the node so it can be checked against every form the
 * formatter can actually produce, which is the pairing that matters: the
 * column is budgeted for two characters, so anything the formatter can emit
 * that is longer has to name a class, and a form that names none is a form
 * that paints over the badge beside it.
 */
export function countdownWidthClass(text: string): 'minutes-long' | 'minutes-longest' | null {
  if (text.length >= 5) return 'minutes-longest';
  if (text.length >= 3) return 'minutes-long';
  return null;
}

/**
 * Whether the reader is in the middle of selecting text inside a board.
 *
 * The page repaints once a second to keep the minute counts honest, and a
 * repaint destroys a selection in progress. Copying a destination or a time off
 * the page is a thing people do, so the tick skips its work entirely while a
 * selection is live rather than trying to restore one afterwards, which cannot
 * be done faithfully across replaced nodes.
 */
export function selectionInsideBoards(): boolean {
  const selection = document.getSelection();
  if (selection === null || selection.isCollapsed) return false;
  const anchor = selection.anchorNode;
  if (anchor === null) return false;
  const element = anchor.nodeType === Node.ELEMENT_NODE ? (anchor as Element) : anchor.parentElement;
  return element?.closest('.board') !== null && element !== null;
}

/**
 * How a place name is shortened in a slot too narrow to hold it.
 *
 * One constant, applied wherever a place name has to fit somewhere narrow: the
 * strip's legend, the board chips, and `shortStopName`, which every journey slot
 * goes through. The full name is always still in the tooltip, so this only ever
 * trades characters a local reader supplies from memory against characters that
 * would otherwise be cut off by an ellipsis, which supply nothing.
 *
 * Order matters: the city prefix goes before the station words, and the long
 * compound goes before the short one it contains.
 */
const ABBREVIATIONS: ReadonlyArray<readonly [RegExp, string]> = [
  [/^München[,\-–\s]\s*/iu, ''],
  [/\bHauptbahnhof\b/giu, 'Hbf'],
  [/\bBahnhof\b/giu, 'Bf'],
];

/** The short form of a place name, for a slot that cannot hold the long one. */
export function compact(name: string): string {
  let out = name;
  for (const [pattern, replacement] of ABBREVIATIONS) out = out.replace(pattern, replacement);
  return out.trim();
}
