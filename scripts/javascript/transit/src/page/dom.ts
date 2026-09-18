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

/** Whole minutes from now until an instant, floored at zero. */
export function minutesUntil(epochMs: number, now: number): number {
  return Math.max(0, Math.floor((epochMs - now) / 60_000));
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
