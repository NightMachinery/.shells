import { el } from './dom.ts';

// The one tooltip on this page.
//
// The native `title` attribute was doing this job and it cannot: it takes plain
// text only, so a journey came out as a run-on sentence with separators standing
// in for structure, and on a phone it never appears at all, because a touch
// device has no hover. That is the wrong half to lose. The phone is where this
// page is read and where a slot is narrowest, so it is exactly where the long
// answer is most needed.
//
// What replaces it is deliberately not a dialog. Nothing is trapped, nothing
// behind it is disabled, and it never takes focus away from what opened it: it
// is an explanation of something already on screen, so the reader has to be able
// to keep reading the screen while it is open. One is open at a time, an outside
// tap or Escape closes it, and a scroll closes it because on a page this long a
// scroll is the commonest way to mean "never mind".

/** Margin kept between the tip and every viewport edge, in pixels. */
const EDGE_MARGIN_PX = 8;

/** How long a pointer must rest on a target before the tip opens, in ms. */
const HOVER_DELAY_MS = 120;

export interface TipOptions {
  /**
   * Whether a tap on a touch screen opens the tip.
   *
   * True for something whose click does nothing else, like a journey slot. False
   * for a control with its own primary action, like a jump chip: stealing that
   * tap to show an explanation would break the thing the reader meant to do, so
   * those get the tip on hover and on a long press instead.
   */
  tapOpens?: boolean;
  /** A plain-text summary, so the target still says something to a screen reader. */
  label?: string;
}

interface OpenTip {
  node: HTMLElement;
  target: HTMLElement;
  close: () => void;
}

let open: OpenTip | null = null;
let hoverTimer: number | null = null;

/** Close whatever tip is open. Safe to call when none is. */
export function closeTip(): void {
  open?.close();
}

/** Whether a tip is currently open for this element. */
export function tipOpenFor(target: HTMLElement): boolean {
  return open !== null && open.target === target;
}

function placeTip(node: HTMLElement, target: HTMLElement): void {
  const rect = target.getBoundingClientRect();
  const width = node.offsetWidth;
  const height = node.offsetHeight;
  const vw = window.innerWidth;
  const vh = window.innerHeight;

  // Centred on the target, then pushed back inside the viewport. On a phone
  // that almost always means "flush against the left margin", which is correct:
  // the tip is wider than most of what opens it.
  let left = rect.left + rect.width / 2 - width / 2;
  left = Math.min(Math.max(EDGE_MARGIN_PX, left), Math.max(EDGE_MARGIN_PX, vw - width - EDGE_MARGIN_PX));

  let top = rect.bottom + EDGE_MARGIN_PX;
  if (top + height > vh - EDGE_MARGIN_PX) top = rect.top - height - EDGE_MARGIN_PX;
  if (top < EDGE_MARGIN_PX) top = Math.max(EDGE_MARGIN_PX, (vh - height) / 2);

  node.style.left = `${Math.round(left)}px`;
  node.style.top = `${Math.round(top)}px`;
  node.style.maxHeight = `${Math.max(0, vh - 2 * EDGE_MARGIN_PX)}px`;
}

/**
 * Show a tip for a target. Exported because a few places open one directly, from
 * a button inside another tip for instance, rather than through `attachTip`.
 */
export function showTip(target: HTMLElement, build: () => HTMLElement): void {
  closeTip();
  const node = el('div', 'tip');
  node.setAttribute('role', 'tooltip');
  node.style.position = 'fixed';
  node.append(build());
  document.body.append(node);
  placeTip(node, target);

  const onOutside = (event: Event): void => {
    const hit = event.target;
    if (hit instanceof Node && (node.contains(hit) || target.contains(hit))) return;
    closeTip();
  };
  const onKey = (event: KeyboardEvent): void => {
    if (event.key === 'Escape') closeTip();
  };
  const onScroll = (): void => closeTip();

  // Capture, so a handler on a row cannot swallow the dismissal first.
  document.addEventListener('pointerdown', onOutside, true);
  document.addEventListener('keydown', onKey, true);
  window.addEventListener('scroll', onScroll, true);
  window.addEventListener('resize', onScroll);

  open = {
    node,
    target,
    close: () => {
      document.removeEventListener('pointerdown', onOutside, true);
      document.removeEventListener('keydown', onKey, true);
      window.removeEventListener('scroll', onScroll, true);
      window.removeEventListener('resize', onScroll);
      node.remove();
      open = null;
    },
  };
}

/**
 * Give an element a tip, built lazily when it is actually opened.
 *
 * Lazily because the page rebuilds every board on every render and most tips are
 * never looked at; building a journey's leg list for each of two hundred rows
 * twice a minute would be work done for nobody.
 */
export function attachTip(target: HTMLElement, build: () => HTMLElement, options: TipOptions = {}): void {
  if (options.label !== undefined) {
    target.setAttribute('aria-label', options.label);
    // Kept as well as the tip, not instead of it: a reader who has hovered for
    // the browser's own tooltip delay should not be left with nothing on a
    // browser where the pointer events below did not fire.
    target.title = options.label;
  }

  const openNow = (): void => showTip(target, build);

  target.addEventListener('pointerenter', (event) => {
    if ((event as PointerEvent).pointerType === 'touch') return;
    if (hoverTimer !== null) window.clearTimeout(hoverTimer);
    hoverTimer = window.setTimeout(openNow, HOVER_DELAY_MS);
  });
  target.addEventListener('pointerleave', (event) => {
    if ((event as PointerEvent).pointerType === 'touch') return;
    if (hoverTimer !== null) window.clearTimeout(hoverTimer);
    hoverTimer = null;
    if (tipOpenFor(target)) closeTip();
  });
  target.addEventListener('focus', openNow);
  target.addEventListener('blur', () => {
    if (tipOpenFor(target)) closeTip();
  });

  if (options.tapOpens !== false) {
    target.addEventListener('click', (event) => {
      // A tap on a touch screen opens the tip; a click on a desktop has already
      // had the hover, so it is free to do whatever the element does instead.
      if (!(event instanceof PointerEvent) || event.pointerType !== 'touch') return;
      event.preventDefault();
      event.stopPropagation();
      if (tipOpenFor(target)) closeTip();
      else openNow();
    });
  }
}
