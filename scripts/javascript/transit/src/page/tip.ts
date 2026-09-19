import { button, el } from './dom.ts';

// The one tooltip on this page.
//
// The native `title` attribute was doing this job and it cannot: it takes plain
// text only, so a journey came out as a run-on sentence with separators standing
// in for structure, and on a phone it never appears at all, because a touch
// device has no hover. That is the wrong half to lose. The phone is where this
// page is read and where a slot is narrowest, so it is exactly where the long
// answer is most needed.
//
// What replaces it is deliberately not a dialog on the desktop. Nothing is
// trapped, nothing behind it is disabled, and it never takes focus away from
// what opened it: it is an explanation of something already on screen, so the
// reader has to be able to keep reading the screen while it is open.
//
// On a phone it is a sheet instead, because an anchored popover the width of the
// screen anchored to a slot two centimetres wide is a sheet already, just one
// that has to be aimed at. The sheet has a backdrop, and the backdrop is what
// makes it dismissable by tapping anywhere, which is what a phone reader tries
// first.
//
// It closes only when the reader says so: backdrop, close button, Escape, or
// tapping the thing that opened it again. Not on scroll, and not because the
// board underneath it re-rendered. Both of those used to close it, and both were
// the page taking something away that the reader was in the middle of reading:
// the board refreshes every thirty seconds, so an explanation had a thirty
// second life expectancy through no fault of the reader's.

/** Margin kept between the tip and every viewport edge, in pixels. */
const EDGE_MARGIN_PX = 8;

/** How long a pointer must rest on a target before the tip opens, in ms. */
const HOVER_DELAY_MS = 120;

/**
 * The longest press that still counts as a tap, in milliseconds.
 *
 * Shorter than the long press that opens the reminder popup, so the two cannot
 * both fire from one press, and long enough that a deliberate but unhurried tap
 * still opens the sheet. A press between the two does nothing at all, which is
 * the honest answer: the reader was between two gestures and the page should not
 * guess which.
 */
const TAP_MS = 400;

/** How far a finger may travel during a tap before it is a scroll instead. */
const TAP_MOVE_PX = 10;

/** Below this viewport width the tip is a sheet rather than an anchored popover. */
const SHEET_MAX_WIDTH_PX = 560;

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
  /**
   * Whether hovering opens the tip.
   *
   * False for a target that is only a tap target, like a whole row: on a desktop
   * a tip that follows the pointer down a list of two hundred rows is not an
   * explanation, it is a cursor trail.
   */
  hoverOpens?: boolean;
  /** A plain-text summary, so the target still says something to a screen reader. */
  label?: string;
  /**
   * What this tip explains, stable across re-renders.
   *
   * The page rebuilds every board from scratch whenever anything changes, so the
   * element an open tip was opened from is thrown away several times a minute.
   * The key is how the new element claims the open tip: same key, same
   * explanation, so the tip is re-pointed at the new element instead of being
   * closed. Without it the only way to keep a tip open would be to stop
   * re-rendering, which is the one thing this page is built around.
   */
  key?: string;
  /**
   * What the tip currently says, as a cheap string.
   *
   * Compared on re-render: equal means the content is left alone, so a reader
   * mid-sentence is not interrupted by an identical redraw; different means the
   * body is rebuilt in place, because the journey really did change and the old
   * answer is now wrong.
   */
  signature?: string;
}

interface OpenTip {
  node: HTMLElement;
  body: HTMLElement;
  backdrop: HTMLElement | null;
  target: HTMLElement;
  key: string | null;
  signature: string | null;
  build: () => HTMLElement;
  close: () => void;
}

let open: OpenTip | null = null;
let hoverTimer: number | null = null;
/** Set when a tap opened a tip, so the click the browser sends afterwards is eaten. */
let swallowClick = false;

/** Whether the viewport is narrow enough for the sheet presentation. */
function sheetMode(): boolean {
  return window.innerWidth <= SHEET_MAX_WIDTH_PX;
}

/** Close whatever tip is open. Safe to call when none is. */
export function closeTip(): void {
  open?.close();
}

/** Whether a tip is currently open for this element. */
export function tipOpenFor(target: HTMLElement): boolean {
  return open !== null && open.target === target;
}

/** Whether a tip is open for this key, whichever element it came from. */
export function tipOpenForKey(key: string): boolean {
  return open !== null && open.key === key;
}

function placeTip(node: HTMLElement, target: HTMLElement): void {
  const rect = target.getBoundingClientRect();
  const width = node.offsetWidth;
  const height = node.offsetHeight;
  const vw = window.innerWidth;
  const vh = window.innerHeight;

  // Centred on the target, then pushed back inside the viewport.
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
export function showTip(target: HTMLElement, build: () => HTMLElement, options: TipOptions = {}): void {
  closeTip();
  const sheet = sheetMode();
  const node = el('div', sheet ? 'tip tip-sheet' : 'tip');
  node.setAttribute('role', sheet ? 'dialog' : 'tooltip');
  if (sheet) node.setAttribute('aria-modal', 'false');

  let backdrop: HTMLElement | null = null;
  if (sheet) {
    backdrop = el('div', 'tip-backdrop');
    document.body.append(backdrop);
  } else {
    node.style.position = 'fixed';
  }

  const body = el('div', 'tip-content');
  body.append(build());
  node.append(body);

  if (sheet) {
    // Top right, and drawn over the content rather than above it, so the first
    // line of the explanation is still the first thing under the reader's eye.
    const shut = button('tip-close', '×', 'close');
    shut.setAttribute('aria-label', 'close');
    shut.addEventListener('click', () => closeTip());
    node.append(shut);
  }

  document.body.append(node);
  if (!sheet) placeTip(node, target);

  const onOutside = (event: Event): void => {
    const hit = event.target;
    // A pointer that starts inside the tip belongs to the tip, whatever it does
    // next. This is what lets a link in there be a link: the close handler used
    // to run first and take the anchor out of the document before the browser
    // got as far as following it.
    if (hit instanceof Node && (node.contains(hit) || target.contains(hit))) return;
    closeTip();
  };
  const onKey = (event: KeyboardEvent): void => {
    if (event.key === 'Escape') closeTip();
  };

  // Capture, so a handler on a row cannot swallow the dismissal first.
  document.addEventListener('pointerdown', onOutside, true);
  document.addEventListener('keydown', onKey, true);

  open = {
    node,
    body,
    backdrop,
    target,
    key: options.key ?? null,
    signature: options.signature ?? null,
    build,
    close: () => {
      document.removeEventListener('pointerdown', onOutside, true);
      document.removeEventListener('keydown', onKey, true);
      node.remove();
      backdrop?.remove();
      open = null;
    },
  };
}

/**
 * Hand an open tip over to the element that replaced the one it was opened from.
 *
 * Returns true when this element now owns the open tip. The content is rebuilt
 * only when the signature says it has to.
 */
function rebindTip(target: HTMLElement, build: () => HTMLElement, options: TipOptions): boolean {
  const key = options.key;
  if (open === null || key === undefined || open.key !== key) return false;
  open.target = target;
  open.build = build;
  const signature = options.signature ?? null;
  if (signature !== open.signature) {
    open.signature = signature;
    open.body.replaceChildren(build());
    if (!open.node.classList.contains('tip-sheet')) placeTip(open.node, target);
  }
  return true;
}

/**
 * Give an element a tip, built lazily when it is actually opened.
 *
 * Lazily because the page rebuilds every board on every render and most tips are
 * never looked at; building a journey's leg list for each of two hundred rows
 * twice a minute would be work done for nobody.
 */
export function attachTip(target: HTMLElement, build: () => HTMLElement, options: TipOptions = {}): void {
  const hovers = options.hoverOpens !== false;
  if (options.label !== undefined) {
    target.setAttribute('aria-label', options.label);
    // Kept as well as the tip, not instead of it: a reader who has hovered for
    // the browser's own tooltip delay should not be left with nothing on a
    // browser where the pointer events below did not fire.
    target.title = options.label;
  }

  rebindTip(target, build, options);

  const openNow = (): void => showTip(target, build, options);

  if (hovers) attachHover(target, openNow);

  if (options.tapOpens === false) return;
  attachTap(target, openNow);
}

/** The desktop half: hover with a delay, and the keyboard. */
function attachHover(target: HTMLElement, openNow: () => void): void {
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
}

/**
 * The touch half.
 *
 * The touch path, in pointer events from end to end.
  //
  // It used to hang off the `click` event, filtered by `event instanceof
  // PointerEvent && event.pointerType === 'touch'`. That filter is the bug
  // behind "sometimes a tap does nothing": whether a click arrives as a
  // PointerEvent at all is a browser detail, and where it does not, the handler
  // returned without preventing the default, so the slot followed its own href
  // into a new tab that the browser then refused to open from a gesture it had
  // not classified as one. Nothing appeared, and nothing had gone wrong as far
  // as any log was concerned. Pointer events say what kind of pointer they are
  // by definition, so there is nothing left to infer.
 */
function attachTap(target: HTMLElement, openNow: () => void): void {
  let press: { x: number; y: number; at: number } | null = null;
  const release = (): void => {
    press = null;
    target.classList.remove('pressed');
  };

  target.addEventListener('pointerdown', (event) => {
    const pointer = event as PointerEvent;
    if (pointer.pointerType !== 'touch') return;
    press = { x: pointer.clientX, y: pointer.clientY, at: Date.now() };
    // Immediately, before anything is decided: a phone that does not
    // acknowledge a finger reads as a phone that did not notice it.
    target.classList.add('pressed');
  });

  target.addEventListener('pointermove', (event) => {
    const pointer = event as PointerEvent;
    if (press === null) return;
    if (Math.abs(pointer.clientX - press.x) > TAP_MOVE_PX || Math.abs(pointer.clientY - press.y) > TAP_MOVE_PX) release();
  });

  target.addEventListener('pointercancel', release);
  target.addEventListener('pointerleave', (event) => {
    if ((event as PointerEvent).pointerType === 'touch') release();
  });

  target.addEventListener('pointerup', (event) => {
    const pointer = event as PointerEvent;
    if (pointer.pointerType !== 'touch') return;
    const started = press;
    release();
    if (started === null) return;
    // Past this the press belongs to the long press, which opens the reminder
    // popup and eats the click itself.
    if (Date.now() - started.at > TAP_MS) return;
    if (Math.abs(pointer.clientX - started.x) > TAP_MOVE_PX || Math.abs(pointer.clientY - started.y) > TAP_MOVE_PX) return;
    swallowClick = true;
    // Cleared on a timer as well as by the click, because a browser that sends
    // no click after a tap would otherwise leave the next one armed.
    window.setTimeout(() => {
      swallowClick = false;
    }, 700);
    if (tipOpenFor(target)) closeTip();
    else openNow();
  });

  // The click the browser sends after the tap it has already been served. On an
  // anchor this is what stops the slot from navigating as well as explaining.
  target.addEventListener(
    'click',
    (event) => {
      if (!swallowClick) return;
      swallowClick = false;
      event.preventDefault();
      event.stopPropagation();
    },
    true,
  );
}
