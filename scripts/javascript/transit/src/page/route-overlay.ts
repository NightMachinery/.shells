// The expanded journey view, in the app, for an app that has no tabs.
//
// Installed to a home screen the page runs standalone: no address bar, no tab
// strip, and no way to open a second document. The "open" links were written for
// a browser, where a new tab is free and keeps the board exactly where it was.
// In standalone the same link either does nothing or replaces the only window
// there is, which loses the board.
//
// So in standalone the same view is drawn over the board instead, and the
// history entry is what makes it feel native: opening pushes one, the Android
// back gesture pops it, and the overlay closes on `popstate` without having to
// know which of the two ways back the reader used. Nothing underneath is torn
// down, so the board and the sheet are still there when it closes.
//
// In a browser tab none of this applies and the links stay links.

import type { RouteHandoff } from '../route-link.ts';
import { encodeRoute } from '../route-link.ts';
import { button, el } from './dom.ts';
import { TIP_SHIELD_CLASS } from './tip.ts';
import { icon } from './icons.ts';
import { createRouteView, shareUrl, type RouteView } from './route-view.ts';

/**
 * Whether this is the installed app rather than a tab.
 *
 * Two questions because two platforms answer different ones: the display-mode
 * query is the standard and Safari on iOS has carried its own flag since before
 * that query existed. A page that is wrong about this is wrong in a way the
 * reader feels immediately, so it asks both.
 */
export function standalone(): boolean {
  if (typeof window === 'undefined') return false;
  const asked = typeof window.matchMedia === 'function' && window.matchMedia('(display-mode: standalone)').matches;
  const legacy = (navigator as Navigator & { standalone?: boolean }).standalone === true;
  return asked || legacy;
}

interface OpenOverlay {
  root: HTMLElement;
  view: RouteView;
  scrollY: number;
}

let open: OpenOverlay | null = null;
let installed = false;

/** Whether the overlay is on screen, for anything that needs to know. */
export function overlayOpen(): boolean {
  return open !== null;
}

function closeOverlay(): void {
  if (open === null) return;
  const { root, scrollY } = open;
  open = null;
  root.remove();
  // The board is still mounted and still scrolled where it was, but a history
  // pop is allowed to move it and some engines do. Putting it back costs
  // nothing and removes the one way this can lose the reader's place.
  window.scrollTo(0, scrollY);
}

/**
 * Listen for the way back.
 *
 * One listener for the lifetime of the page rather than one per opening: a
 * listener added on open has to be removed on close, and a close that happens
 * through the history is exactly the case where that is easy to get wrong.
 */
function install(): void {
  if (installed) return;
  installed = true;
  window.addEventListener('popstate', () => {
    // Only when this is the entry that was pushed for an overlay. The board's
    // own jump chips put a hash in the address too, and popping one of those
    // must not close anything.
    if (open !== null) closeOverlay();
  });
}

function copyControl(handoff: RouteHandoff): HTMLElement {
  const control = button('route-copy', 'copy link', 'the address this view would have as a page');
  control.addEventListener('click', () => {
    const url = shareUrl(handoff);
    const said = (text: string): void => {
      control.textContent = text;
      window.setTimeout(() => {
        control.textContent = 'copy link';
      }, 1500);
    };
    // The clipboard is refused outside a secure context and by a reader who has
    // said no to it, and a button that silently does nothing is worse than one
    // that hands over the text. The fallback puts the address on screen, ready
    // to select, which is the thing the reader was going to do with it anyway.
    void navigator.clipboard
      ?.writeText(url)
      .then(() => said('copied'))
      .catch(() => {
        const field = document.createElement('input');
        field.className = 'route-copy-field';
        field.readOnly = true;
        field.value = url;
        control.after(field);
        field.select();
      });
  });
  return control;
}

/**
 * Show one row's journeys over the board.
 *
 * The history entry carries the same fragment the page form would, so the
 * address is honest about what is on screen and a reload lands on the same
 * journey rather than on a blank board.
 */
export function openRouteOverlay(handoff: RouteHandoff): void {
  install();
  if (open !== null) closeOverlay();

  // The sheet underneath dismisses itself on any pointer it does not own, and
  // this view is one it does: it was opened from that sheet and closes back
  // onto it.
  const root = el('div', `route-overlay ${TIP_SHIELD_CLASS}`);
  root.setAttribute('role', 'dialog');
  root.setAttribute('aria-modal', 'true');

  const back = button('route-back', 'back');
  back.prepend(icon('close'));
  // Through the history rather than straight to `closeOverlay`, so the button
  // and the system gesture are the same action and cannot leave the history
  // holding an entry for an overlay that is no longer there.
  back.addEventListener('click', () => history.back());

  const body = el('div', 'route-overlay-body');
  const view = createRouteView(body, {
    controls: [back, copyControl(handoff)],
    onReplanned: (next) => history.replaceState({ transitRoute: true }, '', `#${encodeRoute(next)}`),
  });
  root.append(body);
  document.body.append(root);

  const scrollY = window.scrollY;
  history.pushState({ transitRoute: true }, '', `#${encodeRoute(handoff)}`);
  open = { root, view, scrollY };
  view.setHandoff(handoff);
}
