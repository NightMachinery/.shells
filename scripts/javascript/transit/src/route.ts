// The expanded view of one row's journeys, as its own page.
//
// It takes its whole state from the URL fragment and asks nobody anything, so
// it opens instantly, it opens with no network, and it cannot disagree with the
// board that produced it. See `src/route-link.ts` for why the fragment rather
// than a query string, and for what it carries.
//
// What it draws lives in `src/page/route-view.ts`, because the same view has to
// appear inside the app when the app is installed and has no tabs to open a
// page in. This file is the page half: the fragment, the title and the address
// bar. Everything below that line is shared with the overlay.

import { installIcons } from './page/icons.ts';
import { cssCustomProperties } from './colors.ts';
import { decodeRoute, encodeRoute } from './route-link.ts';
import { createRouteView } from './page/route-view.ts';

function injectColors(): void {
  const style = document.createElement('style');
  style.textContent = cssCustomProperties();
  document.head.append(style);
}

function boot(): void {
  injectColors();
  installIcons();
  const app = document.getElementById('app');
  if (app === null) return;
  const view = createRouteView(app, {
    setsTitle: true,
    // The address is rewritten so the tab can be reloaded, bookmarked or shared
    // and still show what is on screen rather than what was on screen an hour
    // ago. `replaceState` rather than a new entry: this is the same page.
    onReplanned: (handoff) => history.replaceState(null, '', `#${encodeRoute(handoff)}`),
  });
  view.setHandoff(decodeRoute(location.hash));
  window.addEventListener('hashchange', () => {
    view.setHandoff(decodeRoute(location.hash));
  });
}

boot();
