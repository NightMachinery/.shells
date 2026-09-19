// The expanded view of one row's journeys, as its own page.
//
// It takes its whole state from the URL fragment and asks nobody anything, so
// it opens instantly, it opens with no network, and it cannot disagree with the
// board that produced it. See `src/route-link.ts` for why the fragment rather
// than a query string, and for what it carries.
//
// The one thing it does reach for is a fresh plan, and only when the reader
// presses the button. That is the honest answer to a snapshot: it says when it
// was taken, and it offers to take another.

import { installIcons } from './page/icons.ts';
import { cssCustomProperties } from './colors.ts';
import { DEFAULT_WALK_WEIGHT } from './config.ts';
import type { Departure } from './model.ts';
import { planBoard, type RouteOption } from './plan.ts';
import { decodeRoute, encodeRoute, type RouteHandoff } from './route-link.ts';
import { planTargets, type DestinationPlace } from './targets.ts';
import { button, el, timeLabel, timeNode } from './page/dom.ts';
import { idbGet, idbSet, STORE_ORIGINS } from './page/idb.ts';
import { lineBadge, renderLegs, renderNotes } from './page/journey.ts';
import type { ExportedConfig } from './page/types.ts';

let handoff: RouteHandoff | null = null;
let planning = false;
let error: string | null = null;

function injectColors(): void {
  const style = document.createElement('style');
  style.textContent = cssCustomProperties();
  document.head.append(style);
}

/** One journey, as a section: which one it is, its legs, and its footnotes. */
function renderOption(option: RouteOption, index: number, data: RouteHandoff, now: number): HTMLElement {
  const best = index === data.first;
  const section = el('section', `route-option${best ? ' route-option-best' : ''}${option.tight ? ' route-option-tight' : ''}`);

  const head = el('header', 'route-option-head');
  head.append(el('h2', 'route-option-title', `Get off at ${option.exitStopName}`));
  if (best) head.append(el('span', 'route-option-mark', 'recommended'));
  section.append(head);

  section.append(renderLegs(option, data.timezone, now));

  const arrival = el('p', 'route-arrival-line');
  arrival.append(el('span', 'route-arrival-label', `arrive ${option.destinationName}`));
  arrival.append(timeNode(option.arrival, now, data.timezone, 'route-arrival-time'));
  section.append(arrival);

  section.append(renderNotes(option, {}));
  return section;
}

function render(): void {
  const app = document.getElementById('app');
  if (app === null) return;
  const data = handoff;
  if (data === null) {
    app.replaceChildren(
      el('p', 'empty', 'This link carries no journey. Open one from a board’s journey slot, and if this link was pasted from somewhere it may have been cut short.'),
    );
    return;
  }

  const now = Date.now();
  document.title = `${data.row.line} → ${data.destination}`;

  const header = el('header', 'route-page-head');
  const top = el('div', 'route-page-line');
  top.append(lineBadge(data.row));
  top.append(timeNode(data.row.realtime, now, data.timezone, 'route-page-time'));
  if (data.row.delayMin !== 0) {
    top.append(el('span', 'route-page-delay', `${data.row.delayMin > 0 ? '+' : ''}${data.row.delayMin} min`));
  }
  header.append(top);
  header.append(el('p', 'route-page-where', `${data.from} → ${data.destination}`));
  if (data.row.platform !== null) header.append(el('p', 'route-page-platform', `platform ${data.row.platform}`));
  if (data.row.cancelled) header.append(el('p', 'route-page-cancelled', 'This departure is cancelled.'));

  const foot = el('div', 'route-page-foot');
  foot.append(el('span', 'route-page-planned', `planned ${timeLabel(data.plannedAt, now, data.timezone)}`));
  const again = button('route-replan', planning ? 'planning…' : 'plan again');
  again.disabled = planning;
  again.addEventListener('click', () => void replan());
  foot.append(again);
  if (error !== null) foot.append(el('span', 'route-page-error', error));
  header.append(foot);

  const nodes: Node[] = [header];
  if (data.options.length === 0) nodes.push(el('p', 'empty', 'No journey was found for this departure.'));
  // The one being shown first goes first, then the others in their own order.
  const order = [data.first, ...data.options.map((_, index) => index).filter((index) => index !== data.first)];
  for (const index of order) {
    const option = data.options[index];
    if (option !== undefined) nodes.push(renderOption(option, index, data, now));
  }
  app.replaceChildren(...nodes);
}

/**
 * Plan this one row again, now.
 *
 * One row and one board, which is what makes this cheap enough to offer on a
 * page that exists to be opened from a link: it is the same question the board
 * asks, narrowed to the single departure the reader is looking at.
 */
async function replan(): Promise<void> {
  const data = handoff;
  if (data === null || planning) return;
  planning = true;
  error = null;
  render();
  try {
    const response = await fetch('data/config.json', { headers: { Accept: 'application/json' } });
    if (!response.ok) throw new Error(`config.json: HTTP ${response.status}`);
    const config = (await response.json()) as ExportedConfig;
    const place = (config.places ?? []).find((entry) => entry.name === data.destinationKey);
    if (place === undefined) throw new Error('this destination is no longer configured');
    const destinationProfile = config.profiles.find((entry) => entry.key === place.name);
    const targets = planTargets(
      place as DestinationPlace,
      (destinationProfile?.boards ?? []).map((board) => ({
        stops: board.stops,
        walkMinutes: board.walk_minutes,
        walkMinutesByStop: board.walk_minutes_by_stop,
      })),
    );
    const row: Departure = data.row;
    const planned = await planBoard({
      stop: row.stop,
      targets,
      rows: [row],
      startMs: Date.now(),
      baseUrl: config.backends.transitous_base_url,
      walkWeight: config.defaults.walk_weight ?? DEFAULT_WALK_WEIGHT,
      originCache: {
        get: (key) => idbGet<string[]>(STORE_ORIGINS, key),
        set: (key, value) => idbSet(STORE_ORIGINS, key, value),
      },
      ...(config.defaults.plan_modes === undefined ? {} : { planModes: config.defaults.plan_modes }),
    });
    const fresh = planned[0];
    if (fresh === undefined) throw new Error('nothing came back for this departure');
    handoff = { ...data, options: fresh.options, first: 0, plannedAt: Date.now() };
    // The address is rewritten so the tab can be reloaded, bookmarked or shared
    // and still show what is on screen rather than what was on screen an hour
    // ago. `replaceState` rather than a new entry: this is the same page.
    history.replaceState(null, '', `#${encodeRoute(handoff)}`);
  } catch (caught) {
    error = caught instanceof Error ? caught.message : String(caught);
  } finally {
    planning = false;
    render();
  }
}

function boot(): void {
  injectColors();
  installIcons();
  handoff = decodeRoute(location.hash);
  render();
  window.addEventListener('hashchange', () => {
    handoff = decodeRoute(location.hash);
    render();
  });
}

boot();
