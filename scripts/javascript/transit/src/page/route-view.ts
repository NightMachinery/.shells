// The expanded view of one row's journeys, as a thing that can be put anywhere.
//
// It used to be a page and only a page, which was fine while every way of
// reaching it was a link. Installed on a phone there are no tabs, so a link to
// a second document has nowhere to go: the reader taps "open" and either
// nothing happens or the whole app is replaced. The view therefore stopped
// being a page and became a render function over a container, and the page is
// now one of its two callers. The other is the in-app overlay.
//
// Everything it draws comes from the handoff it is given, so it opens instantly,
// it opens with no network, and it cannot disagree with the board that produced
// it. The one thing it reaches for is a fresh plan, and only when the reader
// presses the button.

import { DEFAULT_WALK_WEIGHT } from '../config.ts';
import type { Departure } from '../model.ts';
import { planBoard, type RouteOption } from '../plan.ts';
import { encodeRoute, type RouteHandoff } from '../route-link.ts';
import { planTargets, type DestinationPlace } from '../targets.ts';
import { button, el, timeLabel, timeNode } from './dom.ts';
import { idbGet, idbSet, STORE_ORIGINS } from './idb.ts';
import { lineBadge, renderLegs, renderNotes } from './journey.ts';
import type { ExportedConfig } from './types.ts';

export interface RouteViewOptions {
  /**
   * Controls to put in the header's foot row, before the replan button.
   *
   * The page needs none and the overlay needs two, a way back and a way to copy
   * the link it would have been. Passed in rather than decided here, because
   * this module has no business knowing which of its two callers it is serving.
   */
  controls?: HTMLElement[];
  /** Whether to write the document's title. The page does; an overlay must not. */
  setsTitle?: boolean;
  /** Told when a replan replaces the handoff, so the caller can update its address. */
  onReplanned?: (handoff: RouteHandoff) => void;
}

export interface RouteView {
  /** Draw the current state into the container. */
  render: () => void;
  /** Replace what is being shown, and redraw. */
  setHandoff: (next: RouteHandoff | null) => void;
  /** What is being shown, including any replan that has landed since. */
  current: () => RouteHandoff | null;
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

export function createRouteView(container: HTMLElement, options: RouteViewOptions = {}): RouteView {
  let handoff: RouteHandoff | null = null;
  let planning = false;
  let error: string | null = null;

  /**
   * Plan this one row again, now.
   *
   * One row and one board, which is what makes this cheap enough to offer on a
   * view that exists to be opened from a link: it is the same question the board
   * asks, narrowed to the single departure the reader is looking at.
   */
  const replan = async (): Promise<void> => {
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
      options.onReplanned?.(handoff);
    } catch (caught) {
      error = caught instanceof Error ? caught.message : String(caught);
    } finally {
      planning = false;
      render();
    }
  };

  function render(): void {
    const data = handoff;
    if (data === null) {
      container.replaceChildren(
        el(
          'p',
          'empty',
          'This link carries no journey. Open one from a board’s journey slot, and if this link was pasted from somewhere it may have been cut short.',
        ),
      );
      return;
    }

    const now = Date.now();
    if (options.setsTitle === true) document.title = `${data.row.line} → ${data.destination}`;

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
    for (const control of options.controls ?? []) foot.append(control);
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
    container.replaceChildren(...nodes);
  }

  return {
    render,
    setHandoff: (next: RouteHandoff | null): void => {
      handoff = next;
      error = null;
      render();
    },
    current: (): RouteHandoff | null => handoff,
  };
}

/** The address this view would have as a page, which is what a share has to carry. */
export function shareUrl(handoff: RouteHandoff): string {
  return new URL(`route.html#${encodeRoute(handoff)}`, location.href).href;
}
