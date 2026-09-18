import { contrastText, resolveColor, resolveTextColor } from '../colors.ts';
import type { Departure } from '../model.ts';
import { transitLegs, WALK_MODE, type RouteLeg, type RouteOption } from '../plan.ts';
import type { OriginLevel } from '../origin.ts';
import { clockTime, compact, el, timeLabel, timeNode } from './dom.ts';

// How a journey is drawn, in the one place both the board and the expanded page
// read it from.
//
// The board draws a journey three times over: as a slot two centimetres wide, as
// a tooltip, and, in another tab, as a full page. They must agree, and the only
// way to be sure they agree is for the leg list to have one implementation.

/** A line badge in the operator's own colour, with readable text on it. */
export function lineBadge(dep: Pick<Departure, 'line' | 'mode' | 'color'>): HTMLElement {
  const background = resolveColor(dep);
  const node = el('span', 'badge', dep.line);
  node.style.backgroundColor = background;
  node.style.color = resolveTextColor(dep) ?? contrastText(background);
  return node;
}

/** Whole minutes a walking leg takes. */
export function walkMinutesOf(leg: RouteLeg): number {
  return Math.max(1, Math.round((leg.arrival - leg.departure) / 60_000));
}

/**
 * One journey as a vertical list of legs.
 *
 * Walks get a row of their own rather than a note on the vehicle above them,
 * because a walk is a thing the reader does: fourteen minutes at the end of a
 * journey is the difference between a good recommendation and a bad one, and it
 * has to be as visible as the trains.
 */
export function renderLegs(option: RouteOption, timezone: string, now: number): HTMLElement {
  const list = el('ol', 'legs');
  for (const leg of option.legs) {
    const item = el('li', `leg leg-${leg.kind}`);
    if (leg.kind === 'walk') {
      item.append(el('span', 'leg-badge leg-walk-badge', '⭧'));
      item.append(el('span', 'leg-text', `walk ${walkMinutesOf(leg)} min to ${compact(leg.to)}`));
    } else {
      item.append(lineBadge({ line: leg.line, mode: leg.mode as Departure['mode'], color: null }));
      const text = el('span', 'leg-text');
      text.append(el('span', 'leg-from', compact(leg.from)));
      text.append(timeNode(leg.departure, now, timezone, 'leg-time'));
      text.append(el('span', 'leg-arrow', '→'));
      text.append(el('span', 'leg-to', compact(leg.to)));
      text.append(timeNode(leg.arrival, now, timezone, 'leg-time'));
      item.append(text);
    }
    list.append(item);
  }
  return list;
}

/** What a journey says in one line of plain text, for a label or a title. */
export function journeySummary(option: RouteOption, timezone: string, now: number): string {
  const parts = [`get off at ${option.exitStopName}`];
  for (const leg of option.legs) {
    parts.push(
      leg.kind === 'walk'
        ? `walk ${walkMinutesOf(leg)} min to ${leg.to}`
        : `${leg.line} ${timeLabel(leg.departure, now, timezone)} to ${leg.to}`,
    );
  }
  parts.push(`arrive ${option.destinationName} ${timeLabel(option.arrival, now, timezone)}`);
  parts.push(`${Math.round(option.walkMinutes)} min on foot`);
  parts.push(option.transfers === 0 ? 'direct' : `${option.transfers} change${option.transfers === 1 ? '' : 's'}`);
  if (option.tight) parts.push(`tight by ${option.tightBy} min`);
  return parts.join(', ');
}

/**
 * What the compact slot says before the arrival: where to get off, and what to
 * catch there.
 *
 * Two nodes rather than one string, because they do not give way equally. In a
 * slot two centimetres wide something has to be cut, and cutting the stop name
 * leaves "Hbf … 00:08", which still says where and when; cutting the line
 * leaves "Hauptbahnhof Nord …", which says neither. So the name ellipsises and
 * the line does not.
 */
export function slotHead(option: RouteOption, shorten: (name: string) => string): HTMLElement {
  const wrap = el('span', 'route-head');
  wrap.append(el('span', 'route-exit', shorten(option.exitStopName)));
  const onward = transitLegs(option)[1];
  if (onward !== undefined) wrap.append(el('span', 'route-line', `· ${onward.line}`));
  return wrap;
}

export interface JourneyNotes {
  /** True when this exit is not the one this line usually uses. */
  better?: boolean;
  /** Which step of the origin chain the plan was made from. */
  origin?: OriginLevel | null | undefined;
}

/**
 * The footnotes of a journey, as chips.
 *
 * Chips rather than a sentence because they are independent facts about the
 * same journey and a reader wants to see at a glance which of them apply, not
 * parse a clause list. Each keeps its own explanation, so the short form can be
 * two words.
 */
export function renderNotes(option: RouteOption, notes: JourneyNotes): HTMLElement {
  const wrap = el('div', 'journey-notes');
  const add = (className: string, text: string, title: string): void => {
    const chip = el('span', `journey-note ${className}`, text);
    chip.title = title;
    wrap.append(chip);
  };
  add(
    'note-walk',
    `walk ${Math.round(option.walkMinutes)} min`,
    'minutes on foot over the whole journey: every change plus the walk at the end',
  );
  add(
    'note-changes',
    option.transfers === 0 ? 'direct' : `${option.transfers} change${option.transfers === 1 ? '' : 's'}`,
    'how many vehicles you board after this one',
  );
  if (option.tight) {
    add(
      'note-tight',
      `tight by ${option.tightBy} min`,
      'the onward departure leaves before the walk allows, so this only works if this vehicle runs early or you are quick. It is never the recommendation.',
    );
  }
  if (notes.better === true) {
    add('note-better', 'different exit', 'not where this line usually puts you down on the way here');
  }
  if (notes.origin === 'platform') {
    add('note-origin', 'via platform', 'planned from this stop’s platform, which is how the journey planner knows it');
  }
  if (notes.origin === 'coordinate') {
    add('note-origin', 'via position', 'planned from this stop’s position, which is all the journey planner knows of it');
  }
  return wrap;
}

/** The headline of a journey: where to get off, and when you arrive. */
export function renderJourneyHead(option: RouteOption, timezone: string, now: number): HTMLElement {
  const head = el('div', 'journey-head');
  head.append(el('span', 'journey-exit', `Get off at ${option.exitStopName}`));
  const arrival = el('span', 'journey-arrival');
  arrival.append(timeNode(option.arrival, now, timezone, 'journey-arrival-time'));
  arrival.append(el('span', 'journey-arrival-place', option.destinationName));
  head.append(arrival);
  return head;
}

/** One journey in full: headline, legs, chips. */
export function renderJourney(option: RouteOption, timezone: string, now: number, notes: JourneyNotes): HTMLElement {
  const wrap = el('div', `journey${option.tight ? ' journey-tight' : ''}`);
  wrap.append(renderJourneyHead(option, timezone, now));
  wrap.append(renderLegs(option, timezone, now));
  wrap.append(renderNotes(option, notes));
  return wrap;
}

/** One alternative, as the single line it collapses to until it is opened. */
export function alternativeLine(option: RouteOption, timezone: string): string {
  const onward = transitLegs(option)
    .slice(1)
    .map((leg) => leg.line)
    .join(' → ');
  const chain = onward.length === 0 ? 'direct' : onward;
  return `${compact(option.exitStopName)} · ${chain} · ${clockTime(option.arrival, timezone)} · walk ${Math.round(option.walkMinutes)}`;
}

/** True when the walk legs carry the whole of a journey's mode vocabulary. */
export function isWalkLeg(leg: RouteLeg): boolean {
  return leg.mode === WALK_MODE;
}
