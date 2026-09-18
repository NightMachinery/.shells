import { normaliseLine, walkMinutesFor } from '../filter.ts';
import type { Board, Departure } from '../model.ts';
import { button, el } from './dom.ts';

// Departure alarms: a reminder, a few minutes before a vehicle leaves, for a
// row the reader picked out of a board.
//
// The feature is best effort and the popup says so out loud. There is no server
// and no service worker behind it, so a reminder exists only as a `setTimeout`
// in this page: it fires while the page or the installed app is open, and a
// browser that has had the tab in the background for a long time is free to
// drop the timer. Saying that plainly is better than a promise the page cannot
// keep, because the reader is deciding whether to keep watching the clock.
//
// Two mechanisms make the best effort as good as it can be. Every refresh
// re-arms each alarm against the trip's current realtime departure, so a train
// that picks up four minutes of delay moves its reminder by four minutes. And
// because the same pass runs every thirty seconds, it also catches up: a
// reminder whose timer was dropped while the tab slept still fires, as long as
// it is only a little late.

/** Where the pending alarms are mirrored. Every key here starts `transit.alarm.`. */
const KEY_ALARMS = 'transit.alarm.pending';

/** The lead times offered, in minutes before the target instant. */
const LEAD_CHOICES: readonly number[] = [2, 5, 10, 15, 60];
/** Checked when the popup opens on a row with no alarm yet. */
const LEAD_DEFAULT = 2;

const LONG_PRESS_MS = 500;
/**
 * How far a finger may travel before the press is read as a scroll.
 *
 * This page is scrolled constantly, so the threshold has to be tight: a
 * generous one turns every flick that starts on a row into a popup, which makes
 * the board unusable. A few pixels is the width of the wobble in a stationary
 * thumb.
 */
const MOVE_TOLERANCE_PX = 8;

/** How late a dropped reminder may be and still be worth firing. */
const CATCHUP_MS = 90_000;
/** How long past its instant an alarm is kept before it is forgotten. */
const DROP_GRACE_MS = 60_000;

const EDGE_MARGIN_PX = 8;
/** Below this width the popup is centred rather than anchored to the row. */
const NARROW_VIEWPORT_PX = 420;

const MINUTE_MS = 60_000;

/** One pending reminder set, for one trip. */
interface Alarm {
  /** The trip key, as `tripKey` builds it. */
  key: string;
  line: string;
  destination: string;
  stop: string;
  /** Scheduled departure, epoch milliseconds; half of the trip's identity. */
  planned: number;
  /**
   * The instant the lead times count back from: the departure, less the walk
   * when the reader asked to keep it. Recomputed on every refresh.
   */
  targetMs: number;
  /** Minutes of walking folded into `targetMs`; zero when the toggle is off. */
  walkMinutes: number;
  /** Lead times still to fire, in minutes, ascending. */
  leads: number[];
  /** Lead times already dealt with, so a re-arm does not repeat them. */
  fired: number[];
}

let alarms: Map<string, Alarm> | null = null;

/** Live `setTimeout` handles per alarm key. Never persisted: a reload re-arms. */
const timers = new Map<string, number[]>();

/**
 * The walking time last seen for a trip, keyed the same way as an alarm.
 *
 * The popup seeds its walk figure from the board, but the bell marker's own
 * click has only a `Departure` to work from. This map is rebuilt from scratch
 * on every `rearm`, so it neither goes stale nor grows.
 */
const walkSeeds = new Map<string, number>();

/**
 * How one vehicle is recognised across refreshes: its line, its scheduled
 * departure and the stop it was fetched for.
 *
 * All three parts are stable in a way the obvious alternatives are not. The
 * realtime departure moves with the delay, which is the whole point of re-arming
 * and therefore useless as identity. A row index moves whenever a vehicle ahead
 * leaves. The scheduled departure does not move: it is what the timetable says,
 * and a backend correcting it is publishing a different trip. The stop matters
 * because a board may merge several, and the same line calls at each. The line
 * label is folded through `normaliseLine` because backends disagree about the
 * space between a category and its number, and the same trip must not change
 * identity when the fallback backend answers instead of the primary one.
 */
export function tripKey(dep: Departure): string {
  return `${normaliseLine(dep.line)}|${dep.planned}|${dep.stop}`;
}

// ---------------------------------------------------------------- persistence

function store(): Map<string, Alarm> {
  if (alarms === null) alarms = load();
  return alarms;
}

function isAlarm(value: unknown): value is Alarm {
  if (typeof value !== 'object' || value === null) return false;
  const row = value as Record<string, unknown>;
  return (
    typeof row.key === 'string' &&
    typeof row.line === 'string' &&
    typeof row.destination === 'string' &&
    typeof row.stop === 'string' &&
    typeof row.planned === 'number' &&
    typeof row.targetMs === 'number' &&
    typeof row.walkMinutes === 'number' &&
    Array.isArray(row.leads) &&
    Array.isArray(row.fired)
  );
}

/**
 * Read the mirror, dropping anything already past.
 *
 * The mirror exists for a reload inside the same journey, not for tomorrow: a
 * reminder whose moment has gone is noise, and a stale one would fire the
 * instant the page came back.
 */
function load(): Map<string, Alarm> {
  const now = Date.now();
  const restored = new Map<string, Alarm>();
  let raw: string | null;
  try {
    raw = localStorage.getItem(KEY_ALARMS);
  } catch {
    return restored;
  }
  if (raw === null) return restored;
  let parsed: unknown;
  try {
    parsed = JSON.parse(raw);
  } catch {
    return restored;
  }
  if (!Array.isArray(parsed)) return restored;
  for (const entry of parsed) {
    if (!isAlarm(entry)) continue;
    if (entry.targetMs + DROP_GRACE_MS < now) continue;
    restored.set(entry.key, {
      ...entry,
      leads: entry.leads.filter((lead): lead is number => typeof lead === 'number'),
      fired: entry.fired.filter((lead): lead is number => typeof lead === 'number'),
    });
  }
  return restored;
}

/**
 * Told to the page when the set of pending alarms changes, so the bell markers
 * in the rows can appear and disappear without waiting for the next refresh.
 * The popup owns the alarms and the page owns the rows, and this is the one
 * seam between them.
 */
let onAlarmsChanged: (() => void) | null = null;

export function setOnAlarmsChanged(handler: () => void): void {
  onAlarmsChanged = handler;
}

function persist(): void {
  const pending = [...store().values()];
  onAlarmsChanged?.();
  try {
    if (pending.length === 0) localStorage.removeItem(KEY_ALARMS);
    else localStorage.setItem(KEY_ALARMS, JSON.stringify(pending));
  } catch {
    // A private window and blocked site data both throw here. The alarms still
    // work in memory for this page view, which is the case that matters.
  }
}

function forget(key: string): void {
  store().delete(key);
  clearTimers(key);
}

// --------------------------------------------------------------------- firing

function clearTimers(key: string): void {
  for (const handle of timers.get(key) ?? []) clearTimeout(handle);
  timers.delete(key);
}

let audio: AudioContext | null = null;

/**
 * Create and resume the audio context, which only works inside a user gesture.
 *
 * A context built anywhere else starts suspended and stays that way, so the
 * reminder would be silent with nothing to show for it. The "set" button press
 * is the gesture, and it is the one moment the reader is certainly there.
 */
function unlockAudio(): void {
  if (audio === null) {
    const extended = window as unknown as { webkitAudioContext?: typeof AudioContext };
    const Ctor = window.AudioContext ?? extended.webkitAudioContext;
    if (Ctor === undefined) return;
    try {
      audio = new Ctor();
    } catch {
      audio = null;
      return;
    }
  }
  void audio.resume().catch(() => undefined);
}

/**
 * The in-page cue: two short sine blips through a gain envelope.
 *
 * Synthesised rather than shipped as a file because it costs no bytes in the
 * bundle and there is no asset to go missing, and because it needs no
 * permission, which makes it the only part of a reminder that still works when
 * the browser is refusing notifications. The envelope is there because a bare
 * oscillator starting and stopping clicks.
 */
function chime(): void {
  const ctx = audio;
  if (ctx === null || ctx.state !== 'running') return;
  const tones = [880, 1175];
  let index = 0;
  for (const hz of tones) {
    const at = ctx.currentTime + index * 0.18;
    const osc = ctx.createOscillator();
    const gain = ctx.createGain();
    osc.type = 'sine';
    osc.frequency.value = hz;
    gain.gain.setValueAtTime(0.0001, at);
    gain.gain.exponentialRampToValueAtTime(0.2, at + 0.02);
    gain.gain.exponentialRampToValueAtTime(0.0001, at + 0.16);
    osc.connect(gain);
    gain.connect(ctx.destination);
    osc.start(at);
    osc.stop(at + 0.18);
    index += 1;
  }
}

function notificationsAvailable(): boolean {
  return typeof Notification !== 'undefined';
}

function permission(): NotificationPermission | null {
  return notificationsAvailable() ? Notification.permission : null;
}

function iconUrl(): string {
  try {
    return new URL('icon-180.png', location.href).href;
  } catch {
    return '';
  }
}

function show(title: string, body: string, tag: string): void {
  if (permission() !== 'granted') return;
  try {
    // The tag is the trip key, so a re-armed reminder replaces the one already
    // on screen instead of stacking a second card for the same vehicle.
    new Notification(title, { body, silent: false, tag, icon: iconUrl() });
  } catch {
    // Chrome on Android refuses a page-constructed notification and wants a
    // service worker registration instead. The audio cue still plays, so the
    // reminder is degraded rather than lost.
  }
}

function fire(alarm: Alarm, lead: number, now: number): void {
  alarm.fired.push(lead);
  alarm.leads = alarm.leads.filter((entry) => entry !== lead);
  const untilTarget = Math.max(0, Math.round((alarm.targetMs - now) / MINUTE_MS));
  const body =
    alarm.walkMinutes > 0
      ? `Leave in ${untilTarget} min; it departs in ${untilTarget + alarm.walkMinutes} min.`
      : `Departs in ${untilTarget} min.`;
  show(`${alarm.line} to ${alarm.destination}`, body, alarm.key);
  chime();
}

/**
 * Fire whatever is due, then set a timer for whatever is not.
 *
 * Both halves run on every refresh. The catch-up half is what makes a dropped
 * timer survivable: a reminder more than `CATCHUP_MS` late is retired quietly,
 * because a nudge that arrives after the vehicle has gone is worse than none.
 */
function schedule(alarm: Alarm, now: number): void {
  clearTimers(alarm.key);
  const handles: number[] = [];
  for (const lead of [...alarm.leads]) {
    const at = alarm.targetMs - lead * MINUTE_MS;
    const delay = at - now;
    if (delay <= 0) {
      if (delay > -CATCHUP_MS) fire(alarm, lead, now);
      else {
        alarm.fired.push(lead);
        alarm.leads = alarm.leads.filter((entry) => entry !== lead);
      }
      continue;
    }
    handles.push(
      window.setTimeout(() => {
        fire(alarm, lead, Date.now());
        persist();
        refreshPopup();
      }, delay),
    );
  }
  if (handles.length > 0) timers.set(alarm.key, handles);
}

// ------------------------------------------------------------------ re-arming

/**
 * Re-point every pending alarm at its trip's current departure, and forget the
 * ones that no longer have one. The page calls this after each refresh.
 *
 * Absence is read carefully, because an empty board is ambiguous: a failed
 * fetch and a cancelled trip look identical from here. A trip counts as gone
 * only when its own stop answered with rows reaching past its scheduled time,
 * which is exactly the case where it should have been listed and was not.
 */
export function rearm(boards: Board[], now: number): void {
  const pending = store();
  const seen = new Map<string, Departure>();
  const horizonByStop = new Map<string, number>();
  walkSeeds.clear();

  for (const board of boards) {
    for (const dep of board.departures) {
      const key = tripKey(dep);
      seen.set(key, dep);
      walkSeeds.set(key, walkMinutesFor(board, dep.stop));
      const far = horizonByStop.get(dep.stop);
      if (far === undefined || dep.planned > far) horizonByStop.set(dep.stop, dep.planned);
    }
  }

  for (const alarm of [...pending.values()]) {
    const dep = seen.get(alarm.key);

    if (dep !== undefined) {
      if (dep.cancelled) {
        show(`${alarm.line} to ${alarm.destination}`, 'Cancelled. The reminder is off.', alarm.key);
        chime();
        forget(alarm.key);
        continue;
      }
      // The headsign is sometimes corrected between refreshes, and the
      // notification should quote what the board is showing now.
      alarm.destination = dep.destination;
      alarm.targetMs = dep.realtime - alarm.walkMinutes * MINUTE_MS;
    } else {
      const far = horizonByStop.get(alarm.stop);
      if (far !== undefined && far >= alarm.planned) {
        forget(alarm.key);
        continue;
      }
    }

    if (alarm.targetMs + DROP_GRACE_MS < now) {
      forget(alarm.key);
      continue;
    }
    schedule(alarm, now);
  }

  persist();
  refreshPopup();
}

// -------------------------------------------------------------- the row marker

/** A one-line account of a pending alarm, for the marker's tooltip. */
function describe(alarm: Alarm): string {
  const leads = [...alarm.leads].sort((a, b) => a - b);
  const when = leads.length === 0 ? 'no reminders left' : `${leads.join(', ')} min before`;
  return alarm.walkMinutes > 0
    ? `Reminder ${when} you should leave (${alarm.walkMinutes} min walk)`
    : `Reminder ${when} it departs`;
}

/**
 * The bell for a row that has an alarm on it, or nothing.
 *
 * One element or `null`, never a fragment: the row renderer drops this into a
 * fixed grid slot, and a fragment would spill its children across the columns.
 */
export function alarmMarker(dep: Departure): HTMLElement | null {
  const key = tripKey(dep);
  const alarm = store().get(key);
  if (alarm === undefined) return null;
  const node = button('alarm-marker', '\u{1F514}', describe(alarm));
  node.setAttribute('aria-label', describe(alarm));
  node.addEventListener('click', (event) => {
    // The bell is the short route back to the popup, which is where cancelling
    // lives. Stopping the event keeps it out of whatever the row itself does.
    event.preventDefault();
    event.stopPropagation();
    showPopup(dep, walkSeeds.get(key) ?? alarm.walkMinutes, node);
  });
  return node;
}

// ------------------------------------------------------------- the long press

/**
 * Open something on a long press of `node`, by touch or by mouse.
 *
 * Pointer events rather than separate touch and mouse handling: one code path
 * means the cancel-on-move rule cannot drift between the two, and it is the
 * rule that decides whether the page is usable while scrolling. A press that
 * became a popup swallows the click that follows it, so a long press never also
 * reads as a tap.
 */
export function attachLongPress(node: HTMLElement, open: () => void): void {
  let timer: number | null = null;
  let origin: { x: number; y: number } | null = null;
  let fired = false;

  const cancel = (): void => {
    if (timer !== null) {
      clearTimeout(timer);
      timer = null;
    }
    origin = null;
  };

  node.addEventListener('pointerdown', (event: PointerEvent) => {
    if (event.button !== 0) return;
    cancel();
    fired = false;
    origin = { x: event.clientX, y: event.clientY };
    timer = window.setTimeout(() => {
      timer = null;
      fired = true;
      open();
    }, LONG_PRESS_MS);
  });

  node.addEventListener('pointermove', (event: PointerEvent) => {
    if (origin === null) return;
    if (
      Math.abs(event.clientX - origin.x) > MOVE_TOLERANCE_PX ||
      Math.abs(event.clientY - origin.y) > MOVE_TOLERANCE_PX
    ) {
      cancel();
    }
  });

  node.addEventListener('pointerup', cancel);
  node.addEventListener('pointercancel', cancel);
  node.addEventListener('pointerleave', cancel);

  node.addEventListener(
    'click',
    (event) => {
      if (!fired) return;
      fired = false;
      event.preventDefault();
      event.stopPropagation();
    },
    true,
  );

  node.addEventListener('contextmenu', (event) => {
    // Android raises this in the middle of the press and would put the system
    // menu over the popup. A right-click with no press in progress is left
    // alone, so the desktop keeps its menu and its copy command: people do copy
    // destinations and times off this page.
    if (fired || timer !== null) event.preventDefault();
  });
}

// ------------------------------------------------------------------ the popup

interface Popup {
  node: HTMLElement;
  key: string;
  /** Redraws the parts that go stale while the popup sits open. */
  refresh: () => void;
  close: () => void;
}

let popup: Popup | null = null;

function closePopup(): void {
  popup?.close();
}

function refreshPopup(): void {
  popup?.refresh();
}

/**
 * Put the popup where it can be read: under the row, above it when there is no
 * room below, centred on a narrow screen, and never past a viewport edge. The
 * geometry is inline because it is computed per opening; everything about how
 * the popup looks belongs to the stylesheet.
 */
function place(node: HTMLElement, anchor: HTMLElement): void {
  const rect = anchor.getBoundingClientRect();
  const width = node.offsetWidth;
  const height = node.offsetHeight;
  const vw = window.innerWidth;
  const vh = window.innerHeight;

  let left = vw < NARROW_VIEWPORT_PX ? (vw - width) / 2 : rect.left;
  const room = Math.max(EDGE_MARGIN_PX, vw - width - EDGE_MARGIN_PX);
  left = Math.min(Math.max(EDGE_MARGIN_PX, left), room);

  let top = rect.bottom + EDGE_MARGIN_PX;
  if (top + height > vh - EDGE_MARGIN_PX) top = rect.top - height - EDGE_MARGIN_PX;
  if (top < EDGE_MARGIN_PX) top = Math.max(EDGE_MARGIN_PX, (vh - height) / 2);

  node.style.left = `${Math.round(left)}px`;
  node.style.top = `${Math.round(top)}px`;
  node.style.maxHeight = `${Math.max(0, vh - 2 * EDGE_MARGIN_PX)}px`;
}

function checkbox(className: string): HTMLInputElement {
  const node = document.createElement('input');
  node.type = 'checkbox';
  node.className = className;
  return node;
}

/**
 * The alarm popup for one row.
 *
 * Not a modal: nothing is trapped and nothing behind it is disabled, because
 * this is a small choice made on top of a board the reader is still reading. It
 * closes on an outside tap, on Escape, and on a scroll, which on a page that is
 * scrolled this much is the most common way to mean "never mind".
 */
function showPopup(dep: Departure, walkSeed: number, anchor: HTMLElement): void {
  closePopup();

  const key = tripKey(dep);
  const existing = store().get(key);
  const node = el('div', 'alarm-popup');
  node.setAttribute('role', 'dialog');
  node.setAttribute('aria-label', `Reminder for ${dep.line} to ${dep.destination}`);
  // Anchoring needs the box out of the document flow, and the placement above
  // sets its coordinates in viewport units.
  node.style.position = 'fixed';

  node.append(el('div', 'alarm-popup-title', `${dep.line} to ${dep.destination}`));
  const when = el('div', 'alarm-popup-when');
  node.append(when);

  node.append(
    el(
      'p',
      'alarm-note',
      'Best effort: a reminder only fires while this page is open, and the browser may drop it if the tab sits in the background.',
    ),
  );

  const denied = el('p', 'alarm-denied', 'The browser is refusing notifications for this page, so only the sound will play.');
  denied.hidden = true;
  node.append(denied);

  // Walking time. The figure is seeded per alarm and never written back to
  // storage as a preference: a number remembered from last week would quietly
  // mis-arm every alarm after it.
  const walkRow = el('label', 'alarm-walk');
  const walkToggle = checkbox('alarm-walk-toggle');
  walkToggle.checked = existing !== undefined && existing.walkMinutes > 0;
  const walkInput = document.createElement('input');
  walkInput.type = 'number';
  walkInput.className = 'alarm-walk-input';
  walkInput.min = '0';
  walkInput.max = '180';
  walkInput.step = '1';
  walkInput.inputMode = 'numeric';
  walkInput.value = String(
    existing !== undefined && existing.walkMinutes > 0 ? existing.walkMinutes : Math.max(0, Math.round(walkSeed)),
  );
  walkInput.disabled = !walkToggle.checked;
  walkRow.append(walkToggle, el('span', 'alarm-walk-label', 'Keep walk time'), walkInput, el('span', 'alarm-walk-unit', 'min'));
  node.append(walkRow);

  const leadRow = el('div', 'alarm-leads');
  const leadBoxes = new Map<number, HTMLInputElement>();
  for (const lead of LEAD_CHOICES) {
    const label = el('label', 'alarm-lead');
    const box = checkbox('alarm-lead-box');
    const armed = existing !== undefined ? existing.leads.includes(lead) : lead === LEAD_DEFAULT;
    box.checked = armed;
    label.append(box, el('span', 'alarm-lead-text', `${lead} min`));
    leadRow.append(label);
    leadBoxes.set(lead, box);
  }
  node.append(leadRow);

  const status = el('p', 'alarm-status');
  node.append(status);

  const actions = el('div', 'alarm-actions');
  const set = button('alarm-set', existing === undefined ? 'Set reminder' : 'Update reminder');
  const drop = button('alarm-drop', 'Cancel reminder');
  drop.hidden = existing === undefined;
  const close = button('alarm-close', 'Close');
  actions.append(set, drop, close);
  node.append(actions);

  document.body.append(node);
  place(node, anchor);

  const walkNow = (): number => {
    if (!walkToggle.checked) return 0;
    const value = Number.parseInt(walkInput.value, 10);
    return Number.isFinite(value) && value > 0 ? Math.min(value, 180) : 0;
  };

  const refresh = (): void => {
    const alarm = store().get(key);
    const departs = alarm?.targetMs !== undefined ? alarm.targetMs + alarm.walkMinutes * MINUTE_MS : dep.realtime;
    const minutes = Math.max(0, Math.round((departs - Date.now()) / MINUTE_MS));
    when.textContent = minutes === 0 ? 'departing now' : `departs in ${minutes} min`;
    drop.hidden = alarm === undefined;
    set.textContent = alarm === undefined ? 'Set reminder' : 'Update reminder';
    denied.hidden = permission() !== 'denied';
  };
  refresh();

  const ticker = window.setInterval(refresh, 1_000);

  const onOutside = (event: Event): void => {
    const target = event.target;
    if (target instanceof Node && node.contains(target)) return;
    closePopup();
  };
  const onKey = (event: KeyboardEvent): void => {
    if (event.key === 'Escape') closePopup();
  };
  const onScroll = (): void => closePopup();

  // Capture, so a handler on a row cannot swallow the dismissal first.
  document.addEventListener('pointerdown', onOutside, true);
  document.addEventListener('keydown', onKey, true);
  window.addEventListener('scroll', onScroll, true);
  window.addEventListener('resize', onScroll);

  const dispose = (): void => {
    clearInterval(ticker);
    document.removeEventListener('pointerdown', onOutside, true);
    document.removeEventListener('keydown', onKey, true);
    window.removeEventListener('scroll', onScroll, true);
    window.removeEventListener('resize', onScroll);
    node.remove();
    if (popup?.node === node) popup = null;
  };

  walkToggle.addEventListener('change', () => {
    walkInput.disabled = !walkToggle.checked;
  });

  set.addEventListener('click', () => {
    // This click is the user gesture, and the only one the feature is
    // guaranteed. Both things that need one happen here: the audio context is
    // created and resumed, and permission is asked for the first time.
    unlockAudio();
    if (notificationsAvailable() && Notification.permission === 'default') {
      void Notification.requestPermission().then(() => refresh());
    }

    const now = Date.now();
    const chosen = LEAD_CHOICES.filter((lead) => leadBoxes.get(lead)?.checked === true);
    if (chosen.length === 0) {
      status.textContent = 'Pick at least one reminder time.';
      return;
    }

    const walk = walkNow();
    const target = dep.realtime - walk * MINUTE_MS;
    const future = chosen.filter((lead) => target - lead * MINUTE_MS > now);
    if (future.length === 0) {
      status.textContent = 'Every one of those moments has already passed.';
      return;
    }

    const alarm: Alarm = {
      key,
      line: dep.line,
      destination: dep.destination,
      stop: dep.stop,
      planned: dep.planned,
      targetMs: target,
      walkMinutes: walk,
      leads: [...future].sort((a, b) => a - b),
      fired: chosen.filter((lead) => !future.includes(lead)),
    };
    store().set(key, alarm);
    schedule(alarm, now);
    persist();

    // The confirming blip doubles as proof that the sound works, which for a
    // best-effort reminder is worth knowing before you stop watching the clock.
    chime();
    const dropped = chosen.length - future.length;
    status.textContent =
      `Reminder at ${alarm.leads.join(', ')} min before ${walk > 0 ? 'you leave' : 'it departs'}.` +
      (dropped > 0 ? ' Earlier ones had already passed.' : '');
    refresh();
  });

  drop.addEventListener('click', () => {
    forget(key);
    persist();
    status.textContent = 'Reminder cancelled.';
    refresh();
  });

  close.addEventListener('click', () => closePopup());

  popup = { node, key, refresh, close: dispose };
}

/**
 * Open the alarm popup for a row. The row renderer hands this to
 * `attachLongPress`, and the board supplies the walking time the popup starts
 * from.
 */
export function openAlarmPopup(dep: Departure, board: Board, anchor: HTMLElement): void {
  showPopup(dep, walkMinutesFor(board, dep.stop), anchor);
}

/** Whether a row currently has a reminder on it. */
export function hasAlarm(dep: Departure): boolean {
  return store().has(tripKey(dep));
}
