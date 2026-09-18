import type { BoardView } from './types.ts';

// Everything the page remembers between visits, in one place.
//
// Two stores with two jobs. `localStorage` holds the reader's small choices:
// which tab, how far ahead, which board is expanded, which lines are hidden,
// and the one credential the translation feature may be given. Those are tiny,
// needed synchronously during the first paint, and meaningless on another
// device. Bulk data goes to IndexedDB instead; see `idb.ts`.
//
// Every accessor swallows its errors. A private window, blocked site data and a
// browser that refuses storage all throw here, and the page has to work anyway,
// so a failed read is an absent preference and a failed write is forgotten.

const KEY_PROFILE = 'transit.profile';
const KEY_HORIZON = 'transit.horizon';
const KEY_VIEW = 'transit.view';
const KEY_FILTER = 'transit.filter';
const KEY_GEMINI = 'transit.geminiKey';
const KEY_DESTINATION = 'transit.destination';
const KEY_SORT = 'transit.sortByArrival';

function read(key: string): string | null {
  try {
    return localStorage.getItem(key);
  } catch {
    return null;
  }
}

function write(key: string, value: string): void {
  try {
    localStorage.setItem(key, value);
  } catch {
    /* see the note at the top of this file */
  }
}

function remove(key: string): void {
  try {
    localStorage.removeItem(key);
  } catch {
    /* see the note at the top of this file */
  }
}

export function readProfile(): string | null {
  return read(KEY_PROFILE);
}

export function writeProfile(key: string): void {
  write(KEY_PROFILE, key);
}

export function readHorizon(): number | null {
  const raw = read(KEY_HORIZON);
  if (raw === null) return null;
  const value = Number.parseInt(raw, 10);
  return Number.isFinite(value) && value > 0 ? value : null;
}

export function writeHorizon(minutes: number): void {
  write(KEY_HORIZON, String(minutes));
}

/**
 * The identity of a board for the purpose of remembering things about it.
 *
 * The title rather than the index, because a board's position moves whenever
 * one is added above it and a reader who collapsed "Bus to the station" means
 * that board, not the third one. Two boards in one profile sharing a title
 * would share their state, which is a fair reading of a configuration that
 * names two panels the same thing.
 */
export function boardId(profileKey: string, title: string): string {
  return `${profileKey}::${title}`;
}

export function readView(id: string): BoardView | null {
  const raw = read(`${KEY_VIEW}.${id}`);
  return raw === 'integrated' || raw === 'full' || raw === 'collapsed' ? raw : null;
}

export function writeView(id: string, view: BoardView): void {
  write(`${KEY_VIEW}.${id}`, view);
}

/**
 * Which (stop, line) pairs a board is hiding, as a set of `stop|line` keys.
 *
 * Stored as the hidden set rather than the shown set so that a line added to
 * the configuration later shows up by default. A reader who hid the 68 wants
 * the 68 hidden, not everything except what existed the day they chose.
 */
export function readHidden(id: string): Set<string> {
  const raw = read(`${KEY_FILTER}.${id}`);
  if (raw === null) return new Set();
  try {
    const parsed: unknown = JSON.parse(raw);
    return Array.isArray(parsed) ? new Set(parsed.filter((entry): entry is string => typeof entry === 'string')) : new Set();
  } catch {
    return new Set();
  }
}

export function writeHidden(id: string, hidden: ReadonlySet<string>): void {
  if (hidden.size === 0) {
    remove(`${KEY_FILTER}.${id}`);
    return;
  }
  write(`${KEY_FILTER}.${id}`, JSON.stringify([...hidden].sort()));
}

/** The key a (stop, line) pair is hidden under. */
export function filterKey(stop: string, line: string): string {
  return `${stop}|${line}`;
}

/**
 * Where the commute view plans to, remembered per profile because the answer is
 * a property of where you are standing: from a home profile you are going to
 * work, and from work you are going home.
 */
export function readDestination(profileKey: string): string | null {
  return read(`${KEY_DESTINATION}.${profileKey}`);
}

export function writeDestination(profileKey: string, destinationKey: string | null): void {
  if (destinationKey === null) remove(`${KEY_DESTINATION}.${profileKey}`);
  else write(`${KEY_DESTINATION}.${profileKey}`, destinationKey);
}

export function readSortByArrival(): boolean {
  return read(KEY_SORT) === '1';
}

export function writeSortByArrival(value: boolean): void {
  if (value) write(KEY_SORT, '1');
  else remove(KEY_SORT);
}

/**
 * The API key for the paid translation fallback.
 *
 * `localStorage` and nowhere else: it is the reader's own credential, it must
 * never reach the configuration file or either repository, and it is scoped to
 * this origin on this device. The page treats its absence as "that provider is
 * not available" rather than as an error, so nothing is ever sent without one.
 */
export function readGeminiKey(): string | null {
  const value = read(KEY_GEMINI);
  return value !== null && value.trim().length > 0 ? value.trim() : null;
}

export function writeGeminiKey(value: string): void {
  if (value.trim().length === 0) remove(KEY_GEMINI);
  else write(KEY_GEMINI, value.trim());
}
