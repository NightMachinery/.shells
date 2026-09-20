import { icon } from './icons.ts';
import { normaliseLine } from '../filter.ts';
import type { Message } from '../model.ts';
import { button, el } from './dom.ts';
import { messageHash } from '../message-hash.ts';
import { readGeminiKey, writeGeminiKey } from './store.ts';
import type { TranslationShare } from './source.ts';
import {
  chromeTranslationReady,
  probeTranslationProviders,
  providerLabel,
  rememberedTranslation,
  rememberShared,
  sourceOfProvider,
  TARGET_LANGUAGE,
  translate,
  translationProviders,
  uiLanguageDiffers,
  type Translation,
} from './translate.ts';

// The disruptions panel: the operator's service messages, narrowed to the lines
// this profile actually rides, and with the four things a reader wants to do to
// them — fold them away, filter them, silence one, and read them in English.
//
// Every piece of state here is module-level rather than DOM state, for one
// reason that applies to all of it: the page rebuilds itself from scratch on
// every refresh, so anything held in an element is lost twice a minute. A
// `<details>` that snapped shut, a chip that unselected itself and a settings
// box that emptied while the reader was typing into it would each be worse than
// not having the feature at all.

/**
 * The panel is collapsed until the reader opens it.
 *
 * These notices run to several paragraphs each and the operator posts one per
 * affected section, so rendering them open pushes every board below the fold on
 * a phone, which is the opposite of what the page is for. The summary says how
 * many there are and which lines they concern, which is enough to decide
 * whether to open it.
 */
let panelOpen = false;

/** Which line chips are selected. Empty means "all", not "none". */
const selectedLines = new Set<string>();

/** Whether acknowledged messages are currently being shown anyway. */
let showAcknowledged = false;

/**
 * Message hashes the reader has asked to see in the original.
 *
 * Per message rather than a panel-wide switch, because the reason to want the
 * original is a specific one: a stop name or a line label that came out wrong,
 * in one notice. Switching the whole panel back would hide the nine
 * translations that were fine.
 */
const showOriginal = new Set<string>();

/** True once the automatic pass has run, so it runs once per load and not per paint. */
let autoTranslated = false;

/** True while a translation run is in flight, so the control can say so. */
let translating = false;

/**
 * The identity of each message text, resolved ahead of time.
 *
 * Hashing goes through `crypto.subtle` and is therefore asynchronous, while a
 * repaint is synchronous and cannot await anything. So `primeMessageState`
 * resolves every hash the next render will need and parks it here, and the
 * render path only ever reads this map.
 *
 * The rule for the hash itself is in `message-hash.ts`, because the planning
 * server keys the shared translation store by the same thing and the two
 * cannot be allowed to drift.
 */
const hashes = new Map<string, string>();

/**
 * Where translations are shared, parked here for the same reason as everything
 * else in this file: the repaint path is synchronous and the buttons on it fire
 * long after the call that knew about the source. It is set by
 * `primeMessageState`, which is called before anything renders.
 *
 * Null until then, and it stays effectively null on a page with no planning
 * server, where sharing is a pair of calls that find nothing and offer nowhere.
 */
let sharing: TranslationShare | null = null;

/**
 * Hashes this client has already offered, so a repaint or a second run does not
 * offer the same translation again. The server would take it happily, since the
 * write is idempotent, but it is a request that buys nothing.
 */
const offered = new Set<string>();

/**
 * Acknowledgements live under one key per message hash rather than one list.
 *
 * The format stays in this file because nothing else has any business knowing
 * it: a per-key layout means two tabs acknowledging different messages cannot
 * overwrite each other's work, which a single JSON list read-modify-written by
 * both would.
 */
const ACK_PREFIX = 'transit.ack.';

/** How long an acknowledgement of a message that is no longer posted is kept. */
const ACK_TTL_MS = 30 * 24 * 60 * 60_000;

/** How long the copy button shows its result before going back to its label. */
const COPY_NOTICE_MS = 1_800;

/**
 * The instruction the copied block opens with. Named here rather than built at
 * the call site because the wording is the feature: it is what makes the paste
 * usable in a chat window with no further typing.
 */
const COPY_PROMPT =
  'Translate the following Munich public-transport service messages to English, keeping line names and times:';

// --------------------------------------------------------------- persistence

function ackKey(hash: string): string {
  return `${ACK_PREFIX}${hash}`;
}

/**
 * Every storage access is wrapped, because a private window, blocked site data
 * and a browser that has evicted the origin all throw here, and none of them is
 * a reason for the panel not to render. A failed read is "not acknowledged" and
 * a failed write is forgotten.
 */
function isAcknowledged(hash: string | undefined): boolean {
  if (hash === undefined) return false;
  try {
    return localStorage.getItem(ackKey(hash)) !== null;
  } catch {
    return false;
  }
}

function acknowledge(hash: string): void {
  try {
    localStorage.setItem(ackKey(hash), String(Date.now()));
  } catch {
    /* see the note on isAcknowledged */
  }
}

function unacknowledge(hash: string): void {
  try {
    localStorage.removeItem(ackKey(hash));
  } catch {
    /* see the note on isAcknowledged */
  }
}

/**
 * Drop acknowledgements of messages that are no longer posted and are older
 * than the retention window.
 *
 * Without this the key count grows for the life of the browser profile, one key
 * per notice ever silenced. A message that is still posted is never pruned no
 * matter how old the acknowledgement is, so a standing notice someone silenced
 * in the spring does not come back in the autumn.
 */
function forgetStaleAcknowledgements(current: ReadonlySet<string>): void {
  const cutoff = Date.now() - ACK_TTL_MS;
  try {
    const stale: string[] = [];
    for (let index = 0; index < localStorage.length; index += 1) {
      const key = localStorage.key(index);
      if (key === null || !key.startsWith(ACK_PREFIX)) continue;
      if (current.has(key.slice(ACK_PREFIX.length))) continue;
      const at = Number.parseInt(localStorage.getItem(key) ?? '', 10);
      if (Number.isFinite(at) && at > cutoff) continue;
      stale.push(key);
    }
    for (const key of stale) localStorage.removeItem(key);
  } catch {
    /* see the note on isAcknowledged */
  }
}

// ------------------------------------------------------------------ selection

/**
 * Relevance: a message is relevant when it names a line this profile
 * configures. A profile that configures no lines at all asks for everything its
 * stops serve, so it gets every message too.
 */
function relevantMessages(messages: Message[], wanted: ReadonlySet<string>): Message[] {
  if (wanted.size === 0) return messages;
  return messages.filter((message) => message.lines.some((line) => wanted.has(normaliseLine(line))));
}

/** The lines the relevant messages are about, normalised key to the operator's spelling. */
function affectedLines(messages: Message[], wanted: ReadonlySet<string>): Map<string, string> {
  const affected = new Map<string, string>();
  for (const message of messages) {
    for (const line of message.lines) {
      const key = normaliseLine(line);
      if (wanted.size > 0 && !wanted.has(key)) continue;
      if (!affected.has(key)) affected.set(key, line);
    }
  }
  return new Map([...affected.entries()].sort((a, b) => a[0].localeCompare(b[0])));
}

/**
 * Whether a message survives the chip selection.
 *
 * A message that names no line at all is network-wide, so no line chip can
 * exclude it: filtering to the U-Bahn should not hide "no service anywhere this
 * evening".
 */
function matchesChips(message: Message): boolean {
  if (selectedLines.size === 0) return true;
  if (message.lines.length === 0) return true;
  return message.lines.some((line) => selectedLines.has(normaliseLine(line)));
}

// ------------------------------------------------------------------ rendering

/** The validity window in the device's own zone, which is the only one this module knows. */
const WHEN_FORMAT = new Intl.DateTimeFormat('en-GB', {
  day: '2-digit',
  month: 'short',
  hour: '2-digit',
  minute: '2-digit',
  hour12: false,
});

function describeValidity(message: Message): string | null {
  const from = typeof message.validFrom === 'number' ? message.validFrom : null;
  const to = typeof message.validTo === 'number' ? message.validTo : null;
  if (from !== null && to !== null) return `from ${WHEN_FORMAT.format(from)} until ${WHEN_FORMAT.format(to)}`;
  if (from !== null) return `from ${WHEN_FORMAT.format(from)}`;
  if (to !== null) return `until ${WHEN_FORMAT.format(to)}`;
  return null;
}

/**
 * Who a translation is owed to, in the line under it.
 *
 * Worth saying, because the reader's judgement of a translation depends on it.
 * One produced here came from a provider this reader chose and can turn off;
 * one that came from the shared store was made by somebody else's browser, and
 * whether that was an on-device translator or Gemini is the difference between
 * "probably fine" and "probably right", on text full of line names and platform
 * numbers where the cheap translators come apart.
 */
function creditFor(translation: Translation): string {
  if (translation.from === undefined) return 'translated on this device';
  // The planning server's own is not another client and is not somebody else's
  // phone: it is a proper translation API with a credential the server holds,
  // and it is the best answer anybody here is going to get.
  if (translation.from === 'google') return 'translated by server';
  return `translated by another client · ${translation.from}`;
}

/** One message as plain text, for the clipboard. */
function forCopy(message: Message): string {
  const parts = [message.title];
  if (message.lines.length > 0) parts.push(`Lines: ${message.lines.join(' ')}`);
  const when = describeValidity(message);
  if (when !== null) parts.push(when);
  parts.push(message.text);
  return parts.join('\n');
}

async function copyAll(messages: Message[], node: HTMLButtonElement): Promise<void> {
  const label = node.textContent ?? 'Copy all';
  const payload = `${COPY_PROMPT}\n\n${messages.map(forCopy).join('\n\n')}`;
  try {
    // `navigator.clipboard` is absent on an insecure origin, and `writeText`
    // rejects when the document is not focused or the permission was refused.
    // The confirmation lives in the button because a full repaint is due within
    // the second and would carry away anything rendered elsewhere.
    await navigator.clipboard.writeText(payload);
    node.textContent = 'Copied';
  } catch {
    node.textContent = 'Copy failed';
    node.setAttribute('aria-label', 'Copy failed: the clipboard was refused, this needs a focused page on a secure origin');
  }
  window.setTimeout(() => {
    node.textContent = label;
  }, COPY_NOTICE_MS);
}

async function translateAll(messages: Message[], onChange: () => void): Promise<void> {
  translating = true;
  onChange();
  try {
    // One at a time, with a repaint after each, so the translations appear as
    // they land rather than all at the end of a run that may take a while over
    // a phone connection.
    for (const message of messages) {
      const produced = await translate(message.text);
      onChange();
      void offerTranslation(message, produced);
    }
  } finally {
    translating = false;
    onChange();
  }
}

/**
 * Offer one translation this device produced to the next reader.
 *
 * Not awaited by the loop above, because nothing on screen is waiting for it:
 * the translation is already rendered and the offer is a favour to somebody
 * else's phone. Every reason not to offer is silent, and none of them is an
 * error the reader is shown.
 *
 * Only what this device produced. A translation that arrived FROM the store
 * carries the client kind that made it, and offering that back would let a
 * browser translation this page received get re-asserted under this page's own
 * name, which is exactly what the store's precedence rule exists to prevent.
 */
async function offerTranslation(message: Message, translation: Translation | null): Promise<void> {
  if (translation === null || translation.from !== undefined || sharing === null) return;
  const hash = hashes.get(message.text);
  if (hash === undefined || offered.has(hash)) return;
  offered.add(hash);
  await sharing.shareTranslation({
    hash,
    lang: TARGET_LANGUAGE,
    source: sourceOfProvider(translation.provider),
    text: translation.text,
    original_length: message.text.length,
  });
}

// ------------------------------------------------------- the settings popover

/**
 * The key box hangs off `document.body` rather than off the panel.
 *
 * Inside the panel it would be destroyed by the next repaint, which lands every
 * thirty seconds, and a forty-character key cannot be typed in that. Outside the
 * re-rendered tree it simply stays put. Nothing is trapped and nothing behind it
 * is disabled: it closes on Escape, on an outside tap, and on either button.
 */
let keyBox: { node: HTMLElement; close: () => void } | null = null;

function closeKeyBox(): void {
  keyBox?.close();
}

function openKeyBox(onChange: () => void): void {
  closeKeyBox();

  const node = el('div', 'translate-settings');
  node.setAttribute('role', 'dialog');
  node.setAttribute('aria-label', 'Translation settings');
  node.append(el('p', 'translate-settings-title', 'Translate with Gemini'));
  node.append(
    el(
      'p',
      'translate-settings-note',
      'The key is kept in this browser and is sent nowhere except to Google when you ask for a translation.',
    ),
  );

  const input = document.createElement('input');
  input.type = 'password';
  input.className = 'translate-key';
  input.placeholder = 'Gemini API key';
  input.autocomplete = 'off';
  input.spellcheck = false;
  node.append(input);

  // The saved key is never written back into the box, not even as dots: the
  // page has no reason to hand a credential back out once it holds one, so
  // presence is reported in words and replacing it means typing it again.
  const status = el('p', 'translate-settings-status');
  const writeStatus = (): void => {
    status.textContent = readGeminiKey() === null ? 'No key saved here yet.' : 'A key is saved in this browser.';
  };
  writeStatus();
  node.append(status);

  const actions = el('div', 'translate-settings-actions');
  const save = button('translate-save', 'Save');
  const forget = button('translate-forget', 'Forget key');
  const close = button('translate-close', 'Close');
  forget.hidden = readGeminiKey() === null;
  actions.append(save, forget, close);
  node.append(actions);

  const onOutside = (event: Event): void => {
    const target = event.target;
    if (target instanceof Node && node.contains(target)) return;
    closeKeyBox();
  };
  const onKey = (event: KeyboardEvent): void => {
    if (event.key === 'Escape') closeKeyBox();
  };
  // Capture, so a handler on the panel cannot swallow the dismissal first.
  document.addEventListener('pointerdown', onOutside, true);
  document.addEventListener('keydown', onKey, true);

  const dispose = (): void => {
    document.removeEventListener('pointerdown', onOutside, true);
    document.removeEventListener('keydown', onKey, true);
    node.remove();
    if (keyBox?.node === node) keyBox = null;
  };

  save.addEventListener('click', () => {
    // An empty box means "never mind", not "delete what is stored". Dropping a
    // key takes the button that says so.
    if (input.value.trim().length > 0) writeGeminiKey(input.value);
    input.value = '';
    dispose();
    onChange();
  });
  forget.addEventListener('click', () => {
    writeGeminiKey('');
    input.value = '';
    writeStatus();
    forget.hidden = true;
    onChange();
  });
  close.addEventListener('click', () => dispose());

  document.body.append(node);
  input.focus();
  keyBox = { node, close: dispose };
}

// -------------------------------------------------------------------- surface

/**
 * The disruptions panel, or `null` when this profile has nothing to show.
 *
 * `configuredLines` is the set of line labels the visible profile names; it is
 * normalised here rather than at the call site so the caller can pass the
 * configuration's own spellings.
 */
export function renderMessages(
  messages: Message[],
  configuredLines: ReadonlySet<string>,
  onChange: () => void,
): HTMLElement | null {
  if (messages.length === 0) return null;
  const wanted = new Set([...configuredLines].map(normaliseLine));
  const relevant = relevantMessages(messages, wanted);
  if (relevant.length === 0) return null;

  const affected = affectedLines(relevant, wanted);
  // A line that has stopped being affected cannot stay selected, or the reader
  // is left staring at an empty list with no chip on screen explaining why.
  for (const line of [...selectedLines]) if (!affected.has(line)) selectedLines.delete(line);

  const chosen = relevant.filter(matchesChips);
  const hiddenCount = chosen.filter((message) => isAcknowledged(hashes.get(message.text))).length;
  const visible = showAcknowledged ? chosen : chosen.filter((message) => !isAcknowledged(hashes.get(message.text)));

  const panel = document.createElement('details');
  panel.className = 'disruptions';
  panel.open = panelOpen;
  panel.addEventListener('toggle', () => {
    // A repaint replaces this element, and the detached one can still deliver a
    // queued toggle. Reading its state then would hand the panel the answer of
    // a node nobody is looking at any more.
    if (panel.isConnected) panelOpen = panel.open;
  });

  const summary = document.createElement('summary');
  summary.className = 'disruptions-summary';
  summary.append(el('strong', 'disruptions-count', visible.length === 1 ? '1 service message' : `${visible.length} service messages`));
  const lineList = [...affected.values()];
  if (lineList.length > 0) summary.append(el('span', 'disruption-lines', lineList.join(' ')));
  if (hiddenCount > 0) {
    summary.append(el('span', 'disruptions-hidden', `${hiddenCount} acknowledged`));
    const restore = button('disruptions-restore', showAcknowledged ? 'hide again' : 'show');
    restore.setAttribute(
      'aria-label',
      `${showAcknowledged ? 'hide again' : 'show'}: acknowledged messages stay out of the list until their text changes`,
    );
    restore.addEventListener('click', (event) => {
      // Both, because a click anywhere inside a summary otherwise folds the
      // panel as well as pressing the button.
      event.preventDefault();
      event.stopPropagation();
      showAcknowledged = !showAcknowledged;
      onChange();
    });
    summary.append(restore);
  }
  panel.append(summary);

  if (affected.size > 1) {
    const chips = el('div', 'disruptions-chips');
    const all = button(`disruptions-chip${selectedLines.size === 0 ? ' active' : ''}`, 'all');
    all.addEventListener('click', () => {
      selectedLines.clear();
      onChange();
    });
    chips.append(all);
    for (const [key, label] of affected) {
      const chip = button(`disruptions-chip${selectedLines.has(key) ? ' active' : ''}`, label);
      chip.addEventListener('click', () => {
        if (selectedLines.has(key)) selectedLines.delete(key);
        else selectedLines.add(key);
        onChange();
      });
      chips.append(chip);
    }
    panel.append(chips);
  }

  const actions = el('div', 'disruptions-actions');
  const copy = button('disruptions-copy', 'Copy all');
  copy.setAttribute('aria-label', 'Copy all: copy every message shown here, ready to paste into a translator');
  copy.disabled = visible.length === 0;
  copy.addEventListener('click', () => void copyAll(visible, copy));
  actions.append(copy);

  const providers = translationProviders();
  const untranslated = visible.filter((message) => {
    const hash = hashes.get(message.text);
    return hash === undefined || rememberedTranslation(hash) === null;
  });
  const primary = providers[0];
  if (primary === undefined) {
    const setup = button('disruptions-translate', 'Translate…');
    setup.setAttribute('aria-label', 'Translate…: no translator is available here yet');
    setup.addEventListener('click', () => openKeyBox(onChange));
    actions.append(setup);
  } else {
    const run = button('disruptions-translate', translating ? 'Translating…' : `Translate with ${providerLabel(primary)}`);
    run.disabled = translating || untranslated.length === 0;
    run.setAttribute(
      'aria-label',
      `${translating ? 'Translating…' : `Translate with ${providerLabel(primary)}`}: ${
        untranslated.length === 0 ? 'everything shown here is already translated' : `${untranslated.length} still to translate`
      }`,
    );
    run.addEventListener('click', () => void translateAll(untranslated, onChange));
    actions.append(run);
  }

  const settings = button('disruptions-settings');
  settings.append(icon('gear'));
  settings.setAttribute('aria-label', 'translation settings');
  settings.addEventListener('click', () => openKeyBox(onChange));
  actions.append(settings);
  panel.append(actions);

  if (visible.length === 0) {
    panel.append(el('p', 'disruptions-note', hiddenCount > 0 ? 'everything here is acknowledged' : 'nothing matches these lines'));
    return panel;
  }

  for (const message of visible) {
    const hash = hashes.get(message.text);
    const acknowledged = isAcknowledged(hash);
    const item = el('div', `disruption${acknowledged ? ' disruption-acknowledged' : ''}`);
    item.append(el('strong', 'disruption-title', message.title));
    if (message.lines.length > 0) item.append(el('span', 'disruption-lines', message.lines.join(' ')));
    const when = describeValidity(message);
    if (when !== null) item.append(el('span', 'disruption-when', when));
    const hasTranslation = hash !== undefined && rememberedTranslation(hash) !== null;
    if (!hasTranslation) item.append(el('p', 'disruption-text', message.text));

    const translation = hash === undefined ? null : rememberedTranslation(hash);
    if (translation !== null && hash !== undefined) {
      // The translation is the body once there is one, with the original one tap
      // away. A reader who wanted the German would be reading the operator's own
      // site; a reader here has already said, by the language their browser
      // speaks, which one they want first.
      const original = showOriginal.has(hash);
      const box = el('p', 'disruption-translation', original ? message.text : translation.text);
      const credit = el('span', 'disruption-provider', original ? 'original' : creditFor(translation));
      // Which provider actually produced it stays available without spending a
      // line on it: the credit answers "can I trust this", and the provider
      // name answers "which one got it wrong", which is a rarer question. Only
      // for one this device made, because that is the only case where the
      // provider is a thing this page knows rather than guesses.
      if (!original && translation.from === undefined) {
        credit.setAttribute('aria-label', `${creditFor(translation)}: ${providerLabel(translation.provider)}`);
      }
      box.append(credit);
      item.append(box);
      const toggle = button('disruption-original', original ? 'show translation' : 'original');
      toggle.setAttribute(
        'aria-label',
        `${original ? 'show translation' : 'original'}: ${original ? 'show the translated text again' : 'show the operator’s own words'}`,
      );
      toggle.addEventListener('click', () => {
        if (original) showOriginal.delete(hash);
        else showOriginal.add(hash);
        onChange();
      });
      item.append(toggle);
    }

    if (hash !== undefined) {
      const ack = button('disruption-ack', acknowledged ? 'bring back' : 'got it');
      ack.setAttribute(
        'aria-label',
        `${acknowledged ? 'bring back' : 'got it'}: ${
          acknowledged ? 'show this message in the list again' : 'hide this message until its text changes'
        }`,
      );
      ack.addEventListener('click', () => {
        if (acknowledged) unacknowledge(hash);
        else acknowledge(hash);
        onChange();
      });
      item.append(ack);
    }
    panel.append(item);
  }
  return panel;
}

/** Forget the chip selection, for when the reader switches to another profile. */
export function resetMessageFilters(): void {
  selectedLines.clear();
  showAcknowledged = false;
}

/**
 * Resolve everything the next render needs but cannot await: the hash of every
 * message text, which is what an acknowledgement, a cached translation and a
 * shared one are all keyed by.
 *
 * `share` is where translations are pooled between the browsers reading this
 * page. It is awaited, unlike the provider probe below, because the whole
 * point is to render a translation somebody else already made INSTEAD of
 * starting one here, and a lookup that lands after the automatic pass has
 * already begun has saved nobody anything. It is one request against a server
 * on the same machine as the page, so the wait is the wait of a local round
 * trip; with no server it does not happen at all.
 */
export async function primeMessageState(messages: Message[], share: TranslationShare): Promise<void> {
  sharing = share;
  const pending = [...new Set(messages.map((message) => message.text))].filter((text) => !hashes.has(text));
  const resolved = await Promise.all(pending.map(async (text) => [text, await messageHash(text)] as const));
  for (const [text, hash] of resolved) hashes.set(text, hash);

  // An empty list is what a failed fetch looks like, and pruning against it
  // would throw away acknowledgements of messages that are still posted.
  if (messages.length > 0) {
    forgetStaleAcknowledgements(new Set(messages.map((message) => hashes.get(message.text) ?? '')));
  }

  await adoptSharedTranslations(messages);

  // Not awaited: the probe only sharpens an answer the panel already has a
  // provisional version of, and the first paint should not wait on the browser
  // deciding whether it can download a language pack. The next repaint, thirty
  // seconds away at most, picks up the verdict.
  void probeTranslationProviders();
}

/**
 * Ask the shared store about every notice on screen, and take what it has.
 *
 * Only the ones nothing is known about locally: the IndexedDB cache stays the
 * layer in front of this, so a notice this browser has already translated, or
 * already picked up from the store on an earlier visit, costs no request at
 * all. What arrives is written into that same cache, keyed the same way, so the
 * next visit does not ask again either.
 *
 * Never throws. The store is an optimisation on an optimisation, and a page
 * whose panel failed to render because a cache lookup went wrong would be a bad
 * trade at any price.
 */
async function adoptSharedTranslations(messages: Message[]): Promise<void> {
  if (sharing === null || messages.length === 0) return;
  const wanted = new Set<string>();
  for (const message of messages) {
    const hash = hashes.get(message.text);
    if (hash === undefined || rememberedTranslation(hash) !== null) continue;
    wanted.add(hash);
  }
  if (wanted.size === 0) return;
  try {
    const found = await sharing.fetchTranslations(TARGET_LANGUAGE, [...wanted]);
    await Promise.all(
      Object.entries(found).map(async ([hash, entry]) => {
        if (!wanted.has(hash)) return;
        await rememberShared(hash, entry.text, entry.source);
        // Already there, so there is nothing to offer back. This also keeps a
        // page that only ever received translations from ever writing.
        offered.add(hash);
      }),
    );
  } catch {
    /* no server, or a server that did not answer: the panel translates as it always did */
  }
}

/**
 * Translate every notice, once per load, when the browser can do it on the
 * device and the reader is not already reading in the notices' own language.
 *
 * Automatic only for the on-device translator. It is free and nothing leaves
 * the machine, so doing it unasked costs the reader nothing and saves them a
 * tap on every visit. Gemini stays a button: it spends their money and sends
 * the text to Google, and neither is a thing to do on their behalf.
 */
export async function autoTranslate(messages: Message[], onChange: () => void): Promise<void> {
  if (autoTranslated || messages.length === 0) return;
  if (!uiLanguageDiffers()) return;
  await probeTranslationProviders();
  if (!chromeTranslationReady()) return;
  autoTranslated = true;
  const pending = messages.filter((message) => {
    const hash = hashes.get(message.text);
    return hash === undefined || rememberedTranslation(hash) === null;
  });
  await translateAll(pending, onChange);
}
