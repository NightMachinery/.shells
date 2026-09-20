import { idbGet, idbSet, STORE_TRANSLATIONS } from './idb.ts';
import { messageHash } from '../message-hash.ts';
import { readGeminiKey } from './store.ts';
import type { ClientTranslationSource, TranslationSource } from './wire.ts';

// German service messages, turned into English by whichever provider is usable,
// with every result cached by the hash of the source text.
//
// Two providers, because neither one covers the whole audience. The browser's
// own Translator API is free, runs on the device and needs no credential, but
// it is desktop-only and ships in one browser family, which on a page built for
// a phone means it is usually absent. Gemini covers that gap, costs the reader
// money and needs the reader's own key, so it is opt-in and is never called
// without one: no key is not an error, it is a provider that does not exist.
//
// Content addressing is the point of the cache, not an implementation detail,
// and the rule for it lives in `message-hash.ts` because three things key off
// it and they run in different processes. See the note there.
//
// A third source of translations, after the two providers, is the planning
// server. It keeps whatever the browsers reading this page produced, so a notice
// somebody else paid Gemini to translate arrives here for nothing, and when the
// server has a translation credential of its own it will have translated the
// notice before anybody asked. Those are stored in the same cache as our own,
// marked with where they came from, so the panel can say so and so that we never
// offer somebody else's work back to the store as ours.

export type TranslationProvider = 'chrome' | 'gemini';

export interface Translation {
  text: string;
  provider: TranslationProvider;
  /**
   * Absent when this device produced it. Set to the kind of client that did,
   * when it arrived from the shared store.
   */
  from?: TranslationSource;
}

/**
 * What this page calls itself on the wire when it offers a translation.
 *
 * A client source, never the server's own: see the note on
 * `ClientTranslationSource` in `wire.ts` for why the narrower type is the point
 * rather than an accident.
 */
export function sourceOfProvider(provider: TranslationProvider): ClientTranslationSource {
  return provider === 'gemini' ? 'gemini' : 'browser';
}

/** How the provider is named to the reader. */
export function providerLabel(provider: TranslationProvider): string {
  return provider === 'chrome' ? 'Chrome' : 'Gemini';
}

const SOURCE_LANGUAGE = 'de';
/**
 * The language everything here translates into, and the one the shared store is
 * asked about. Exported because the store is keyed by it: a lookup for the
 * wrong language is a lookup that finds nothing, silently.
 */
export const TARGET_LANGUAGE = 'en';

/**
 * Whether translating is worth doing at all in this browser.
 *
 * It is not when the reader's own interface is already in the language the
 * notices are written in: translating German into English for somebody reading
 * the page in German is work nobody asked for, and the toggle it adds to every
 * message is clutter. `navigator.language` is what the browser was asked to
 * speak, which is the closest thing to "the UI language" a page can observe.
 */
export function uiLanguageDiffers(): boolean {
  const language = typeof navigator === 'undefined' ? '' : (navigator.language ?? '');
  return !language.toLowerCase().startsWith(SOURCE_LANGUAGE);
}

/**
 * Whether the browser's own translator is usable right now.
 *
 * Only the browser's, never Gemini: the on-device translator is free, private
 * and needs no credential, so running it without being asked costs the reader
 * nothing. Gemini costs them money and uses their key, so it stays a button
 * they press.
 */
export function chromeTranslationReady(): boolean {
  return chromeUsable === true && translatorFactory() !== null;
}

/**
 * The Gemini model, as one constant so it can be moved when Google retires a
 * name. The `-latest` alias tracks the current release of that variation, which
 * is what a page with no evaluation harness wants: it will never be pinned to a
 * model that has been withdrawn.
 */
const GEMINI_MODEL = 'gemini-flash-lite-latest';
const GEMINI_ENDPOINT = `https://generativelanguage.googleapis.com/v1beta/models/${GEMINI_MODEL}:generateContent`;

/**
 * The instruction carried in `systemInstruction` rather than glued in front of
 * the notice, so the model cannot read an imperative sentence inside a notice
 * as a change to its own job.
 */
const GEMINI_SYSTEM = [
  'Translate the German public-transport service message the user sends into English.',
  'Keep line names, stop names, platform numbers, dates and times exactly as written.',
  'Reply with the translation alone: no preamble, no notes, no quotation marks.',
].join(' ');

// ---------------------------------------------------------------------------
// The browser's Translator API
// ---------------------------------------------------------------------------

/**
 * The slice of the Translator API this page uses. It is read off `self` through
 * this shape rather than declared as a global, because the TypeScript DOM
 * library does not carry it yet and a `declare global` here would collide with
 * the real declaration the day it lands.
 */
interface TranslatorInstance {
  translate(input: string): Promise<string>;
}

interface TranslatorFactory {
  availability(options: { sourceLanguage: string; targetLanguage: string }): Promise<string>;
  create(options: { sourceLanguage: string; targetLanguage: string }): Promise<TranslatorInstance>;
}

function translatorFactory(): TranslatorFactory | null {
  const factory = (self as unknown as { Translator?: Partial<TranslatorFactory> }).Translator;
  if (factory === undefined || typeof factory.create !== 'function' || typeof factory.availability !== 'function') {
    return null;
  }
  return factory as TranslatorFactory;
}

/**
 * Three spellings of "no" because the availability enum was renamed while the
 * API was being standardised, and a browser in the field may answer with any of
 * them. Anything else, including `downloadable` and `downloading`, counts as
 * usable: the model downloads on first use and the reader waits once.
 */
const UNUSABLE = new Set(['unavailable', 'unsupported', 'no']);

/**
 * What the last availability probe concluded. `null` means nothing has probed
 * yet, and the presence of the factory is taken as a provisional yes so the
 * first paint can already offer the control.
 */
let chromeUsable: boolean | null = null;

/**
 * The one translator instance, created on demand.
 *
 * On demand rather than at load, because `create()` needs transient user
 * activation when the language pack has to be downloaded, and the page's first
 * paint has none. Every call into this provider comes from a button, so the
 * activation is there by the time it matters. A failed creation clears this
 * back to `null` instead of caching the failure, so a rejection that was only
 * a missing activation does not disable the provider for the session.
 */
let translatorReady: Promise<TranslatorInstance | null> | null = null;

function chromeTranslator(): Promise<TranslatorInstance | null> {
  if (translatorReady !== null) return translatorReady;
  const factory = translatorFactory();
  if (factory === null) {
    chromeUsable = false;
    return Promise.resolve(null);
  }
  translatorReady = factory
    .create({ sourceLanguage: SOURCE_LANGUAGE, targetLanguage: TARGET_LANGUAGE })
    .catch(() => {
      translatorReady = null;
      return null;
    });
  return translatorReady;
}

async function translateWithChrome(text: string): Promise<Translation | null> {
  if (chromeUsable === false) return null;
  const translator = await chromeTranslator();
  if (translator === null) return null;
  try {
    const out = await translator.translate(text);
    return out.trim().length === 0 ? null : { text: out.trim(), provider: 'chrome' };
  } catch {
    // A single failed translation says nothing about the next one, so the
    // provider stays enabled and this message simply falls through to Gemini.
    return null;
  }
}

// ---------------------------------------------------------------------------
// Gemini
// ---------------------------------------------------------------------------

/** The first text part of the first candidate, or null for any other shape. */
function firstCandidateText(payload: unknown): string | null {
  if (typeof payload !== 'object' || payload === null) return null;
  const candidates = (payload as { candidates?: unknown }).candidates;
  if (!Array.isArray(candidates)) return null;
  const candidate: unknown = candidates[0];
  if (typeof candidate !== 'object' || candidate === null) return null;
  const content = (candidate as { content?: unknown }).content;
  if (typeof content !== 'object' || content === null) return null;
  const parts = (content as { parts?: unknown }).parts;
  if (!Array.isArray(parts)) return null;
  const chunks: string[] = [];
  for (const part of parts) {
    if (typeof part !== 'object' || part === null) continue;
    const value = (part as { text?: unknown }).text;
    if (typeof value === 'string') chunks.push(value);
  }
  const joined = chunks.join('').trim();
  return joined.length === 0 ? null : joined;
}

async function translateWithGemini(text: string): Promise<Translation | null> {
  const key = readGeminiKey();
  if (key === null) return null;
  try {
    const response = await fetch(GEMINI_ENDPOINT, {
      method: 'POST',
      // The key travels in a header rather than in the query string, so it does
      // not land in a proxy log or a referrer along the way.
      headers: { 'content-type': 'application/json', 'x-goog-api-key': key },
      body: JSON.stringify({
        systemInstruction: { parts: [{ text: GEMINI_SYSTEM }] },
        contents: [{ role: 'user', parts: [{ text }] }],
        generationConfig: { temperature: 0, candidateCount: 1 },
      }),
    });
    if (!response.ok) return null;
    const payload: unknown = await response.json();
    const out = firstCandidateText(payload);
    return out === null ? null : { text: out, provider: 'gemini' };
  } catch {
    return null;
  }
}

// ---------------------------------------------------------------------------
// The cache
// ---------------------------------------------------------------------------

interface CacheRecord {
  text: string;
  provider: TranslationProvider;
  /** Epoch milliseconds, so a later eviction pass has something to sort by. */
  at: number;
  /** Which kind of client produced it, when it was not this one. */
  from?: TranslationSource;
}

/**
 * Translations already in this session's memory, keyed by the hash of their
 * source. A repaint is synchronous and must not await anything, so the render
 * path reads this map and only the priming pass touches IndexedDB.
 */
const memo = new Map<string, Translation>();

function asTranslation(value: unknown): Translation | null {
  if (typeof value !== 'object' || value === null) return null;
  const record = value as Partial<CacheRecord>;
  if (typeof record.text !== 'string' || record.text.length === 0) return null;
  if (record.provider !== 'chrome' && record.provider !== 'gemini') return null;
  const from =
    record.from === 'browser' || record.from === 'gemini' || record.from === 'google' ? record.from : undefined;
  return from === undefined ? { text: record.text, provider: record.provider } : { text: record.text, provider: record.provider, from };
}

/**
 * The cache key for a piece of source text: its hash, unmodified. The caller
 * hashes the same string for its own purposes, so the two agree without either
 * side having to know the other's rule.
 */
export function translationKey(text: string): Promise<string> {
  return messageHash(text);
}

/** A translation already in memory, for a synchronous repaint. */
export function rememberedTranslation(hash: string): Translation | null {
  return memo.get(hash) ?? null;
}

/**
 * Take a translation another client produced into this device's own cache.
 *
 * Marked with the client kind it came from, and that mark is what stops it
 * being offered back to the store later: the store already has it, and a
 * round trip that re-asserts somebody else's work under our name would break
 * the rule that a browser translation never displaces a Gemini one.
 */
export async function rememberShared(hash: string, text: string, source: TranslationSource): Promise<void> {
  if (text.trim().length === 0) return;
  // `provider` is this page's own vocabulary and says which of ITS two
  // providers made a translation. A shared one was not made by either, so the
  // field is filled in only because the record shape wants it, and `from` is
  // what everything actually reads: the panel credits by `from`, and nothing
  // shared is ever offered back.
  const translation: Translation = { text, provider: source === 'browser' ? 'chrome' : 'gemini', from: source };
  memo.set(hash, translation);
  const record: CacheRecord = { text: translation.text, provider: translation.provider, at: Date.now(), from: source };
  await idbSet(STORE_TRANSLATIONS, hash, record);
}

/** The cached translation of a text, from memory or IndexedDB. Never networked. */
export async function cachedTranslation(text: string): Promise<Translation | null> {
  if (text.trim().length === 0) return null;
  const hash = await messageHash(text);
  const remembered = memo.get(hash);
  if (remembered !== undefined) return remembered;
  const stored = asTranslation(await idbGet<unknown>(STORE_TRANSLATIONS, hash));
  if (stored !== null) memo.set(hash, stored);
  return stored;
}

// ---------------------------------------------------------------------------
// The surface
// ---------------------------------------------------------------------------

/** Which providers could serve a translation right now, best first. */
export function translationProviders(): TranslationProvider[] {
  const providers: TranslationProvider[] = [];
  if (chromeUsable !== false && translatorFactory() !== null) providers.push('chrome');
  if (readGeminiKey() !== null) providers.push('gemini');
  return providers;
}

/**
 * Ask the browser whether it can actually translate this language pair, then
 * report the providers. Separate from `translationProviders` because that one
 * is called during a paint and this one awaits the browser.
 */
export async function probeTranslationProviders(): Promise<TranslationProvider[]> {
  const factory = translatorFactory();
  if (factory === null) {
    chromeUsable = false;
  } else {
    try {
      const state = await factory.availability({ sourceLanguage: SOURCE_LANGUAGE, targetLanguage: TARGET_LANGUAGE });
      chromeUsable = !UNUSABLE.has(state);
    } catch {
      chromeUsable = false;
    }
  }
  return translationProviders();
}

/**
 * The translation of a text: from the cache when there is one, otherwise from
 * the first provider that answers. `null` means no provider was available or
 * none of them produced anything, which is a state the caller renders rather
 * than an error it handles.
 */
export async function translate(text: string): Promise<Translation | null> {
  if (text.trim().length === 0) return null;
  const hash = await messageHash(text);
  const cached = memo.get(hash) ?? (await cachedTranslation(text));
  if (cached !== null) return cached;

  const fresh = (await translateWithChrome(text)) ?? (await translateWithGemini(text));
  if (fresh === null) return null;
  memo.set(hash, fresh);
  const record: CacheRecord = { ...fresh, at: Date.now() };
  await idbSet(STORE_TRANSLATIONS, hash, record);
  return fresh;
}
