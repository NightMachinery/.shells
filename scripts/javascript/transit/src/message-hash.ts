// What a service message IS, as far as anything that caches or shares a
// translation of it is concerned.
//
// Three separate things key off a notice's text: the acknowledgement that hides
// it, the translation cached in this browser, and the translation shared with
// the other browsers reading the same page. They have to agree, and they run in
// different processes on different machines, so the rule cannot be "whatever
// each of them happens to do". It is written down once, here, and imported by
// all of them.
//
// Content addressing rather than the operator's own notice id, because the
// operator edits a notice in place instead of posting a new one. Keyed by id, a
// translation would go quietly stale against text that had changed underneath
// it; keyed by the text, a changed notice is a miss and corrects itself.
//
// Normalised first, because the same notice arrives spelled differently. The
// two upstreams wrap it at different widths, one of them uses a non-breaking
// space where the other uses an ordinary one, and a feed that gained a trailing
// newline overnight would otherwise invalidate every translation anybody had
// paid for. Trimming and collapsing runs of whitespace to a single space makes
// those the same notice, which is what a reader would say they are.
//
// This module is deliberately free of both runtimes. `TextEncoder` and
// `crypto.subtle` are globals in the browser and in bun, so the page and the
// planning server run the same code rather than two implementations that agree
// until one of them is edited.

/** How many hex characters a hash is, which is also what a validator checks. */
export const MESSAGE_HASH_LENGTH = 64;

const HASH_PATTERN = /^[0-9a-f]{64}$/;

/**
 * The text as the hash sees it: trimmed, with every run of whitespace, of any
 * kind, collapsed to one space.
 *
 * Exported because a caller that wants to show why two notices hashed the same
 * should be able to show the normalised form rather than be told to trust this.
 */
export function normaliseMessageText(text: string): string {
  return text.trim().replace(/\s+/g, ' ');
}

/** The identity of a message text: SHA-256 of its normalised form, lowercase hex. */
export async function messageHash(text: string): Promise<string> {
  const bytes = new TextEncoder().encode(normaliseMessageText(text));
  const digest = await crypto.subtle.digest('SHA-256', bytes);
  return [...new Uint8Array(digest)].map((byte) => byte.toString(16).padStart(2, '0')).join('');
}

/**
 * Whether a string is something this module could have produced.
 *
 * Here rather than at the server that checks it, so that the shape of a hash
 * and the code that makes one can never drift apart.
 */
export function isMessageHash(value: unknown): value is string {
  return typeof value === 'string' && HASH_PATTERN.test(value);
}
