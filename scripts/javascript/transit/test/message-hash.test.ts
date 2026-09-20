import { describe, expect, test } from 'bun:test';
import { isMessageHash, MESSAGE_HASH_LENGTH, messageHash, normaliseMessageText } from '../src/message-hash.ts';

// This hash is a contract between three things that never run together: the
// panel in the browser, the translation cache in IndexedDB, and the planning
// server's shared store. So these tests are about the properties the contract
// promises rather than about any particular digest: the same notice hashes the
// same however it was wrapped, a different notice does not, and the answer is
// always something the server's validator will accept.

const NOTICE = 'Between the two ends of the line there is no service this evening.';

describe('normalising a message text', () => {
  test('trims the ends and collapses every run of whitespace to one space', () => {
    expect(normaliseMessageText('  a \n\n  b\t\tc  ')).toBe('a b c');
  });

  test('a non-breaking space is whitespace, because the feeds disagree about which one they send', () => {
    expect(normaliseMessageText('a b')).toBe('a b');
  });

  test('text that is already normal is left exactly as it is', () => {
    expect(normaliseMessageText(NOTICE)).toBe(NOTICE);
  });
});

describe('the message hash', () => {
  test('is stable: the same text hashes the same every time', async () => {
    expect(await messageHash(NOTICE)).toBe(await messageHash(NOTICE));
  });

  test('is 64 lowercase hex characters, which is what the server will accept', async () => {
    const hash = await messageHash(NOTICE);
    expect(hash).toMatch(/^[0-9a-f]{64}$/);
    expect(hash.length).toBe(MESSAGE_HASH_LENGTH);
    expect(isMessageHash(hash)).toBe(true);
  });

  test('a notice rewrapped, re-indented or with a trailing newline is the same notice', async () => {
    const one = await messageHash(NOTICE);
    expect(await messageHash(`  ${NOTICE}\n`)).toBe(one);
    expect(await messageHash(NOTICE.replace(/ /g, '\n   '))).toBe(one);
    expect(await messageHash(NOTICE.replace(' ', ' '))).toBe(one);
  });

  test('a notice whose words changed is a different notice', async () => {
    expect(await messageHash(NOTICE)).not.toBe(await messageHash(`${NOTICE} Replacement buses run.`));
    // Collapsing whitespace is not deleting it: two words do not become one.
    expect(await messageHash('a b')).not.toBe(await messageHash('ab'));
  });

  test('the empty text and whitespace-only text are the same, and are still a hash', async () => {
    expect(await messageHash('   \n ')).toBe(await messageHash(''));
    expect(isMessageHash(await messageHash(''))).toBe(true);
  });
});

describe('recognising a hash', () => {
  test('rejects the wrong length, uppercase hex, and anything that is not a string', () => {
    expect(isMessageHash('a'.repeat(63))).toBe(false);
    expect(isMessageHash('a'.repeat(65))).toBe(false);
    expect(isMessageHash('A'.repeat(64))).toBe(false);
    expect(isMessageHash(`${'a'.repeat(63)}g`)).toBe(false);
    expect(isMessageHash(null)).toBe(false);
    expect(isMessageHash(64)).toBe(false);
  });
});
