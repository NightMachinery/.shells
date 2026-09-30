import { describe, expect, test } from 'bun:test';
import { noticesFor, translatableTexts } from '../src/page/messages.ts';
import type { Message } from '../src/model.ts';

// Which notices the page looks up and translates. The operator's feed carries
// every notice in the network, a few hundred at a time, and a profile's lines
// are named in a handful of them. Working through all of them once sent a
// lookup of two hundred hashes, which the server refused as too large, and then
// a translation offer per notice until it refused those too. So the rule under
// test is the panel's own: notices about lines the reader can see, and nothing
// with no text to translate.

function notice(title: string, lines: string[], text = `${title}: Wegen Bauarbeiten entfällt der Halt.`): Message {
  return { title, text, lines, backend: 'mvg' };
}

describe('noticesFor', () => {
  test('keeps only the notices naming a line the profile shows, whatever the spelling', () => {
    const feed = [notice('a', ['U6']), notice('b', ['S 8']), notice('c', ['X99']), notice('d', ['u 6', 'X98'])];
    const shown = noticesFor(feed, new Set(['U6', 's8']));
    expect(shown.map((message) => message.title)).toEqual(['a', 'b', 'd']);
  });

  test('a few hundred notices elsewhere in the network cost nothing', () => {
    const elsewhere = Array.from({ length: 260 }, (_, index) => notice(`far ${index}`, [`X${index}`]));
    const shown = noticesFor([...elsewhere, notice('near', ['U6'])], new Set(['U6']));
    expect(shown.map((message) => message.title)).toEqual(['near']);
  });

  test('drops a notice with no text, which has nothing to translate', () => {
    const feed = [notice('empty', ['U6'], ''), notice('blank', ['U6'], '  \n '), notice('full', ['U6'])];
    expect(noticesFor(feed, new Set(['U6'])).map((message) => message.title)).toEqual(['full']);
  });

  test('a profile that names no lines sees every notice, as the panel does', () => {
    const feed = [notice('a', ['U6']), notice('b', ['X99'])];
    expect(noticesFor(feed, new Set())).toHaveLength(2);
  });
});

describe('translatableTexts', () => {
  test('the body and then the title, each translated on its own', () => {
    const message = notice('Umleitung wegen Marathon', ['16'], 'Wegen des Marathons entfällt der Halt.');
    expect(translatableTexts(message)).toEqual(['Wegen des Marathons entfällt der Halt.', 'Umleitung wegen Marathon']);
  });

  test('a title that is the body, or empty, is not a second text', () => {
    expect(translatableTexts(notice('Same', ['16'], 'Same'))).toEqual(['Same']);
    expect(translatableTexts(notice('  ', ['16'], 'Body.'))).toEqual(['Body.']);
    expect(translatableTexts(notice('Title', ['16'], ''))).toEqual(['Title']);
  });
});
