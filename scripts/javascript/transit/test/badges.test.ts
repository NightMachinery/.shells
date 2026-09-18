import { describe, expect, test } from 'bun:test';
import { destinationBadges, significantWords } from '../src/page/badges.ts';

describe('significantWords', () => {
  test('drops the words that do not say which place this is', () => {
    expect(significantWords('CityRing via Giselastraße – Hauptbahnhof')).toEqual(['CityRing', 'Giselastraße', 'Hbf']);
  });

  test('drops a standalone station word but keeps the compounded one', () => {
    expect(significantWords('Pasing Bahnhof')).toEqual(['Pasing']);
    expect(significantWords('Ostbahnhof')).toEqual(['Ostbahnhof']);
  });
});

describe('destinationBadges', () => {
  test('takes the initials of up to three significant words', () => {
    const badges = destinationBadges(['CityRing via Giselastraße – Hauptbahnhof']);
    expect(badges.get('CityRing via Giselastraße – Hauptbahnhof')?.text).toBe('CGH');
  });

  test('resolves a collision by lengthening the first word', () => {
    const badges = destinationBadges(['Pasing', 'Petershausen']);
    expect(badges.get('Pasing')?.text).toBe('Pa');
    expect(badges.get('Petershausen')?.text).toBe('Pe');
  });

  test('lengthens as far as it needs to', () => {
    const badges = destinationBadges(['Neuperlach', 'Neuaubing', 'Neufahrn']);
    const texts = [...badges.values()].map((badge) => badge.text);
    expect(new Set(texts).size).toBe(3);
    expect(badges.get('Neuperlach')?.text).toBe('Neup');
    expect(badges.get('Neuaubing')?.text).toBe('Neua');
    expect(badges.get('Neufahrn')?.text).toBe('Neuf');
  });

  test('falls back to a digit when the names agree to the end', () => {
    const badges = destinationBadges(['Ost', 'Ost Nord', 'Ost Süd']);
    const texts = [...badges.values()].map((badge) => badge.text);
    expect(new Set(texts).size).toBe(3);
  });

  test('spaces the hues evenly and keeps them alphabetical', () => {
    const badges = destinationBadges(['Cee', 'Aay', 'Bee']);
    expect(badges.get('Aay')?.hue).toBe(0);
    expect(badges.get('Bee')?.hue).toBe(120);
    expect(badges.get('Cee')?.hue).toBe(240);
  });

  test('is stable no matter what order the runs arrive in', () => {
    const one = destinationBadges(['Bee', 'Aay', 'Bee']);
    const other = destinationBadges(['Aay', 'Bee']);
    expect(one.get('Aay')).toEqual(other.get('Aay') as never);
    expect(one.get('Bee')).toEqual(other.get('Bee') as never);
  });
});
