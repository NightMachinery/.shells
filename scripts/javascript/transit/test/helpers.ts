import type { FetchLike } from '../src/http.ts';

/** Read a trimmed synthetic fixture. Every id in these files is made up. */
export async function fixture<T>(name: string): Promise<T> {
  return JSON.parse(await Bun.file(`${import.meta.dir}/fixtures/${name}`).text()) as T;
}

export interface MockFetch {
  fetchImpl: FetchLike;
  /** Every URL the code under test asked for, in order. */
  urls: string[];
}

/**
 * Build an injectable fetch. Nothing global is patched: the backends take their
 * fetch as a constructor argument, so a test never has to put the process back
 * the way it found it.
 */
export function mockFetch(handler: (url: string, call: number) => unknown): MockFetch {
  const urls: string[] = [];
  const fetchImpl: FetchLike = async (url: string) => {
    const index = urls.length;
    urls.push(url);
    const body = handler(url, index);
    if (body instanceof Response) return body;
    return new Response(JSON.stringify(body), {
      status: 200,
      headers: { 'content-type': 'application/json' },
    });
  };
  return { fetchImpl, urls };
}

/** A fetch that always fails, for the fallback tests. */
export function failingFetch(message = 'synthetic network failure'): MockFetch {
  const urls: string[] = [];
  const fetchImpl: FetchLike = async (url: string) => {
    urls.push(url);
    throw new Error(message);
  };
  return { fetchImpl, urls };
}

/** 2026-01-01T08:00:00Z, the instant every fixture is written around. */
export const FIXTURE_NOW = Date.parse('2026-01-01T08:00:00Z');

export const FIXTURE_WINDOW = {
  fromMs: FIXTURE_NOW - 60_000,
  toMs: FIXTURE_NOW + 120 * 60_000,
};
