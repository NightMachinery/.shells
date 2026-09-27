import { afterEach, describe, expect, test } from 'bun:test';
import { fetchJson, USER_AGENT } from '../src/http.ts';

// What an upstream is told this client is. The planning server runs this
// package from a single address and asks a free journey planner many questions
// a minute, and that planner's policy is that such a client names itself; a
// server that sent the default was indistinguishable from any other script.

function capture(): { seen: string[]; fetchImpl: (url: string, init?: RequestInit) => Promise<Response> } {
  const seen: string[] = [];
  return {
    seen,
    fetchImpl: async (_url, init) => {
      seen.push(new Headers(init?.headers).get('user-agent') ?? '');
      return new Response('{}', { status: 200 });
    },
  };
}

describe('the upstream User-Agent', () => {
  afterEach(() => {
    delete process.env.TRANSIT_USER_AGENT;
  });

  test('is the browser-shaped default when nothing names one', async () => {
    const { seen, fetchImpl } = capture();
    await fetchJson('https://example.invalid/x', { fetchImpl });
    expect(seen).toEqual([USER_AGENT]);
  });

  test('is whatever TRANSIT_USER_AGENT says, read per request rather than at import', async () => {
    const { seen, fetchImpl } = capture();
    process.env.TRANSIT_USER_AGENT = 'example-server/1';
    await fetchJson('https://example.invalid/x', { fetchImpl });
    expect(seen).toEqual(['example-server/1']);
  });
});
