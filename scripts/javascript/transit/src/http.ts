// The one place an HTTP request is made. Both backends and the browser bundle
// import this, so it must stay free of anything that only exists under bun.

/** The shape of `fetch` the package depends on. Tests inject their own. */
export type FetchLike = (url: string, init?: RequestInit) => Promise<Response>;

/**
 * Request timeout. Every outbound call gets one: the upstream APIs are free
 * services with no availability promise, and a hung socket must not wedge a
 * `--watch` loop or the page's refresh timer.
 */
export const HTTP_TIMEOUT_MS = 10_000;

/**
 * Some edges in front of these APIs answer differently, or not at all, without
 * a browser-shaped User-Agent. Sending one is insurance, not deception: the
 * request rate stays well inside what a person clicking around would produce.
 *
 * The default only. A long-running process that makes many requests should
 * say what it is: Transitous asks every server-side client for a User-Agent
 * naming the application and its version, and a planning server that sent this
 * one was indistinguishable from any other script. `TRANSIT_USER_AGENT`
 * replaces it, read per request so a process can set it after import. A
 * browser does not let a page set this header at all and sends a Referer
 * instead, which Transitous accepts in its place.
 */
export const USER_AGENT = 'Mozilla/5.0';

/** The User-Agent a request goes out with right now. */
export function userAgent(): string {
  return envOverride('TRANSIT_USER_AGENT') ?? USER_AGENT;
}

export interface FetchJsonOptions {
  timeoutMs?: number;
  fetchImpl?: FetchLike;
  headers?: Record<string, string>;
}

/** Thrown for a non-2xx response, so callers can distinguish it from a timeout. */
export class HttpError extends Error {
  readonly status: number;
  readonly url: string;
  constructor(url: string, status: number, statusText: string) {
    super(`HTTP ${status} ${statusText} for ${url}`);
    this.name = 'HttpError';
    this.status = status;
    this.url = url;
  }
}

/** GET a URL and parse the body as JSON, with a timeout and the shared headers. */
export async function fetchJson<T>(url: string, options: FetchJsonOptions = {}): Promise<T> {
  const impl: FetchLike = options.fetchImpl ?? ((u, i) => fetch(u, i));
  const response = await impl(url, {
    method: 'GET',
    headers: {
      Accept: 'application/json',
      'User-Agent': userAgent(),
      ...(options.headers ?? {}),
    },
    signal: AbortSignal.timeout(options.timeoutMs ?? HTTP_TIMEOUT_MS),
  });
  if (!response.ok) throw new HttpError(url, response.status, response.statusText);
  return (await response.json()) as T;
}

/**
 * Read a configuration knob that may be supplied either as a process
 * environment variable (CLI) or as a global set by the page shell (browser).
 * The global wins so a deployed page can point itself at a mirror without a
 * rebuild.
 */
export function envOverride(name: string): string | undefined {
  const bag = (globalThis as { TRANSIT_ENV?: Record<string, string | undefined> }).TRANSIT_ENV;
  const fromGlobal = bag?.[name];
  if (typeof fromGlobal === 'string' && fromGlobal.length > 0) return fromGlobal;
  const proc = (globalThis as { process?: { env?: Record<string, string | undefined> } }).process;
  const fromEnv = proc?.env?.[name];
  if (typeof fromEnv === 'string' && fromEnv.length > 0) return fromEnv;
  return undefined;
}
