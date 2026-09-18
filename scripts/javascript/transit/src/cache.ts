import { narrowModes, type Backend, type DepartureOptions, type Window } from './backends/types.ts';
import type { Departure, Message, Mode, StopHit } from './model.ts';

// Opt-in response cache backed by Redis. This module is bun-only and is never
// imported by the browser entry point; the page has its own in-memory state and
// no business holding a Redis connection.

/** Every key this package writes starts here, so a stray key set is easy to spot. */
export const CACHE_PREFIX = 'transit:';

// The two time-to-live values live here and nowhere else.
/** Departure boards go stale within seconds; this is short on purpose. */
export const DEPARTURES_TTL_SECONDS = 30;
/** Stop searches and platform lookups are effectively static. */
export const LOOKUP_TTL_SECONDS = 86_400;

/** FNV-1a over the request parameters, to keep keys short and printable. */
export function paramsHash(input: string): string {
  let hash = 0x811c9dc5;
  for (let i = 0; i < input.length; i += 1) {
    hash ^= input.charCodeAt(i);
    hash = Math.imul(hash, 0x01000193) >>> 0;
  }
  return hash.toString(16).padStart(8, '0');
}

/**
 * A window is bucketed to whole minutes before it becomes part of a key.
 * Hashing the raw millisecond bounds would make every invocation a miss, since
 * no two runs start in the same millisecond.
 */
export function departuresKey(backend: string, stop: string, window: Window, transportTypes: readonly string[]): string {
  const canonical = [
    Math.floor(window.fromMs / 60_000),
    Math.ceil(window.toMs / 60_000),
    [...transportTypes].sort().join(','),
  ].join('|');
  return `${CACHE_PREFIX}dep:${backend}:${stop}:${paramsHash(canonical)}`;
}

export function lookupKey(backend: string, kind: string, q: string): string {
  return `${CACHE_PREFIX}lookup:${backend}:${kind}:${q}`;
}

export interface TransitCache {
  enabled: boolean;
  get<T>(key: string): Promise<T | null>;
  set(key: string, value: unknown, ttlSeconds: number): Promise<void>;
  close(): void;
}

/** A cache that is never consulted, for the default (caching off) path. */
export function nullCache(): TransitCache {
  return {
    enabled: false,
    async get<T>(): Promise<T | null> {
      return null;
    },
    async set(): Promise<void> {
      /* nothing is stored */
    },
    close(): void {
      /* nothing is open */
    },
  };
}

/** Connection string variable the runtime already understands. */
export const REDIS_URL_ENV = 'REDIS_URL';
/** Password variable the standard Redis command-line client uses. */
export const REDIS_AUTH_ENV = 'REDISCLI_AUTH';
const REDIS_LOCAL_DEFAULT = 'redis://default:@localhost:6379';

/**
 * Where to connect. A full connection string wins. Failing that, a server on
 * this machine that wants a password is reached with the same credential the
 * standard client uses, so the password stays in the environment and never
 * needs a home in this package or its config. Returning nothing means the
 * runtime's own default applies.
 */
export function redisUrl(env: Record<string, string | undefined> = process.env): string | undefined {
  const explicit = env[REDIS_URL_ENV];
  if (explicit !== undefined && explicit.length > 0) return explicit;
  const auth = env[REDIS_AUTH_ENV];
  if (auth !== undefined && auth.length > 0) {
    return REDIS_LOCAL_DEFAULT.replace('default:@', `default:${encodeURIComponent(auth)}@`);
  }
  return undefined;
}

/**
 * Redis-backed cache. Any Redis problem degrades to a miss: a departure board
 * that renders slightly slower is strictly better than one that refuses to
 * render because an optional accelerator is down. The failure is reported once
 * per process so a permanently broken Redis is still noticeable.
 */
export function redisCache(warn: (line: string) => void = (line) => process.stderr.write(`${line}\n`)): TransitCache {
  let warned = false;
  const url = redisUrl();
  const client: TransitRedisClient = url === undefined ? Bun.redis : new Bun.RedisClient(url);
  function degrade(error: unknown): void {
    if (warned) return;
    warned = true;
    warn(`transit: cache unavailable, continuing without it (${error instanceof Error ? error.message : String(error)})`);
  }

  return {
    enabled: true,
    async get<T>(key: string): Promise<T | null> {
      try {
        const raw = await client.get(key);
        if (raw === null || raw === undefined) return null;
        return JSON.parse(raw) as T;
      } catch (error) {
        degrade(error);
        return null;
      }
    },
    async set(key: string, value: unknown, ttlSeconds: number): Promise<void> {
      try {
        await client.set(key, JSON.stringify(value));
        await client.expire(key, ttlSeconds);
      } catch (error) {
        degrade(error);
      }
    },
    close(): void {
      try {
        client.close();
      } catch {
        /* closing a connection that never opened is not an error worth reporting */
      }
    },
  };
}

export function createCache(enabled: boolean): TransitCache {
  return enabled ? redisCache() : nullCache();
}

/** Wrap a backend so its reads go through the cache. */
export function withCache(backend: Backend, cache: TransitCache, transportTypes: readonly Mode[]): Backend {
  if (!cache.enabled) return backend;
  return {
    name: backend.name,
    async search(q: string): Promise<StopHit[]> {
      const key = lookupKey(backend.name, 'search', q);
      const hit = await cache.get<StopHit[]>(key);
      if (hit !== null) return hit;
      const value = await backend.search(q);
      await cache.set(key, value, LOOKUP_TTL_SECONDS);
      return value;
    },
    async nearby(lat: number, lon: number): Promise<StopHit[]> {
      const key = lookupKey(backend.name, 'nearby', `${lat},${lon}`);
      const hit = await cache.get<StopHit[]>(key);
      if (hit !== null) return hit;
      const value = await backend.nearby(lat, lon);
      await cache.set(key, value, LOOKUP_TTL_SECONDS);
      return value;
    },
    async departures(stop: string, window: Window, options?: DepartureOptions): Promise<Departure[]> {
      // A per-call narrowing is part of the request and so part of the key. A
      // board asking for one category would otherwise store its short answer
      // under the key the unnarrowed board reads, and empty that board out.
      const types = narrowModes(transportTypes, options?.transportTypes);
      const key = departuresKey(backend.name, stop, window, types);
      const hit = await cache.get<Departure[]>(key);
      if (hit !== null) return hit;
      const value = await backend.departures(stop, window, options);
      await cache.set(key, value, DEPARTURES_TTL_SECONDS);
      return value;
    },
    async messages(): Promise<Message[]> {
      const key = lookupKey(backend.name, 'messages', 'all');
      const hit = await cache.get<Message[]>(key);
      if (hit !== null) return hit;
      const value = await backend.messages();
      await cache.set(key, value, DEPARTURES_TTL_SECONDS);
      return value;
    },
  };
}

/** Adapter matching the platform-id lookup hook the aggregator backend accepts. */
export function lookupCacheAdapter(cache: TransitCache): { get(key: string): Promise<string[] | null>; set(key: string, value: string[]): Promise<void> } {
  return {
    get: (key: string) => cache.get<string[]>(`${CACHE_PREFIX}lookup:${key}`),
    set: (key: string, value: string[]) => cache.set(`${CACHE_PREFIX}lookup:${key}`, value, LOOKUP_TTL_SECONDS),
  };
}
