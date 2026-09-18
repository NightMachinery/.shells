// A minimal IndexedDB key-value store.
//
// Why IndexedDB and not `localStorage`: the page caches a whole profile's
// boards so that opening it underground, or on a dead connection, still shows
// the last known times with their age rather than an empty screen. That is tens
// of kilobytes of structured data per profile, which `localStorage` stores as a
// string it has to re-parse on every read, inside a five-megabyte budget shared
// with everything else the page remembers. IndexedDB stores the object graph
// and is asynchronous, so a large read does not block the first paint.
//
// Every operation resolves rather than rejects when the store is unavailable.
// A private window, blocked site data and a browser that has evicted the origin
// all fail here, and none of them is a reason for the page not to work: the
// cache is an optimisation, and the network is the source of truth.

const DB_NAME = 'transit';
const DB_VERSION = 2;

/** Object stores, named for what they hold rather than for who writes them. */
export const STORE_BOARDS = 'boards';
export const STORE_TRANSLATIONS = 'translations';
/**
 * Which identifier the journey planner accepts for a stop. Worth keeping
 * between visits rather than re-deriving: it is a fact about the aggregator's
 * data, it costs a request to establish, and it does not change from one day
 * to the next.
 */
export const STORE_ORIGINS = 'origins';

const STORES: readonly string[] = [STORE_BOARDS, STORE_TRANSLATIONS, STORE_ORIGINS];

let opening: Promise<IDBDatabase | null> | null = null;

function openDb(): Promise<IDBDatabase | null> {
  if (opening !== null) return opening;
  opening = new Promise<IDBDatabase | null>((resolve) => {
    let request: IDBOpenDBRequest;
    try {
      request = indexedDB.open(DB_NAME, DB_VERSION);
    } catch {
      resolve(null);
      return;
    }
    request.onupgradeneeded = () => {
      const db = request.result;
      for (const store of STORES) if (!db.objectStoreNames.contains(store)) db.createObjectStore(store);
    };
    request.onsuccess = () => resolve(request.result);
    request.onerror = () => resolve(null);
    // A blocked upgrade means another tab holds an older version open. Rather
    // than wait for it, give up and run without a cache for this session.
    request.onblocked = () => resolve(null);
  });
  return opening;
}

export async function idbGet<T>(store: string, key: string): Promise<T | null> {
  const db = await openDb();
  if (db === null) return null;
  return new Promise<T | null>((resolve) => {
    try {
      const request = db.transaction(store, 'readonly').objectStore(store).get(key);
      request.onsuccess = () => resolve((request.result as T | undefined) ?? null);
      request.onerror = () => resolve(null);
    } catch {
      resolve(null);
    }
  });
}

export async function idbSet(store: string, key: string, value: unknown): Promise<void> {
  const db = await openDb();
  if (db === null) return;
  return new Promise<void>((resolve) => {
    try {
      const transaction = db.transaction(store, 'readwrite');
      transaction.objectStore(store).put(value, key);
      transaction.oncomplete = () => resolve();
      transaction.onerror = () => resolve();
      transaction.onabort = () => resolve();
    } catch {
      resolve();
    }
  });
}

/** Hex SHA-256 of a string, for keying a cache by content rather than by index. */
export async function sha256Hex(text: string): Promise<string> {
  const bytes = new TextEncoder().encode(text);
  const digest = await crypto.subtle.digest('SHA-256', bytes);
  return [...new Uint8Array(digest)].map((byte) => byte.toString(16).padStart(2, '0')).join('');
}
