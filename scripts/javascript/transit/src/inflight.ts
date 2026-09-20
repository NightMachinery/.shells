// Two answers to the same question, asked at the same moment.
//
// A cache stops the second request only if the first one has already finished.
// Everything on this page starts at once, so the common case is not "asked
// again later", it is "asked twice in the same millisecond": two boards at the
// same stop, two boards planning to the same place, a prefetch overlapping the
// profile the reader is looking at. A plain cache misses every one of those,
// and the miss is invisible because the answers agree.
//
// So requests are shared by promise, not by result. The first caller starts the
// work and every caller that arrives before it finishes waits on the same
// promise. The entry is removed when it settles, so a failure is retried rather
// than remembered, and nothing here is a cache: it holds a request only while
// that request is in the air.

/** How many of these may run at once, and what is waiting for a turn. */
interface Gate {
  limit: number;
  running: number;
  queue: Array<() => void>;
}

const gates = new Map<string, Gate>();

/** Where a call waits for its turn when the gate is full. */
export interface LimitOptions {
  /**
   * Ahead of everything already waiting, rather than behind it.
   *
   * The gates are first come, first served, which is right for the work a page
   * starts on its own: no board's third page matters more than another's. It is
   * wrong for a lookup a reader just asked for by opening a sheet. That request
   * used to join the back of a queue of dozens of background trip checks and
   * the sheet said "reading where this goes" for ten seconds or more on a fast
   * connection, which reads as the list having been taken away. The cap still
   * holds; only the order changes.
   */
  front?: boolean | undefined;
}

/**
 * Cap how many calls of one kind are in flight at once.
 *
 * Without this, a long horizon across three profiles fires dozens of requests
 * in the same instant. Two things go wrong then and only one of them is
 * visible: a public aggregator starts refusing, which shows up as a board that
 * failed for no reason a reader can see, and a phone on a slow connection
 * spends its bandwidth on the last board's third page while the first board is
 * still waiting for its first.
 */
export async function withLimit<T>(name: string, limit: number, run: () => Promise<T>, options: LimitOptions = {}): Promise<T> {
  const gate = gates.get(name) ?? { limit, running: 0, queue: [] };
  gate.limit = limit;
  gates.set(name, gate);

  if (gate.running >= gate.limit) {
    await new Promise<void>((resolve) => {
      if (options.front === true) gate.queue.unshift(resolve);
      else gate.queue.push(resolve);
    });
  }
  gate.running += 1;
  try {
    return await run();
  } finally {
    gate.running -= 1;
    const next = gate.queue.shift();
    if (next !== undefined) next();
  }
}

const inFlight = new Map<string, Promise<unknown>>();

/**
 * Share one in-flight request between everybody who asks for it.
 *
 * The key must name the answer, not the caller: two boards asking about the
 * same stop over the same window want the same rows, and a key that included
 * the board would defeat the whole point.
 */
export function share<T>(key: string, run: () => Promise<T>, onJoined?: () => void): Promise<T> {
  const running = inFlight.get(key);
  if (running !== undefined) {
    // Told at the moment of joining rather than inferred from how long the wait
    // was. A joined request waits exactly as long as the one it joined, so
    // "came back suspiciously fast" does not identify it; the first attempt at
    // counting these measured nothing for that reason.
    onJoined?.();
    return running as Promise<T>;
  }
  const started = run().finally(() => {
    inFlight.delete(key);
  });
  inFlight.set(key, started);
  return started;
}

/** Forget every gate and every shared request. Tests use it between cases. */
export function resetInflight(): void {
  gates.clear();
  inFlight.clear();
}

/** How many requests are being shared right now, for the timing line. */
export function inFlightCount(): number {
  return inFlight.size;
}
