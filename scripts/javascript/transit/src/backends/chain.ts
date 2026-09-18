import type { Departure, Message, StopHit } from '../model.ts';
import type { Backend, Window } from './types.ts';

/** What happened for one stop: which backend answered, and why the first did not. */
export interface ChainOutcome {
  stop: string;
  backend: string;
  error?: string;
}

export interface ChainedBackend extends Backend {
  /** Keyed by the argument the call was made with (a stop id, or a lookup key). */
  readonly outcomes: ReadonlyMap<string, ChainOutcome>;
}

function describe(error: unknown): string {
  return error instanceof Error ? error.message : String(error);
}

/**
 * Compose two backends into one that tries the primary and, on any failure,
 * asks the fallback exactly once. There is no retry: these are free services,
 * and a retry loop against one is both rude and useless, since the failure
 * that matters (the service being down) does not clear within a second.
 *
 * A transport error and a non-2xx response are the same event here; `fetchJson`
 * already turns the latter into a throw.
 */
export function chain(primary: Backend, fallback: Backend, onOutcome?: (outcome: ChainOutcome) => void): ChainedBackend {
  const outcomes = new Map<string, ChainOutcome>();

  function record(key: string, backend: string, error?: string): void {
    const outcome: ChainOutcome = error === undefined ? { stop: key, backend } : { stop: key, backend, error };
    outcomes.set(key, outcome);
    onOutcome?.(outcome);
  }

  async function attempt<T>(key: string, run: (backend: Backend) => Promise<T>): Promise<T> {
    try {
      const value = await run(primary);
      record(key, primary.name);
      return value;
    } catch (error) {
      const value = await run(fallback);
      record(key, fallback.name, describe(error));
      return value;
    }
  }

  return {
    name: `${primary.name}+${fallback.name}`,
    outcomes,
    search(q: string): Promise<StopHit[]> {
      return attempt(`search:${q}`, (backend) => backend.search(q));
    },
    nearby(lat: number, lon: number): Promise<StopHit[]> {
      return attempt(`nearby:${lat},${lon}`, (backend) => backend.nearby(lat, lon));
    },
    departures(stop: string, window: Window): Promise<Departure[]> {
      return attempt(stop, (backend) => backend.departures(stop, window));
    },
    messages(): Promise<Message[]> {
      return attempt('messages', (backend) => backend.messages());
    },
  };
}
