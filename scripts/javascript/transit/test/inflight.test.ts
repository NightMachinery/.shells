import { describe, expect, test, beforeEach } from 'bun:test';
import { resetInflight, share, withLimit } from '../src/inflight.ts';

beforeEach(() => {
  resetInflight();
});

describe('share', () => {
  test('two callers asking at the same moment make one request', async () => {
    let calls = 0;
    const run = async (): Promise<string> => {
      calls += 1;
      await new Promise((resolve) => setTimeout(resolve, 10));
      return 'rows';
    };
    const [a, b] = await Promise.all([share('k', run), share('k', run)]);
    expect(calls).toBe(1);
    expect(a).toBe('rows');
    expect(b).toBe('rows');
  });

  test('the second caller is told it joined rather than started one', async () => {
    let joined = 0;
    const run = async (): Promise<number> => {
      await new Promise((resolve) => setTimeout(resolve, 5));
      return 1;
    };
    await Promise.all([
      share('k', run, () => {
        joined += 1;
      }),
      share('k', run, () => {
        joined += 1;
      }),
    ]);
    expect(joined).toBe(1);
  });

  test('a request asked again after the first one finished is a new request', async () => {
    let calls = 0;
    const run = async (): Promise<number> => {
      calls += 1;
      return calls;
    };
    await share('k', run);
    await share('k', run);
    expect(calls).toBe(2);
  });

  test('a failure is not remembered, so the next caller retries', async () => {
    let calls = 0;
    const run = async (): Promise<number> => {
      calls += 1;
      throw new Error('upstream said no');
    };
    await expect(share('k', run)).rejects.toThrow('upstream said no');
    await expect(share('k', run)).rejects.toThrow('upstream said no');
    expect(calls).toBe(2);
  });

  test('different keys are different questions', async () => {
    let calls = 0;
    const run = async (): Promise<number> => {
      calls += 1;
      await new Promise((resolve) => setTimeout(resolve, 5));
      return calls;
    };
    await Promise.all([share('a', run), share('b', run)]);
    expect(calls).toBe(2);
  });
});

describe('withLimit', () => {
  test('never lets more than the limit run at once', async () => {
    let running = 0;
    let peak = 0;
    const task = async (): Promise<void> => {
      running += 1;
      peak = Math.max(peak, running);
      await new Promise((resolve) => setTimeout(resolve, 5));
      running -= 1;
    };
    await Promise.all(Array.from({ length: 10 }, () => withLimit('gate', 3, task)));
    expect(peak).toBe(3);
    expect(running).toBe(0);
  });

  test('a call marked front runs before everything already waiting', async () => {
    const order: string[] = [];
    let release: () => void = () => {};
    const blocker = withLimit('gate', 1, () => new Promise<void>((resolve) => (release = resolve)));
    const waiting = ['a', 'b', 'c'].map((name) =>
      withLimit('gate', 1, async () => {
        order.push(name);
      }),
    );
    const urgent = withLimit('gate', 1, async () => {
      order.push('urgent');
    }, { front: true });
    release();
    await Promise.all([blocker, ...waiting, urgent]);
    expect(order).toEqual(['urgent', 'a', 'b', 'c']);
  });

  test('a task that throws still gives up its slot', async () => {
    const fail = async (): Promise<void> => {
      throw new Error('no');
    };
    await expect(withLimit('gate', 1, fail)).rejects.toThrow('no');
    // If the slot had leaked, this would never resolve.
    await expect(withLimit('gate', 1, async () => 'through')).resolves.toBe('through');
  });
});
