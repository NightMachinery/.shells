import { describe, expect, test } from 'bun:test';
import { SCRATCH_E2E_DIR, sweepTargets } from './e2e/phone.ts';

// The harness kills whatever is still holding its own Chrome profile directory
// after a run. Once, it killed everything else instead: the directory arrived
// as a `Subprocess` object because a caller passed one argument to a function
// that takes two, `String`-ed to `[object Object]`, and went to `pgrep -f` as a
// pattern, which matched most of the machine. bun does not type-check, so an
// arity error that would not have compiled ran three times.
//
// These are the tests for that. Every one of them is about an argument that
// should never arrive, which is exactly the kind of test that gets skipped
// because "it cannot happen".

const PS_OUTPUT = [
  `  101 /Applications/Google Chrome.app/Contents/MacOS/Google Chrome --headless=new --user-data-dir=${SCRATCH_E2E_DIR}/chrome-profile-1`,
  `  102 /Applications/Google Chrome.app/Contents/MacOS/Google Chrome Helper (Renderer) --user-data-dir=${SCRATCH_E2E_DIR}/chrome-profile-1`,
  '  103 /usr/bin/ssh-agent -l',
  '  104 /sbin/launchd',
  '  105 tmux: server',
  `  106 /Applications/Google Chrome.app/Contents/MacOS/Google Chrome --user-data-dir=${SCRATCH_E2E_DIR}/chrome-profile-2`,
  '    1 /sbin/launchd',
].join('\n');

const MINE = `${SCRATCH_E2E_DIR}/chrome-profile-1`;

describe('the harness process sweep', () => {
  test('a directory that is not a string throws before anything is listed', () => {
    // The actual bug: a Subprocess where a path belonged.
    expect(() => sweepTargets({ pid: 1 } as never, PS_OUTPUT, 999, 998)).toThrow(TypeError);
    expect(() => sweepTargets(undefined as never, PS_OUTPUT, 999, 998)).toThrow(TypeError);
    expect(() => sweepTargets('' as never, PS_OUTPUT, 999, 998)).toThrow(TypeError);
  });

  test('a relative path throws', () => {
    expect(() => sweepTargets('chrome-profile-1', PS_OUTPUT, 999, 998)).toThrow(/absolute/);
  });

  test('an absolute path outside the harness scratch root throws', () => {
    expect(() => sweepTargets('/', PS_OUTPUT, 999, 998)).toThrow(/refusing to sweep/);
    expect(() => sweepTargets('/Users', PS_OUTPUT, 999, 998)).toThrow(/refusing to sweep/);
    expect(() => sweepTargets('/tmp/somebody-elses-profile', PS_OUTPUT, 999, 998)).toThrow(/refusing to sweep/);
  });

  test('a path that only looks like a prefix of the scratch root throws', () => {
    expect(() => sweepTargets(`${SCRATCH_E2E_DIR}-elsewhere`, PS_OUTPUT, 999, 998)).toThrow(/refusing to sweep/);
  });

  test('only the processes holding this run own profile are returned', () => {
    expect(sweepTargets(MINE, PS_OUTPUT, 999, 998)).toEqual([101, 102]);
  });

  test('the sweep never returns itself, its parent, or init', () => {
    expect(sweepTargets(MINE, PS_OUTPUT, 101, 998)).toEqual([102]);
    expect(sweepTargets(MINE, PS_OUTPUT, 999, 102)).toEqual([101]);
    const initLine = `    1 chrome --user-data-dir=${MINE}`;
    expect(sweepTargets(MINE, initLine, 999, 998)).toEqual([]);
  });

  test('the path is matched literally, not as a pattern', () => {
    // Every character a regex would treat as syntax, in a path that exists
    // nowhere in the listing. A pattern match would find something here; a
    // literal one finds nothing.
    const wild = `${SCRATCH_E2E_DIR}/.*`;
    expect(sweepTargets(wild, PS_OUTPUT, 999, 998)).toEqual([]);
  });
});
