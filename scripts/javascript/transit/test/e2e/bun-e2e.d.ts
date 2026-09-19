// Minimal ambient declarations for the extra bun runtime surface the touch
// harness needs, beyond what `src/bun.d.ts` already declares. Kept separate
// (and inside `test/e2e/`) rather than extending `src/bun.d.ts`, which is the
// package's own minimal surface and not this harness's to grow.
//
// `declare namespace Bun` here merges with the one in `src/bun.d.ts`: this
// file adds members, it does not repeat or replace the existing ones.

declare namespace Bun {
  interface ServeHandlerOptions {
    hostname?: string;
    port?: number;
    /** Seconds a connection may sit without data before bun closes it. */
    idleTimeout?: number;
    fetch(request: Request): Response | Promise<Response>;
  }

  interface Server {
    readonly port: number;
    readonly hostname: string;
    stop(closeActiveConnections?: boolean): void;
  }

  function serve(options: ServeHandlerOptions): Server;

  interface SpawnOptions {
    cmd: string[];
    /** The directory the child starts in, which is where it looks for its own files. */
    cwd?: string;
    env?: Record<string, string | undefined>;
    stdout?: 'pipe' | 'inherit' | 'ignore';
    stderr?: 'pipe' | 'inherit' | 'ignore';
    stdin?: 'pipe' | 'inherit' | 'ignore';
  }

  interface Subprocess {
    readonly pid: number;
    readonly stdout: ReadableStream<Uint8Array>;
    readonly stderr: ReadableStream<Uint8Array>;
    readonly exited: Promise<number>;
    kill(signal?: number | string): void;
  }

  function spawn(options: SpawnOptions): Subprocess;

  interface SpawnSyncResult {
    readonly stdout: Uint8Array;
    readonly stderr: Uint8Array;
    readonly exitCode: number;
    readonly success: boolean;
  }

  function spawnSync(options: SpawnOptions): SpawnSyncResult;

  function write(path: string, data: string | Uint8Array): Promise<number>;

  function sleep(ms: number): Promise<void>;
}

/**
 * Whether this module is the file bun was invoked with.
 *
 * The harness needs it because it has two entry points over one body of setup:
 * the assertion suite and the screenshot run beside it. Without the guard, the
 * screenshot run's `import` of the suite would execute the suite.
 *
 * Merged into the `ImportMeta` in `src/bun.d.ts` rather than added there, for
 * the reason at the top of this file: that one is the package's surface and
 * nothing under `src/` asks this question.
 */
interface ImportMeta {
  readonly main: boolean;
}
