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
