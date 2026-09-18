// Minimal ambient declarations for the slice of the bun and node runtimes this
// package actually touches. Declaring them here rather than depending on
// `@types/bun` keeps `bun install` optional: the only devDependency is the
// TypeScript compiler itself, and `bun run typecheck` works from a bare
// checkout.
//
// Nothing under `src/page.ts` may reach any of these. The browser bundle is
// built from DOM globals alone.

interface TransitBunFile {
  text(): Promise<string>;
  exists(): Promise<boolean>;
}

interface TransitRedisClient {
  get(key: string): Promise<string | null>;
  set(key: string, value: string): Promise<unknown>;
  expire(key: string, seconds: number): Promise<unknown>;
  close(): void;
}

declare namespace Bun {
  function file(path: string): TransitBunFile;
  const TOML: { parse(source: string): unknown };
  const redis: TransitRedisClient;
  const RedisClient: new (url?: string) => TransitRedisClient;
}

declare const process: {
  argv: string[];
  env: Record<string, string | undefined>;
  exit(code?: number): never;
  exitCode?: number;
  on(event: string, listener: (...args: unknown[]) => void): void;
  stdout: { isTTY?: boolean; write(chunk: string): unknown };
  stderr: { isTTY?: boolean; write(chunk: string): unknown };
};

interface ImportMeta {
  /** Absolute URL of this module. */
  url: string;
  /** Absolute path of the directory holding this module (a bun extension). */
  dir: string;
}

declare module 'bun:test' {
  export function describe(label: string, body: () => void): void;
  export function test(label: string, body: () => unknown): void;
  export const it: typeof test;
  // The matcher surface is large and stable; a permissive shape keeps this
  // declaration short without weakening the types of the code under test.
  export function expect(actual: unknown): any;
  export function beforeEach(body: () => unknown): void;
  export function afterEach(body: () => unknown): void;
}
