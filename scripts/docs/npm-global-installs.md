# Global npm installs: why pnpm cannot install large packages

`pnpm add -g` and `pnpm install -g` cannot install any npm package whose
*tarball* is larger than roughly 50 MB. The install aborts with

```
FATAL ERROR: invalid array length Allocation failed - JavaScript heap out of memory
```

and exit status 134. Verified on node v24.4.1 and pnpm 10.32.1, on mb2.

The message is misleading twice over: it is not a heap-size problem, and it is
not the package's fault. It is a string-length ceiling being hit inside pnpm's
worker thread while it hashes the tarball. Until pnpm or Node fixes it, install
large packages with npm, which is what [agfi:codex-install] and
[agfi:claude-install] now do.

## The mechanism

Traced end to end:

1. pnpm downloads the tarball on the main thread as a Node `Buffer` and hands
   it to a worker thread via `postMessage` for integrity verification and
   extraction (`addTarballToStore` in
   `/opt/homebrew/lib/node_modules/pnpm/dist/worker.js`).
2. The structured clone that carries a `Buffer` across a `MessagePort` strips
   the `Buffer` prototype. The worker receives a plain `Uint8Array`. Verified
   directly with a `MessageChannel` round-trip: `Buffer.isBuffer()` is `false`
   on the far side.
3. The worker calls `crypto.hash(algo, buffer, "hex")`. Node fast-paths a real
   `Buffer`, but a plain `Uint8Array` falls through to string coercion:
   `Uint8Array.prototype.toString()` is `join()`, so the bytes are rendered as
   decimal numbers joined with commas.
4. Compressed tarball bytes are high-entropy, so most render as three digits
   plus a comma: about 4.5 characters per byte, and V8 strings are two bytes
   per character. Past V8's maximum string length the allocation fails fatally
   inside the worker, which takes the whole process down.

Reproduced standalone, the JavaScript stack is:

```
RangeError: Invalid string length
    at Uint8Array.join (<anonymous>)
    at Uint8Array.toString (<anonymous>)
    at Object.hash (node:internal/crypto/hash:264:10)
```

A `Buffer` of the same size hashes fine; only a plain `Uint8Array` takes the
string path.

### The threshold

Measured by hashing high-entropy (`crypto.randomFillSync`) data as a plain
`Uint8Array`, one process per size:

- 45 MB: ok
- 50 MB: ok
- 55 MB: fatal

So the practical rule is: any tarball over about 50 MB.

### `--max-old-space-size` does not help

It is the obvious thing to reach for, and it does nothing here. The ceiling is
V8's maximum string length, not the heap size, so no amount of
`NODE_OPTIONS=--max-old-space-size=...` moves it. A parked `if false` branch in
the old `codex-install` had tried exactly that.

### Packages currently over the line

darwin-arm64 tarball sizes, measured with an HTTP range request:

- `@openai/codex` 0.153.4: 115,672,312 bytes, about 110 MB (288 MB unpacked)
- `@anthropic-ai/claude-code` 2.1.263: 87,267,290 bytes, about 83 MB (199 MB
  unpacked)

Both were confirmed to crash pnpm. Both install cleanly with npm, which does no
worker round-trip.

## The second bug: npm's orphaned staging directories

`npm install -g` upgrades a package by renaming the old directory out of the
way to `.<name>-<random>` beside it, then renaming it back or deleting it at
the end. A crash between those two steps leaves the staging directory behind,
and npm never collects it. Every later install of that package then fails
before downloading anything:

```
npm error ENOTEMPTY: directory not empty, rename
  '/opt/homebrew/lib/node_modules/@openai/codex'
  -> '/opt/homebrew/lib/node_modules/@openai/.codex-QDftHxCm'
```

because npm derives the same staging name again and refuses to overwrite a
non-empty one.

On this machine a 359 MB `.codex-QDftHxCm` had sat under `@openai` since
January, left by an earlier instance of the pnpm crash above, and it made every
`codex-install` fail. [agfi:h-npm-install-clean-staging] now trashes these
before installing.

## PATH shadowing

npm's global prefix (`/opt/homebrew/bin`) and `$PNPM_HOME`
(`~/.local/share/pnpm`) both put shims on PATH, and on this machine homebrew
comes first. So a package freshly installed through pnpm can sit shadowed
behind an older copy installed through npm, and the install looks like it did
nothing. Both `codex` and `claude` were found in this state, e.g. codex 0.153.4
from npm winning over codex 0.153.3 from pnpm.

This is why every install function ends by calling
[agfi:h-npm-install-report], which lists every copy of the command on PATH, in
PATH order, with its version:

```
h-npm-install-report: /opt/homebrew/bin/codex: codex-cli 0.153.4
h-npm-install-report: /Users/evar/.local/share/pnpm/codex: codex-cli 0.153.3
h-npm-install-report: 2 copies of 'codex' are on PATH; the first wins.
```

If the version you just installed is not on the first line, the install
worked and PATH is the problem.

## The functions

In `./zshlang/auto-load/others/ins.zsh`:

- [agfi:npm-install] takes `npm_install_engine`, an enum: `auto` (the default:
  pnpm when installed, npm otherwise), `pnpm`, or `npm`. It also takes
  `npm_install_pnpm_opts`, an array of extra pnpm-only flags.
- [agfi:npm-install-npm] and [agfi:npm-install-pnpm] are the shorthands.
- [agfi:h-npm-global-dir] resolves where npm would put a global package.
- [agfi:h-npm-install-clean-staging] trashes orphaned npm staging directories
  for a package.
- [agfi:h-npm-install-report] lists every copy of a command on PATH, in PATH
  order, with its version.

In `./zshlang/auto-load/others/codex.zsh`: [agfi:codex-install] delegates to
npm; [agfi:codex-install-npm] and [agfi:codex-install-pnpm] pick the engine
explicitly.

In `./zshlang/auto-load/others/claude.zsh`: [agfi:claude-install] delegates to
npm; [agfi:claude-install-npm] and [agfi:claude-install-pnpm] pick the engine
explicitly.

Both `-pnpm` variants are currently broken by the bug above. They are kept so
the pnpm route is one word away once pnpm or node fixes it.
[agfi:claude-install-pnpm] additionally passes
`--allow-build='@anthropic-ai/claude-code'`, because pnpm 10 refuses to run a
dependency's postinstall script unless the package is named there, and
claude-code's `install.cjs` postinstall is what puts the native binary in
place.

## Upstream

The proper fixes are upstream. pnpm should re-wrap the received bytes as a
`Buffer` (or transfer the `ArrayBuffer`) before hashing, and Node should not
string-coerce a plain typed array in `crypto.hash`. Nothing is filed that we
know of. Until one of them lands, use npm for large packages.
