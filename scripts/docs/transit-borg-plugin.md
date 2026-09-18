# A chat plugin for the departure boards (deferred spec)

This is a design, not an implementation. It records what a betterborg plugin over
[agfi:transit-board] would have to do, so that whoever writes it does not have to
re-derive the interface.

## Why it is worth having

The boards answer a question you ask while already moving: standing up, putting shoes on,
halfway down the stairs. A phone page is the right shape for that, but it needs the page
to be open. A chat command is the shape for the other half of the same question, the one
you ask from inside a conversation you are already in, and it costs one message.

## Where it runs

On the always-on host, alongside the other betterborg plugins, not on the laptop. The
laptop sleeps; the point of a chat command is that it answers when you are not at the
laptop.

Three things have to exist on that host: bun, a checkout of this repository for the
`javascript/transit` package, and the private configuration repository the package reads
through `ADDRESS_CONFIG`. None of them needs anything else installed.

betterborg runs one tmux session per plugin directory, selected by `borg_plugin_path=`,
so this plugin either joins `stdplugins/` or gets a directory of its own with the usual
`tmuxnew` line recorded in the deployment's README. A directory of its own is the better
default: the departures plugin shells out to bun and has a dependency the other plugins do
not, so a crash in it should not take them down.

## Commands

Short, because they are typed one-handed:

- `.h` or `.home`, `.w` or `.work`: the corresponding profile. An optional trailing
  duration (`.h 90m`, `.w 2h`) overrides the horizon for that one call.
- `.h <profile>`: another profile by its key, for the case where more than one profile is
  a home. Bare `.h` follows the configuration's own default-home key, so the plugin never
  has to know which one that is; naming a profile after it selects that one instead.
- `.dep <query>`: an ad-hoc stop. The query goes through the CLI's stop search, and when
  it is ambiguous the reply lists the candidates rather than guessing.

They register as admin-only handlers with the same `@borg.on(admin_cmd(pattern=...))`
form the other plugins use, with `async` bodies and `event.reply`. Pattern sketch:
`^\.(h|home|w|work)(?:\s+([a-z0-9]+))?(?:\s+(\d+)([hm]))?$` and `^\.dep\s+(.+)$`,
with the optional profile word resolved by the tool, not by the plugin: an unknown profile
is the tool's validation error and the plugin should relay it rather than guess.

The profile keys themselves are not in this repository and must not be. A reply that lists
the available profiles should get them from the tool's configuration export, so this public
file and this public plugin name only the word `home`.

## How it talks to the tool

By subprocess, not by reimplementing anything:

    bun <package>/src/cli.ts board <profile> --json --cache --horizon <minutes>

`--cache` is on here and off almost everywhere else, deliberately. A chat is the one place
the same question gets asked several times in a minute by several people, and the
departures cache exists exactly for that. Its lifetime is a constant in the package's
cache module; the plugin does not get its own copy of that number.

The reply is rendered from the JSON document, never scraped from the terminal renderer:
one message per command, with the board title in bold, up to a handful of rows per board
in the form `HH:MM (+d)  <line> -> <destination>`, cancelled rows struck through, the far
window as a compact strip, and a footer saying when it was generated and which backend
answered. Naming the backend matters because a fallback answer is a slightly different
answer, and a reader who does not know which one they got cannot tell why the platform
number went missing.

## The contract between them

The plugin depends on the CLI's `--json` document and on nothing else. That document
carries `schema_version`, and the plugin must refuse loudly on a version it does not know
rather than rendering a half-understood message. Its shape, per board, is the board title,
the stops it covers, which backend answered, and a list of departures each carrying the
line, the mode, the destination, the planned and realtime timestamps, the delay, whether
it is cancelled, whether it is a replacement service, the platform, the direction letter,
the stop it belongs to and whether it is still catchable given the configured walk time.

`catchable` is computed by the tool, not by the plugin, because the walk time lives in the
configuration and the configuration is what the tool reads.

## What this spec does not settle

Whether the plugin should ever push unprompted. A standing "your usual train is cancelled"
alert is a different product with a different failure mode: it needs a subscription model,
it needs to remember what it already said, and it turns a polite on-demand tool into
something that polls upstream all day. Decide that separately, and if it is ever built,
build it against the same JSON contract.
