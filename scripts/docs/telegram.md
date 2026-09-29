# Telegram helpers


## Destination safety

Zsh Telegram helpers that accept a `dest` argument, and the Python `tsend.py` receiver argument, abort before sending when the destination is empty or whitespace-only.

## Session selection

`tsend.py`'s Telethon backend (`TSEND_BACKEND=1`) reads its session file from
`TELEGRAM_SESSION`, falling back to `~/alice_is_happy` when that is unset. The
value is passed through `expanduser`, so a `~/...` path works even when the
shell never expanded it, and Telethon appends `.session` when the path does not
already end in it — both forms are accepted.

The Bot API backend (`TSEND_BACKEND=2`) has no session file at all; it is
stateless and authenticates per call with `TSEND_TOKEN`.

An already-authorized session is never re-logged-in, so `TSEND_TOKEN` being set
globally does not force a bot login onto a user session.

`tecast.py` honors the same variable and the same default.

## Sending as the main account

The default session is a bot. To send as the main user account instead, use the
`-main` variants, which inject `$telegram_session_main` into `TELEGRAM_SESSION`:

    tsend-main -- someUser 'hello'
    tsendf-main someUser ~/pics/cat.png
    air-main someUser

One variant exists per entry of `tsend_main_variants`: `tsend`, `tsend-retry`,
`tsendf`, `tsendf-discrete`, `tsendf-book`, `tsend-url`, `tsend-urls` and `air`.
They all go through `tlg-main-run`, so nested helpers inherit the session too.

`telegram_session_main` is host-specific and set outside this repository. When
it is unset the variants abort with an error naming the caller, rather than
silently falling back to the bot session.

## Rich messages

A **rich message** is the message kind Telegram added in Bot API 10.1 (MTProto
layer 227), whose Markdown the server parses itself: headings, tables with
alignment, task lists, LaTeX, footnotes and `<details>`. Below, **classic**
means the formatting tsend always had, where Telethon or python-telegram-bot
parse the text into entities before sending it.

`--parse-mode=rich` selects it, for sends and for the `edit` command:

    tsend --parse-mode=rich -- someUser "$(< notes.md)"
    tsend edit --parse-mode=rich -- someUser 42 "$(< notes.md)"

[agfi:md2tlg2] and [agfi:org2tlg2] are the rich versions of [agfi:md2tlg] and
[agfi:org2tlg], which stay classic.

### The two backends

- **Telethon** (`TSEND_BACKEND=1`) sends a raw `messages.sendMessage` with an
  empty text plus `InputRichMessageMarkdown`, and edits with a raw
  `messages.editMessage`, because Telethon's `send_message` and `edit_message`
  take no rich message. This needs **Telethon 1.44 or newer**. On an older one
  (the shared environment has 1.43.2), tsend exits 1 before connecting, naming
  the Python and its Telethon version and the two ways out: run under a newer
  Telethon, or use the Bot API backend. tsend's shebang is `/usr/bin/env
  python`, so putting another environment's `bin` first in `PATH` is enough to
  switch.
- **Bot API** (`TSEND_BACKEND=2`) calls `sendRichMessage`, and `editMessageText`
  with `rich_message` in place of `text`. python-telegram-bot 20.8 predates
  both, so tsend reaches them through its generic `do_api_request` and its
  `api_kwargs`; no upgrade is needed.

### Rules

- **Limits.** A rich message holds at most 32768 characters and 500 blocks,
  where nested blocks, list items and table rows each count. tsend checks the
  length itself and exits 1 when it is exceeded; it never splits a rich
  message, since a split can land inside a table or a code fence. Only the
  server can count blocks, so that limit arrives as its error. For long text,
  use the classic mode, which splits it and sends very long text as a file.
- **No files.** `--file` with the rich mode is refused before anything is
  sent. Send the files with a second call.
- **Failures.** A rejection (Markdown the server will not take, too many
  blocks, a missing right, no Premium) fails at once with the server's error.
  Network errors are retried as for classic sends.
- **Accounts.** Bots can send rich messages as they are. User accounts, such as
  the `-main` variants, need Premium.
- **Ids.** `--print-ids` works as for classic sends. Telegram may answer a rich
  send over MTProto with an `UpdateShortSentMessage`, which carries nothing but
  the id, so tsend reads the id from the answer and never the content.
- **Link previews** are off unless `--link-preview` is given, on both backends.
- **Edits** take the whole new Markdown. Telegram never gives a rich message's
  source back (reading one returns empty text), so there is nothing to patch.
- **Debugging.** With `-v`, a send says what Telegram answered. The Bot API
  backend also lists the blocks the server parsed the Markdown into, which is
  the only view of that there is.

### `md2tlg2` and `org2tlg2`

[agfi:md2tlg2] is [agfi:md2tlg] with the rich mode; it also sends to
`$me_tlg`, and it takes extra tsend arguments from `tsend_opts`.

[agfi:org2tlg2] converts org to GitHub-flavored Markdown (pandoc's
`gfm-tex_math_gfm`) rather than the pandoc Markdown of [agfi:org2md]. GitHub's
dialect is what Telegram's rich parser reads: pipe tables instead of
space-aligned ones, and `$...$` math instead of GitHub's own dollar-backtick
form. Unlike [agfi:org2tlg], it unescapes nothing afterwards; those `sd`
rewrites exist only because Telethon's Markdown parser shows backslash escapes
verbatim, and in a rich table cell `\|` is load-bearing. It calls tsend once
instead of [agfi:tsend-retry], because tsend already retries network errors and
what remains (no rich support, text over the limit) would make an unlimited
retry hang. It also takes `tsend_opts`. A source block keeps its language; one
without a language becomes an indented code block.

Checked live on 2026-09-29: each backend sent, edited and deleted one rich
message with a heading and a table, Telethon 1.45.0 on the bot session and the
Bot API through [agfi:org2tlg2]. The Telethon send was answered with
`Updates`, and the Bot API reported the blocks `paragraph, heading, table`.

## Copied message cleanup

`tlg-strip-metadata` removes Telegram Desktop copied-message prefixes such as `[6/11/2026  12:50] Name: ` from each line, preserving only the message body. It accepts args, stdin, or the clipboard, and copies the result when run interactively.
