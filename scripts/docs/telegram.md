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

## Copied message cleanup

`tlg-strip-metadata` removes Telegram Desktop copied-message prefixes such as `[6/11/2026  12:50] Name: ` from each line, preserving only the message body. It accepts args, stdin, or the clipboard, and copies the result when run interactively.
