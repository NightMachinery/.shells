# Telegram helpers


## Destination safety

Zsh Telegram helpers that accept a `dest` argument, and the Python `tsend.py` receiver argument, abort before sending when the destination is empty or whitespace-only.

## Copied message cleanup

`tlg-strip-metadata` removes Telegram Desktop copied-message prefixes such as `[6/11/2026  12:50] Name: ` from each line, preserving only the message body. It accepts args, stdin, or the clipboard, and copies the result when run interactively.
