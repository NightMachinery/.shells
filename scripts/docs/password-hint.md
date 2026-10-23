# macOS login password hint

Run `password-hint-darwin` to read the current shell user's local account hint:

```zsh
password-hint-darwin
```

The helper uses `dscl . -read "/Users/$(whoami)" hint`. Lowercase `hint` is
the attribute used by the [macOS Security Compliance Project](https://raw.githubusercontent.com/usnistgov/macos_security/tahoe/rules/os/os_password_hint_remove.yaml).
It preserves `dscl`'s output, diagnostics and exit status, including the
diagnostic when no hint attribute exists. It does not require sudo.

The account is the one running the shell, which can differ from the user at
the desktop. This reads the login password hint, not the password itself.

Implementation: [macOS.zsh](../zshlang/auto-load/others/macOS/macOS.zsh).
