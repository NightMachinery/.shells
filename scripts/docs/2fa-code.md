# `2fa-code`

`2fa-code` copies a TOTP code for a raw Base32 2FA secret or an `otpauth://` URL, then prints the remaining validity window.

```zsh
2fa-code EX6TRKUH4Q6NMZWPE4RIWVRLHF2DION6
2fa-code 'otpauth://totp/example?secret=EX6TRKUH4Q6NMZWPE4RIWVRLHF2DION6&issuer=Example'
```

The generated code is sent through `ec-copy`, so it is echoed and copied to the clipboard. The validity duration is printed with `ecgray`.

## Dependency

The helper uses `oathtool` from OATH Toolkit. Install `oath-toolkit` with Homebrew or `oathtool` on apt-based Linux systems.
