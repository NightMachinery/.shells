# Remote Termux clipboard

`h-paste-from-remote-termux HOST` prints the clipboard of a remote Termux host
to standard output:

```zsh
h-paste-from-remote-termux phone
```

The function runs `termux-clipboard-get` over SSH without allocating a terminal.
SSH is detached from local standard input, so it cannot consume data from an
enclosing pipeline.
It streams the returned bytes directly, preserving newlines and leaving the
local clipboard unchanged. SSH connection and remote-command failures are
returned to the caller.

This read path has no relationship to Emacs's mobile-clipboard reachability
cache: every invocation starts a new SSH command. It requires an SSH server on
the Termux device and the Termux:API add-on plus the `termux-api` package, which
provides `termux-clipboard-get`. Neither function logs the
clipboard contents.

Android may restrict clipboard reads while Termux is in the background. If the
remote command succeeds but does not return the expected clipboard, bring
Termux to the foreground and retry.

`tealy-paste` is the convenience function for the SSH host alias `tealy`:

```zsh
tealy-paste
```

Exactly one non-empty host is required. Hosts beginning with `-` or containing
whitespace are rejected so they cannot be interpreted as SSH options or split
into extra arguments. Configure hostnames, users, ports, and keys in
`~/.ssh/config`; for example, the `tealy` alias belongs there rather than in
this function.
