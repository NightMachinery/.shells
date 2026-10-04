package main

const helpText = `Usage: brishzgo [--] COMMAND [ARG...]

Run a zsh command in BrishGarden. Output streams by default; older gardens
fall back to raw or JSON. The exit status is the command's status.

  -h, --help     Show this help when used as the sole argument.
  --             End client options; pass everything after it to the garden.
  -c             Legacy alias for --.

Examples:
  brishzgo print -r -- hello
  brishzgo command --help
  brishzgo -- --help
  brishz_in=MAGIC_READ_STDIN brishzgo cat < input.bin

Environment:
  bshEndpoint              Garden base URL (default http://127.0.0.1:7230).
  GARDEN_PORT              Local port when bshEndpoint is unset.
  brishz_in                Literal stdin, or MAGIC_READ_STDIN to read stdin.
  brishz_session           Persistent garden session.
  brishz_async             Non-empty launches a detached request; discard output.
  brishz_copy, brishz_c     Non-empty copies a replay command with pbcopy.
  brishz_nolog             Non-empty disables garden logging.
  brishz_failure_expected  Non-empty marks failures as expected.
  brishz_stream=n          Disable streaming (raw/JSON instead).
  brishz_raw=n             Disable raw fallback (JSON instead).
  brishz_binary=y          Require exact byte transport.
  brishz_noquote=y         Join argv as shell code without quoting.
  brishz_debug=y           Log requests to stderr, with credentials redacted.
  DISABLE_BRISH=y          Disable garden requests (local help still works).

See docs/brishzgo.md in the scripts repository for authentication, proxy
variables, fallback behavior and exit statuses.
`
