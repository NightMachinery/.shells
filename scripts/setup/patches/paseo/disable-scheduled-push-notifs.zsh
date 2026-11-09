#!/usr/bin/env zsh
##
#: Patch the installed Paseo daemon so a scheduled run that merely finishes
#: sends no phone push. Permission requests still push, and the app still
#: marks the run in its sidebar. Upstream request:
#: https://github.com/getpaseo/paseo/discussions/3948
#:
#: usage: disable-scheduled-push-notifs.zsh [--revert]
#: Idempotent. An update replaces the patched file, so rerun it after every
#: update (paseo-cli-update does). It takes effect when the daemon restarts.
##
set -euo pipefail

local paseo_bin
paseo_bin="$(command -v paseo)" || { print -ru2 -- "$0: paseo is not installed"; exit 1; }

#: The pnpm wrapper names the CLI package and the node it runs on; the server
#: package is linked beside the CLI's.
local cli_dir node
cli_dir="$(command grep -o 'global/[^"]*/@getpaseo+cli@[^/]*/node_modules/@getpaseo/cli' "${paseo_bin}" | command head -1 || true)"
[[ -n "${cli_dir}" ]] || { print -ru2 -- "$0: cannot find the Paseo CLI package from ${paseo_bin}"; exit 1; }
cli_dir="${paseo_bin:h}/${cli_dir}"
node="$(command grep -o '/[^"]*/bin/node' "${paseo_bin}" | command head -1 || true)"
[[ -x "${node}" ]] || node="$(command -v node)"

local pkg_dir="${cli_dir:h}/server"
pkg_dir="${pkg_dir:A}"
local target="${pkg_dir}/dist/server/server/websocket-server.js"
[[ -f "${target}" ]] || { print -ru2 -- "$0: not found: ${target}"; exit 1; }

local marker='LOCAL PATCH schedule-push'
if [[ "${1:-}" == --revert ]] ; then
    command cp "${target}.orig" "${target}"
    print -r -- "reverted ${target}"
    exit 0
fi
if command grep -qF "${marker}" "${target}" ; then
    print -r -- "already patched: ${target}"
    exit 0
fi

command cp "${target}" "${target}.orig"
#: A literal replacement of exactly one occurrence, or nothing at all.
"${node}" -e '
const fs = require("fs");
const [file] = process.argv.slice(1);
const old = "pushEligible: isPushEligibleAttentionReason(params.reason),";
const replacement =
  "pushEligible: isPushEligibleAttentionReason(params.reason) && " +
  "!(params.reason === \"finished\" && agent.labels?.[\"paseo.schedule-id\"]), // LOCAL PATCH schedule-push";
const src = fs.readFileSync(file, "utf8");
if (src.split(old).length !== 2) {
  console.error("pattern not found exactly once: Paseo changed, update this patch");
  process.exit(1);
}
fs.writeFileSync(file, src.replace(old, replacement));
' "${target}"
"${node}" --check "${target}"
print -r -- "patched ${target}; restart the daemon (paseo-restart) to load it"
