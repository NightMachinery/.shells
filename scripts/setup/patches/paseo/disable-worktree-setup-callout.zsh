#!/usr/bin/env zsh
##
#: Patch the Paseo desktop app so its sidebar never shows "Set up worktree
#: scripts". Dismissing that callout lasts for one project only, and every git
#: worktree created outside Paseo becomes a project of its own
#: (https://github.com/getpaseo/paseo/issues/9192), so it keeps coming back.
#: Upstream request for a global opt-out:
#: https://github.com/getpaseo/paseo/discussions/9197
#:
#: usage: disable-worktree-setup-callout.zsh [--revert]
#: PASEO_APP overrides the bundle (default /Applications/Paseo.app).
#: Idempotent, and a no-op without the app. A desktop update replaces the
#: bundle, so rerun after one (login and paseo-cli-update do). It takes effect
#: when the app is relaunched.
#:
#: The edit sits in app-dist, outside app.asar and its integrity hash. It does
#: break the bundle's code-signature seal, which macOS does not re-check at
#: launch for an app it has already run, and which the updater does not need:
#: it verifies the downloaded update, not the installed app.
##
set -euo pipefail

local app="${PASEO_APP:-/Applications/Paseo.app}"
local web="${app}/Contents/Resources/app-dist/_expo/static/js/web"
if [[ ! -d "${web}" ]] ; then
    print -r -- "no Paseo desktop app at ${app}; nothing to patch"
    exit 0
fi

local marker='LOCAL PATCH worktree-setup-callout'
local pattern='shouldShowWorktreeSetupCallout=function\([A-Za-z0-9_$]+\)\{return'
local -a hits
hits=( ${(f)"$(command grep -l -E "${pattern}" "${web}"/*.js || true)"} )
(( ${#hits} == 1 )) || { print -ru2 -- "$0: expected the callout in one bundle file, found ${#hits}: Paseo changed, update this patch"; exit 1; }
local target="${hits[1]}"

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
#: Exactly one occurrence, or nothing at all: `return false&&<original>`.
PATTERN="${pattern}" MARKER="${marker}" command perl -0777 -i -pe '
    my $n = s{($ENV{PATTERN})}{$1/*$ENV{MARKER}*/!1&&}g;
    die "pattern matched $n times: Paseo changed, update this patch\n" unless $n == 1;
' "${target}" || { command cp "${target}.orig" "${target}"; exit 1; }
if (( ${+commands[node]} )) && ! command node --check "${target}" ; then
    command cp "${target}.orig" "${target}"
    print -ru2 -- "$0: the patched bundle does not parse; restored the original"
    exit 1
fi
print -r -- "patched ${target}; relaunch Paseo to load it"
