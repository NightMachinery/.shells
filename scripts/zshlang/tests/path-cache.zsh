#!/usr/bin/env zsh
emulate -R zsh
setopt ERR_EXIT PIPE_FAIL

repo="${${0:A}:h:h:h}"
source "${repo}/zshlang/basic/auto-load/path.zsh"
source "${repo}/zshlang/basic/auto-load/path-cache.zsh"

function fail {
    print -ru2 -- "FAIL: ${*}"
    exit 1
}

fixture="$(command mktemp -d "${TMPDIR:-/tmp}/night-path-test.XXXXXXXX")"
trap 'command rm -rf -- "${fixture}"' EXIT
export XDG_CACHE_HOME="${fixture}/cache"
export XDG_CONFIG_HOME="${fixture}/config"
export NIGHTDIR="${fixture}/scripts"
git_dir="${XDG_CONFIG_HOME}/vcsh/repo.d/night.sh.git"
h-night-path-cache-file
cache="${REPLY}"

function executable {
    print -r -- '#!/bin/sh' > "${1}"
    command chmod +x "${1}"
}

command mkdir -p -- "${git_dir}" "${NIGHTDIR}"/{a-first,'z last',source-only,empty,symlinks,'build.app/Contents'}
executable "${NIGHTDIR}/a-first/duplicate"
executable "${NIGHTDIR}/z last/duplicate"
executable "${NIGHTDIR}/a-first/.hidden"
executable "${NIGHTDIR}/build.app/Contents/app"
print -r -- ':' > "${NIGHTDIR}/source-only/library"
command ln -s -- ../a-first/duplicate "${NIGHTDIR}/symlinks/executable-link"
command ln -s -- ../gone "${NIGHTDIR}/empty/broken-link"

path=( "${NIGHTDIR}/source-only" "${fixture}/missing" /usr/bin /bin )
h-night-path-apply
h-night-path-drop-missing
[[ -r "${cache}" ]] || fail 'missing cache was not rebuilt'
[[ "$(whence -p duplicate)" == "${NIGHTDIR}/z last//duplicate" ]] || fail 'recursive glob precedence changed'
[[ "$(whence -p executable-link)" == "${NIGHTDIR}/symlinks//executable-link" ]] || fail 'executable symlink dropped'
(( ${#path} == 5 )) || fail "unexpected PATH entries: ${(qq)path}"

command mkdir -p -- "${NIGHTDIR}/later"
executable "${NIGHTDIR}/later/new-command"
path=( /usr/bin /bin )
h-night-path-apply
[[ -z "$(whence -p new-command)" ]] || fail 'fresh cache scanned for new directories'

command touch -t 200001010000 "${cache}"
command touch "${git_dir}/index"
h-night-path-apply
[[ -z "$(whence -p new-command)" ]] || fail 'stale cache did not serve old PATH'
for attempt in {1..100}; do
    [[ "${cache}" -nt "${git_dir}/index" ]] && break
    command sleep 0.02
done
[[ "${cache}" -nt "${git_dir}/index" ]] || fail 'stale cache did not refresh'
h-night-path-apply
[[ -n "$(whence -p new-command)" ]] || fail 'refreshed cache omitted the new directory'

# A truncated cache and one with a header and footer but an invalid path
# must both be ignored and replaced.
print -nr -- 'truncated' > "${cache}"
path=( /usr/bin /bin )
h-night-path-apply
[[ -n "$(whence -p new-command)" ]] || fail 'truncated cache was not rebuilt'
print -rN -- "night-path-v1:${NIGHTDIR}" '+/outside/' 'end:3' > "${cache}"
path=( /usr/bin /bin )
h-night-path-apply
[[ -n "$(whence -p new-command)" && "${path[(Ie)/outside/]}" == 0 ]] || fail 'invalid cache was trusted'

print -r -- 'ok: PATH cache preserves order, filters entries, and recovers from missing, stale and corrupt caches'
