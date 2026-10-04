#!/usr/bin/env zsh
# Run with: zsh -f zshlang/tests/tealy-mv-media.zsh
# Synthetic library only: fake SSH serves local fixtures to real rsync.

setopt errexit pipefail typesetsilent
typeset -gr tealy_media_test_root="${0:A:h:h:h}"
typeset -gx TEALY_MEDIA_TEST_TMP
TEALY_MEDIA_TEST_TMP="$(command mktemp -d "${TMPDIR:-/tmp}/tealy-mv-media.XXXXXX")"
TEALY_MEDIA_TEST_TMP="${TEALY_MEDIA_TEST_TMP:A}"
trap 'command rm -rf -- "${TEALY_MEDIA_TEST_TMP}"' EXIT

unset night_basic_plugin_loaded_p
source "${tealy_media_test_root}/zshlang/basic/basic.plugin.zsh"
source "${tealy_media_test_root}/zshlang/auto-load/others/termux-remote.zsh"
source "${tealy_media_test_root}/zshlang/auto-load/others/rsync.zsh"
function ecerr { print -ru2 -- "$@" }
function reval-ec { "$@" }
ensure-cmd rsync fzf @RET

typeset -gx TEALY_MEDIA_TEST_FIND="${commands[gfind]:-${commands[find]}}"
command mkdir -p "${TEALY_MEDIA_TEST_TMP}"/{bin,remote/storage/movies/anime/series/deeper,destination}
command mkdir -p "${TEALY_MEDIA_TEST_TMP}/remote/storage/shared/audiobooks/author/book"
command cat > "${TEALY_MEDIA_TEST_TMP}/bin/find" <<'EOF'
#!/bin/sh
exec "$TEALY_MEDIA_TEST_FIND" "$@"
EOF
command cat > "${TEALY_MEDIA_TEST_TMP}/bin/ssh" <<'EOF'
#!/bin/sh
while [ "$1" = -n ] || [ "$1" = -T ]; do shift; done
[ "$1" = tealy ] || exit 99
shift
cd "$TEALY_MEDIA_TEST_TMP/remote" || exit 99
if [ "$#" -eq 1 ]; then
    if [ "${TEALY_MEDIA_TEST_LIST_FAIL:-}" = y ]; then
        printf 'storage/movies/partial\0'
        exit 23
    fi
    exec sh -c "$1"
fi
if [ "${TEALY_MEDIA_TEST_TRANSFER_FAIL:-}" = y ]; then exit 23; fi
exec "$@"
EOF
command chmod +x "${TEALY_MEDIA_TEST_TMP}/bin/ssh" "${TEALY_MEDIA_TEST_TMP}/bin/find"
path=( "${TEALY_MEDIA_TEST_TMP}/bin" "${path[@]}" )

typeset -ga tealy_media_test_selection=()
typeset -g tealy_media_test_picker_rc=0 tealy_media_test_approve=n
function fz {
    [[ "${fz_empty:-}" == y && -z "${fzf_mru_context:-}" ]] || return 99
    local opts=("$@")
    (( ${opts[(Ie)--multi]} && ${opts[(Ie)--read0]} && ${opts[(Ie)--print0]} )) || return 99
    command cat > "${TEALY_MEDIA_TEST_TMP}/candidates"
    (( ${#tealy_media_test_selection} )) && printf '%s\0' "${tealy_media_test_selection[@]}"
    return "${tealy_media_test_picker_rc}"
}
function ask {
    [[ "$2" == N && "$(<"${TEALY_MEDIA_TEST_TMP}/review")" == *'Planned moves'* ]] || return 99
    ec ask >> "${TEALY_MEDIA_TEST_TMP}/events"
    [[ "${tealy_media_test_approve}" == y ]]
}
functions[h-tealy-media-test-rsp-mv]="${functions[rsp-mv]}"
function rsp-mv {
    ec transfer >> "${TEALY_MEDIA_TEST_TMP}/events"
    printf '%s\0' "$@" > "${TEALY_MEDIA_TEST_TMP}/transfer-args"
    h-tealy-media-test-rsp-mv "$@"
}
function tealy-media-test-fail {
    print -ru2 -- "FAIL: $1"
    command cat "${TEALY_MEDIA_TEST_TMP}/review" >&2
    exit 1
}
function tealy-media-test-expect {
    local want="$1" ret=0
    shift
    : > "${TEALY_MEDIA_TEST_TMP}/events"
    : > "${TEALY_MEDIA_TEST_TMP}/review"
    "$@" > "${TEALY_MEDIA_TEST_TMP}/stdout" 2> "${TEALY_MEDIA_TEST_TMP}/review" || ret=$?
    [[ "$ret" == "$want" ]] || tealy-media-test-fail "exit $ret, wanted $want"
}

typeset -g tealy_media_test_dest="${TEALY_MEDIA_TEST_TMP}/destination"
typeset -g tealy_media_test_movie="${TEALY_MEDIA_TEST_TMP}/remote/storage/movies"
typeset -g tealy_media_test_odd=$'line\nbreak $(printf sentinel)\047.mkv'
print -r -- 'odd sentinel' > "${tealy_media_test_movie}/${tealy_media_test_odd}"
print -r -- 'hidden sentinel' > "${tealy_media_test_movie}/.hidden"
print -r -- 'recursive sentinel' > "${tealy_media_test_movie}/anime/series/deeper/episode.mkv"
print -r -- 'keep sentinel' > "${tealy_media_test_dest}/keep.txt"
tealy_media_test_selection=( "storage/movies/${tealy_media_test_odd}" )

## Default No shows the exact plan, but retains the source and destination.
tealy-media-test-expect 130 tealy-mv-movies "${tealy_media_test_dest}"
[[ "$(<"${TEALY_MEDIA_TEST_TMP}/events")" == ask &&
    -f "${tealy_media_test_movie}/${tealy_media_test_odd}" &&
    ! -e "${tealy_media_test_dest}/${tealy_media_test_odd}" ]] || tealy-media-test-fail 'declined move'
typeset -g tealy_media_test_source="tealy:storage/movies/${tealy_media_test_odd}"
typeset -g tealy_media_test_target="${tealy_media_test_dest}/${tealy_media_test_odd}"
[[ "$(<"${TEALY_MEDIA_TEST_TMP}/review")" == *"${(q)tealy_media_test_source} -> ${(q)tealy_media_test_target}"* ]] ||
    tealy-media-test-fail 'exact, escaped source/destination plan'
typeset -ga tealy_media_test_candidates=( ${(0)"$(<"${TEALY_MEDIA_TEST_TMP}/candidates")"} )
(( ${tealy_media_test_candidates[(Ie)storage/movies/.hidden]} &&
    ${tealy_media_test_candidates[(Ie)storage/movies/anime/series/]} &&
    ! ${tealy_media_test_candidates[(Ie)storage/movies/anime/series/deeper/]} )) ||
    tealy-media-test-fail 'hidden entries or listing depth'

## Picker cancellation and an empty selection never ask or transfer.
tealy_media_test_picker_rc=130
tealy-media-test-expect 130 tealy-mv-movies "${tealy_media_test_dest}"
[[ ! -s "${TEALY_MEDIA_TEST_TMP}/events" ]] || tealy-media-test-fail 'cancelled picker'
tealy_media_test_picker_rc=0
tealy_media_test_selection=()
tealy-media-test-expect 0 tealy-mv-movies "${tealy_media_test_dest}"
[[ ! -s "${TEALY_MEDIA_TEST_TMP}/events" ]] || tealy-media-test-fail 'empty selection'

## Approval moves unusual names and full directory trees, retaining unrelated files.
tealy_media_test_approve=y
tealy_media_test_selection=( "storage/movies/anime/series/" "storage/movies/anime/" "storage/movies/${tealy_media_test_odd}" )
tealy-media-test-expect 0 tealy-mv-movies "${tealy_media_test_dest}"
[[ "$(<"${TEALY_MEDIA_TEST_TMP}/events")" == $'ask\ntransfer' &&
    ! -e "${tealy_media_test_movie}/${tealy_media_test_odd}" &&
    ! -e "${tealy_media_test_movie}/anime/series/deeper/episode.mkv" &&
    -d "${tealy_media_test_movie}/anime/series/deeper" &&
    -f "${tealy_media_test_dest}/anime/series/deeper/episode.mkv" &&
    -f "${tealy_media_test_dest}/keep.txt" &&
    "$(<"${tealy_media_test_dest}/${tealy_media_test_odd}")" == 'odd sentinel' ]] ||
    tealy-media-test-fail 'approved recursive move'
typeset -ga tealy_media_test_args=( ${(0)"$(<"${TEALY_MEDIA_TEST_TMP}/transfer-args")"} )
(( ${#tealy_media_test_args} == 4 )) || tealy-media-test-fail 'selected child transferred twice'

## Same target names, unknown picker output, and failed listing do not reach ask.
print -r -- same > "${tealy_media_test_movie}/same.mkv"
print -r -- same > "${tealy_media_test_movie}/anime/same.mkv"
tealy_media_test_selection=( storage/movies/same.mkv storage/movies/anime/same.mkv )
tealy-media-test-expect 2 tealy-mv-movies "${tealy_media_test_dest}"
[[ ! -s "${TEALY_MEDIA_TEST_TMP}/events" ]] || tealy-media-test-fail 'same destination names'
tealy_media_test_selection=( storage/movies/missing )
tealy-media-test-expect 2 tealy-mv-movies "${tealy_media_test_dest}"
[[ ! -s "${TEALY_MEDIA_TEST_TMP}/events" ]] || tealy-media-test-fail 'unknown selection'
export TEALY_MEDIA_TEST_LIST_FAIL=y
tealy-media-test-expect 23 tealy-mv-movies "${tealy_media_test_dest}"
[[ ! -s "${TEALY_MEDIA_TEST_TMP}/events" ]] || tealy-media-test-fail 'failed listing'
unset TEALY_MEDIA_TEST_LIST_FAIL

## Transfer failure retains source files and propagates rsync's nonzero status.
tealy_media_test_selection=( storage/movies/same.mkv )
export TEALY_MEDIA_TEST_TRANSFER_FAIL=y
typeset -g tealy_media_test_ret=0
tealy-mv-movies "${tealy_media_test_dest}" > /dev/null 2> "${TEALY_MEDIA_TEST_TMP}/review" || tealy_media_test_ret=$?
(( tealy_media_test_ret != 0 )) && [[ -f "${tealy_media_test_movie}/same.mkv" ]] ||
    tealy-media-test-fail 'failed transfer removed source'
unset TEALY_MEDIA_TEST_TRANSFER_FAIL

## Audiobooks use shared storage; default destination and local overrides work.
print -r -- 'book sentinel' > "${TEALY_MEDIA_TEST_TMP}/remote/storage/shared/audiobooks/author/book/chapter.mp3"
tealy_media_test_selection=( storage/shared/audiobooks/author/book/ )
tealy-media-test-expect 0 tealy-mv-audiobooks "${tealy_media_test_dest}"
[[ -f "${tealy_media_test_dest}/book/chapter.mp3" &&
    ! -e "${TEALY_MEDIA_TEST_TMP}/remote/storage/shared/audiobooks/author/book/chapter.mp3" ]] ||
    tealy-media-test-fail 'audiobook directory move'
tealy_media_test_selection=( storage/movies/.hidden )
typeset -g tealy_mv_audiobooks_root=storage/movies
builtin cd -- "${tealy_media_test_dest}"
tealy-media-test-expect 0 tealy-mv-audiobooks
[[ -f "${tealy_media_test_dest}/.hidden" && ! -e "${tealy_media_test_movie}/.hidden" ]] ||
    tealy-media-test-fail 'default destination or local override'
tealy-media-test-expect 2 tealy-mv-movies "${TEALY_MEDIA_TEST_TMP}/nonexistent"
[[ ! -s "${TEALY_MEDIA_TEST_TMP}/events" ]] || tealy-media-test-fail 'invalid destination'

print -r -- 'ok: media picker depth, confirmation, cancellation, literal filenames, recursive moves, collisions, failures, audiobooks, and local override'
