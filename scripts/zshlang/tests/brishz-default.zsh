#!/usr/bin/env zsh
# Run with: zsh -f zshlang/tests/brishz-default.zsh
# Check the real wrapper's external-client boundary without using a garden.

setopt errexit pipefail typesetsilent
typeset -gr brishz_test_root="${0:A:h:h:h}"
typeset -g brishz_test_tmp
brishz_test_tmp="$(command mktemp -d "${TMPDIR:-/tmp}/brishz-default.XXXXXX")"
trap 'command rm -rf -- "${brishz_test_tmp}"' EXIT

unset night_basic_plugin_loaded_p
source "${brishz_test_root}/zshlang/basic/basic.plugin.zsh"
source "${brishz_test_root}/zshlang/auto-load/others/brish.zsh"
function ecerr { print -ru2 -- "$@" }

typeset -g NIGHTDIR="${brishz_test_root}"
path=( "${brishz_test_tmp}" "${path[@]}" )
command cat > "${brishz_test_tmp}/brishzgo" <<'EOF'
#!/bin/sh
printf '%s\n' go "${brishz_session}" "${brishz_nolog}" "${brishz_in}" "${brishz_stream}" "$@"
if [ -n "${brishz_async}" ]; then printf 'async=%s\n' "${brishz_async}"; fi
if [ -n "${brishz_copy}" ]; then printf 'copy=%s\n' "${brishz_copy}"; fi
if [ "${brishz_in}" = MAGIC_READ_STDIN ]; then command cat; fi
exit 7
EOF
command cat > "${brishz_test_tmp}/brishzq.zsh" <<'EOF'
#!/bin/sh
printf '%s\n' v1 "${brishz_session}" "${brishz_copy}" "$@"
exit 9
EOF
command chmod +x "${brishz_test_tmp}/brishzgo" "${brishz_test_tmp}/brishzq.zsh"

# Still call the guard, but do not install anything during the test.
function go-local-dep {
    [[ "$1" == brishzgo && "$2" == "${brishz_test_root}/golang/brishzgo" ]]
}

function brishz-test-expect {
    local want_rc="$1" want_out="$2"
    shift 2
    local out rc=0
    # Sentinel preserves the client's trailing newlines in command substitution.
    out="$( "$@"; rc=$?; print -rn -- .; exit "${rc}" )" || rc=$?
    [[ "${rc}" == "${want_rc}" && "${out}" == "${want_out}." ]] || {
        print -ru2 -- "FAIL: ${(q)@}: exit ${rc}, output ${(qq)out}"
        exit 1
    }
}

brishz_s='short session' brishz_nolog=y brishz_in='literal' brishz_stream=n \
    brishz-test-expect 7 $'go\nshort session\ny\nliteral\nn\nprintf\na b\nit\'s\n' \
    brishz printf 'a b' "it's"

brishz_s=short brishz_session=full \
    brishz-test-expect 7 $'go\nfull\n\n\n\ntrue\n' brishz true

print -rn -- $'input\n\n' | brishz-test-expect 7 \
    $'go\n\n\nMAGIC_READ_STDIN\n\ncat\ninput\n\n' brishz-in cat

print -rn -- $'direct\n\n' | brishz_in=MAGIC_READ_STDIN \
    brishz-test-expect 7 $'go\n\n\nMAGIC_READ_STDIN\n\ncat\ndirect\n\n' brishz cat

brishz_s=old brishz_c=y brishz-test-expect 9 $'v1\nold\ny\ntrue\n' brishz-v1 true
brishz_async=y brishz-test-expect 7 $'go\n\n\n\n\ntrue\nasync=y\n' brishz true
brishz_c=y brishz-test-expect 7 $'go\n\n\n\n\ntrue\ncopy=y\n' brishz true
brishz_c=y brishz_copy=explicit brishz-test-expect 7 $'go\n\n\n\n\ntrue\ncopy=explicit\n' brishz true
brishz-test-expect 9 $'v1\nopts\n\ntrue\n' @opts session opts @ brishz-v1 true

function go-local-dep { return 42 }
brishz-test-expect 42 '' brishz true

print -r -- 'PASS: brishz default, stdin, argv, status, dependency guard and brishz-v1'
