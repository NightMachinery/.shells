#!/usr/bin/env zsh
# Run: zsh -c 'source "$NIGHTDIR/zshlang/tests/agfi.zsh"'
# Inert: uses temporary fixture directories and stubbed agsi/fz/isI with no GUI or editor calls.
(
    setopt localoptions pipefail
    unsetopt errexit

    local fixture
    fixture="$(command mktemp -d "${TMPDIR:-/tmp}/agfi-test.XXXXXX")" || exit 1
    trap 'command rm -rf -- "$fixture"' EXIT

    command mkdir -p "${fixture}/repo/node_modules/pkg" "${fixture}/repo/bin" "${fixture}/repo/sub" "${fixture}/tmp"

    print -r -- 'TASKS.org
ignored.txt' > "${fixture}/repo/.gitignore"
    print -r -- '!TASKS.org
node_modules/' > "${fixture}/repo/.rgignore"

    print -r -- '* TASKS sentinel
function ignored_in_org {' > "${fixture}/repo/TASKS.org"
    print -r -- 'ignored gitignore line' > "${fixture}/repo/ignored.txt"
    print -r -- 'function meeting-p { return 99; }' > "${fixture}/repo/node_modules/pkg/dep.js"
    print -r -- 'function docs_fn { return 1; }' > "${fixture}/repo/notes.md"
    print -r -- '{"function json_fn": true}' > "${fixture}/repo/data.json"
    print -r -- '#!/usr/bin/env zsh
function extless_fn { return 0; }' > "${fixture}/repo/bin/tool"

    print -r -- 'function retry-eval() { :; }
alifn retry="retry-limited 0"
alias -g retry_g="echo ok"
aliasfn other_fn retry_arg=1
  retryAt.clear();' > "${fixture}/repo/sub/defs.zsh"

    print -r -- 'function alpha_fn { :; }' > "${fixture}/repo/b.zsh"
    print -r -- 'function alpha_fn { :; }' > "${fixture}/repo/a.zsh"

    local failed=0
    function assert_contains {
        local label="$1" haystack="$2" needle="$3"
        if [[ "$haystack" != *"$needle"* ]] ; then
            print -ru2 -- "FAIL: ${label}: expected output to contain '${needle}', got:\n${haystack}"
            failed=1
        fi
    }
    function assert_not_contains {
        local label="$1" haystack="$2" needle="$3"
        if [[ "$haystack" == *"$needle"* ]] ; then
            print -ru2 -- "FAIL: ${label}: expected output NOT to contain '${needle}', got:\n${haystack}"
            failed=1
        fi
    }

    function stub_agsi {
        ugbase_follow=n nightNotes="${fixture}/repo" ntsearch_additional_paths=() ntsearch_glob='' ntl-rg "$@"
    }

    local NIGHTDIR="${fixture}/repo"
    local out

    #: 1. agfi matches function, alifn, alias -g, and rejects method calls / unrelated alias args
    out="$(fnswap h_noglob_agsi stub_agsi fnswap isI false agfi retry 2>/dev/null)"
    assert_contains "function match" "$out" "/sub/defs.zsh:1:function retry-eval()"
    assert_contains "alifn match" "$out" '/sub/defs.zsh:2:alifn retry="retry-limited 0"'
    assert_contains "alias -g match" "$out" '/sub/defs.zsh:3:alias -g retry_g="echo ok"'
    assert_not_contains "reject alias unrelated arg" "$out" "other_fn"
    assert_not_contains "reject method call" "$out" "retryAt.clear"

    #: 2. agfi ignores node_modules and docs, keeps extensionless scripts
    out="$(fnswap h_noglob_agsi stub_agsi fnswap isI false agfi extless_fn 2>/dev/null)"
    assert_contains "extensionless script match" "$out" "/bin/tool:2:function extless_fn"

    if fnswap h_noglob_agsi stub_agsi fnswap isI false agfi meeting-p >/dev/null 2>&1 ; then
        print -ru2 -- "FAIL: agfi should ignore node_modules/"
        failed=1
    fi
    if fnswap h_noglob_agsi stub_agsi fnswap isI false agfi docs_fn >/dev/null 2>&1 ; then
        print -ru2 -- "FAIL: agfi should ignore .md files"
        failed=1
    fi
    if fnswap h_noglob_agsi stub_agsi fnswap isI false agfi ignored_in_org >/dev/null 2>&1 ; then
        print -ru2 -- "FAIL: agfi should ignore .org files"
        failed=1
    fi

    #: 3. ntsearch_fd_h --no-require-git respects .gitignore while .rgignore !TASKS.org overrides it
    out="$(ntsearch_lines_nnp=y stub_agsi 'TASKS sentinel' 2>/dev/null)"
    assert_contains ".rgignore !TASKS.org keeps TASKS.org searchable" "$out" "TASKS sentinel"
    if ntsearch_lines_nnp=y stub_agsi 'ignored gitignore line' >/dev/null 2>&1 ; then
        print -ru2 -- "FAIL: ntsearch should respect .gitignore without .git"
        failed=1
    fi

    #: 4. ntsearch_sort=path keeps deterministic sorted output
    out="$(ntsearch_sort=path fnswap h_noglob_agsi stub_agsi fnswap isI false agfi alpha_fn 2>/dev/null)"
    if [[ "$out" != *"/a.zsh:1:"*"/b.zsh:1:"* ]] ; then
        print -ru2 -- "FAIL: ntsearch_sort=path ordering check failed:\n${out}"
        failed=1
    fi

    #: 5. interactive agfi feeds matching lines directly to fz (not ugfz)
    local post_seen=''
    function stub_agfi_fz {
        command tr '\0' '\n' > "${fixture}/fz_seen.txt"
        print -rn -- $'\0/sub/defs.zsh:1:function retry-eval()\0'
    }
    function stub_postprocess {
        post_seen="${(F)out}"
    }
    fnswap isI true fnswap h_noglob_agsi stub_agsi fnswap fz stub_agfi_fz fnswap ntsearch-postprocess stub_postprocess agfi retry-eval >/dev/null 2>&1
    assert_contains "interactive agfi streams direct rg matches to fz" "$(<"${fixture}/fz_seen.txt")" "/sub/defs.zsh:1:function retry-eval()"
    assert_contains "interactive agfi forwards selected line to postprocess" "$post_seen" "/sub/defs.zsh:1:function retry-eval()"

    #: 6. ugfz non-interactive streaming and interactive temp cleanup
    out="$(print -rl -- 'line one' 'match here' 'line three' | fnswap isI false ugfz 'match' 2>/dev/null)"
    if [[ "$out" != 'match here' ]] ; then
        print -ru2 -- "FAIL: ugfz non-interactive expected 'match here', got '${out}'"
        failed=1
    fi

    local before_tmp after_tmp
    before_tmp=(${fixture}/tmp/ugfz.*(N))
    function stub_gmktemp { command mktemp "${fixture}/tmp/ugfz.XXXXXX"; }
    function stub_ug_fz { command cat >/dev/null; return 0; }
    print -rl -- 'a:1:function foo' | fnswap isI true fnswap gmktemp stub_gmktemp fnswap fz stub_ug_fz ugfz 'foo' >/dev/null 2>&1
    after_tmp=(${fixture}/tmp/ugfz.*(N))
    if (( ${#after_tmp} != ${#before_tmp} )) ; then
        print -ru2 -- "FAIL: ugfz leaked temp file(s): ${after_tmp[*]}"
        failed=1
    fi

    (( failed )) && exit 1
    print -r -- 'agfi: all checks passed'
)