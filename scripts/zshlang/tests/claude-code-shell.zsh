#!/usr/bin/env zsh
# Regression tests for the `unalias' wrapper that lets .zshenv's aliases
# survive Claude Code's shell snapshot. See docs/claude-code-shell.md.
# Run with: zsh -f zshlang/tests/claude-code-shell.zsh

setopt errexit pipefail typesetsilent

typeset -gr cc_shell_test_root="${0:A:h:h:h}"
typeset -g cc_shell_test_tmp
cc_shell_test_tmp="$(command mktemp -d "${TMPDIR:-/tmp}/cc-shell-tests.XXXXXX")"

function cc-shell-test-cleanup {
    if [[ -n "${cc_shell_test_tmp:-}" && -d "${cc_shell_test_tmp}" &&
        "${cc_shell_test_tmp:t}" == cc-shell-tests.* ]] ; then
        command rm -rf -- "${cc_shell_test_tmp}"
    fi
}
trap cc-shell-test-cleanup EXIT

function cc-shell-test-fail {
    print -ru2 -- "FAIL: ${1}"
    exit 1
}

typeset -g cc_shell_test_dir="${cc_shell_test_tmp}/shell-snapshots"
command mkdir -p "${cc_shell_test_dir}"

#: The layout Claude Code writes, as of 2.1.280. A lean snapshot is what
#: ~/.zshrc now produces; an old one still carries every function body.
typeset -g cc_shell_test_lean="${cc_shell_test_dir}/snapshot-zsh-1-lean.sh"
command cat > "${cc_shell_test_lean}" <<'EOF'
# Snapshot file
# Unset all aliases to avoid conflicts with functions
unalias -a 2>/dev/null || true
# Functions
# Shell Options
setopt login
# Aliases
# Check for rg availability
unalias grep 2>/dev/null || true
EOF
typeset -g cc_shell_test_old="${cc_shell_test_dir}/snapshot-zsh-2-old.sh"
command cat > "${cc_shell_test_old}" <<'EOF'
# Snapshot file
# Unset all aliases to avoid conflicts with functions
unalias -a 2>/dev/null || true
# Functions
ls () {
	print -r -- snapshot-ls
}
cc_shell_after () {
	:
}
# Shell Options
# Aliases
alias -- ll='ls -l'
EOF

#: One shell per case, as Claude Code starts one per command: the library
#: under CLAUDECODE=1 with an alias named like a snapshot function, then the
#: snapshot, then a report.
function cc-shell-test-run {
    local snapshot="${1}"
    CLAUDECODE=1 command zsh -f -c '
        source "$1/zshlang/auto-load/others/claude-code-shell.zsh"
        alias ls="print -r -- alias-ls" grep="print -r -- alias-grep"
        alias -g @RET=" || return \$?"
        source "$2" 2>/dev/null || true
        print -r -- "ls=${+aliases[ls]} grep=${+aliases[grep]} RET=${+galiases[@RET]} RETalias=${+aliases[@RET]} ll=${+aliases[ll]} after=${+functions[cc_shell_after]} lsfn=${+functions[ls]}"
    ' _ "${cc_shell_test_root}" "${snapshot}"
}

typeset -g out
out="$(cc-shell-test-run "${cc_shell_test_lean}")"
[[ "${out}" == 'ls=1 grep=0 RET=0 RETalias=1 ll=0 after=0 lsfn=0' ]] ||
    cc-shell-test-fail "lean snapshot keeps .zshenv's aliases, minus Claude Code's own unalias grep: ${out}"

#: The regression: skipping `unalias -a' here made `ls () {' parse as the
#: alias, which stopped the file before anything after it was defined.
out="$(cc-shell-test-run "${cc_shell_test_old}")"
[[ "${out}" == 'ls=0 grep=0 RET=0 RETalias=0 ll=1 after=1 lsfn=1' ]] ||
    cc-shell-test-fail "old snapshot still gets the real unalias -a and parses to the end: ${out}"

#: Outside Claude Code the builtin is untouched.
out="$(command zsh -f -c 'unset CLAUDECODE; source "$1/zshlang/auto-load/others/claude-code-shell.zsh"; whence -w unalias' _ "${cc_shell_test_root}")"
[[ "${out}" == 'unalias: builtin' ]] ||
    cc-shell-test-fail "unalias wrapped without CLAUDECODE: ${out}"

#: The lean test reads the header layout only.
function cc-shell-test-lean-p {
    command zsh -f -c 'source "$1/zshlang/auto-load/others/claude-code-shell.zsh"; h-claude-code-snapshot-lean-p "$2"' _ "${cc_shell_test_root}" "${1}"
}
cc-shell-test-lean-p "${cc_shell_test_lean}" || cc-shell-test-fail 'lean snapshot not recognised'
! cc-shell-test-lean-p "${cc_shell_test_old}" || cc-shell-test-fail 'old snapshot taken for lean'
print -r -- 'no header here' > "${cc_shell_test_dir}/snapshot-zsh-3-odd.sh"
! cc-shell-test-lean-p "${cc_shell_test_dir}/snapshot-zsh-3-odd.sh" || cc-shell-test-fail 'unknown layout taken for lean'

typeset -g cc_shell_test_probe="${cc_shell_test_tmp}/alias-policy.zsh"
command cat > "${cc_shell_test_probe}" <<'EOF'
source "$1/zshlang/basic/magicmacros.zsh"
source "$1/zshlang/auto-load/others/claude-code-shell.zsh"
alias cc_shell_current='printf "%s\n" current-alias'
alias -g 'CC_LITERAL=$(printf "%s" ALIAS_BODY_EXECUTED)'
typeset -A cc_shell_global_bodies=( "${(@kv)galiases}" )
function cc-shell-test-return {
    false @RET
    printf '%s\n' RETURN_MACRO_FELL_THROUGH
}
source "$2"
setopt noextendedglob nobareglobqual
(( ${#galiases} == 0 )) || exit 1
for name in "${(@k)cc_shell_global_bodies}" ; do
    [[ "${aliases[$name]}" == "${cc_shell_global_bodies[$name]}" ]] || exit 1
done
[[ "${aliases[cc_shell_current]}" == 'printf "%s\n" current-alias' ]] || exit 1
out="$(cc-shell-test-return)"
rc=$?
[[ "${rc}" == 1 && -z "${out}" ]] || exit 1
out="$(eval 'command printf "arg=<%s>\n" ... MAGIC @f @RET')" || exit 1
[[ "${out}" == $'arg=<...>\narg=<MAGIC>\narg=<@f>\narg=<@RET>' ]] || exit 1
out="$(eval 'command printf "arg=<%s>\n" "..." "MAGIC" "@f" "@RET"')" || exit 1
[[ "${out}" == $'arg=<...>\narg=<MAGIC>\narg=<@f>\narg=<@RET>' ]] || exit 1
out="$(eval cc_shell_current)" || exit 1
[[ "${out}" == current-alias ]] || exit 1
builtin cd -- "$3" || exit 1
out="$(eval 'command cat payload.txt MAGIC')" || exit 1
[[ "${out}" == "$(< payload.txt)"$'\nSECOND_LITERAL_FILE' ]] || exit 1
out="$(eval 'command grep -F MAGIC magic.h')" || exit 1
[[ "${out}" == MAGIC ]] || exit 1
print -r -- 'ok: current alias bodies, literal arguments and compiled return macro'
EOF
printf '%s\n' 'printf "%s\n" FILE_CONTENT_EXECUTED' > "${cc_shell_test_tmp}/payload.txt"
printf '%s\n' SECOND_LITERAL_FILE > "${cc_shell_test_tmp}/MAGIC"
printf '%s\n' MAGIC > "${cc_shell_test_tmp}/magic.h"
out="$(CLAUDECODE=1 command zsh -f "${cc_shell_test_probe}" \
    "${cc_shell_test_root}" "${cc_shell_test_lean}" "${cc_shell_test_tmp}")" ||
    cc-shell-test-fail 'current alias policy failed'
[[ "${out}" == 'ok: current alias bodies, literal arguments and compiled return macro' ]] ||
    cc-shell-test-fail "alias conversion evaluated a literal body: ${out}"

out="$(CLAUDECODE=1 command zsh -f -c '
    source "$1/zshlang/basic/magicmacros.zsh"
    source "$1/zshlang/auto-load/others/claude-code-shell.zsh"
    alias cc_shell_target="printf target"
    unalias cc_shell_target || exit 1
    [[ "${+galiases[MAGIC]}" == 1 ]] || exit 1
    unalias -a || exit 1
    (( ${#aliases} == 0 && ${#galiases} == 0 )) || exit 1
    print -r -- transparent
' _ "${cc_shell_test_root}")" || cc-shell-test-fail 'inherited CLAUDECODE without snapshot changes unalias behavior'
[[ "${out}" == transparent ]] || cc-shell-test-fail "inherited-marker forwarding: ${out}"

print -r -- 'ok: claude code shell snapshot'
