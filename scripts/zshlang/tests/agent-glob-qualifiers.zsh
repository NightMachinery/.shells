#!/usr/bin/env zsh
# Regression tests for glob qualifiers in the agent tooling under Claude Code's
# shell, which runs commands with NO_BARE_GLOB_QUAL and NO_EXTENDED_GLOB. There
# a bare `(N)' is a literal, so a function that globs with one fails with "no
# matches found" when called from a `!' line.
# Run with: zsh -f zshlang/tests/agent-glob-qualifiers.zsh

setopt errexit pipefail typesetsilent

typeset -gr glob_qual_test_root="${0:A:h:h:h}"
typeset -g glob_qual_test_tmp
glob_qual_test_tmp="$(command mktemp -d "${TMPDIR:-/tmp}/glob-qual-tests.XXXXXX")"

function glob-qual-test-cleanup {
    if [[ -n "${glob_qual_test_tmp:-}" && -d "${glob_qual_test_tmp}" &&
        "${glob_qual_test_tmp:t}" == glob-qual-tests.* ]] ; then
        command rm -rf -- "${glob_qual_test_tmp}"
    fi
}
trap glob-qual-test-cleanup EXIT

function glob-qual-test-fail {
    print -ru2 -- "FAIL: ${1}"
    exit 1
}

#: The basic plugin is not nounset-clean, so that option waits until it has
#: loaded. aliasfn only defines top-level conveniences, which no test needs.
unset night_basic_plugin_loaded_p
source "${glob_qual_test_root}/zshlang/basic/basic.plugin.zsh"
function aliasfn { :; }
source "${glob_qual_test_root}/zshlang/plugins/agent-session/session.zsh"
source "${glob_qual_test_root}/zshlang/auto-load/others/claude-session.zsh"
#: The basic plugin's coloured diagnostics need the full local colour stack.
function ecerr { print -ru2 -- "$@" }
setopt nounset

## Behaviour: a session uuid resolves from the second profile.
#: The reported failure: `2paseo <uuid>' from a `!' line, with the session in
#: `~/.claude/projects' and `~/.claude-work/projects' globbed first.
typeset -g glob_qual_test_id=05cada92-1111-4111-8111-111111111111
typeset -ga claude_code_session_projects_dirs=(
    "${glob_qual_test_tmp}/work/projects"
    "${glob_qual_test_tmp}/default/projects"
)
command mkdir -p "${glob_qual_test_tmp}/work/projects/-other" \
    "${glob_qual_test_tmp}/default/projects/-p"
typeset -g glob_qual_test_transcript="${glob_qual_test_tmp}/default/projects/-p/${glob_qual_test_id}.jsonl"
print -r -- '{}' > "${glob_qual_test_transcript}"

function glob-qual-test-expect-resolve {
    local name="${1}" input="${2}" want_rc="${3}" want_out="${4}"
    local out rc=0
    out="$(setopt nobareglobqual noextendedglob ; h-claude-code-session-resolve "${input}" 2>/dev/null)" || rc=$?
    [[ "${rc}" == "${want_rc}" ]] ||
        glob-qual-test-fail "${name}: exit ${rc}, expected ${want_rc}"
    [[ "${out}" == "${want_out}" ]] ||
        glob-qual-test-fail "${name}: printed '${out}', expected '${want_out}'"
}

glob-qual-test-expect-resolve 'full uuid in the second profile' \
    "${glob_qual_test_id}" 0 "${glob_qual_test_transcript}"
glob-qual-test-expect-resolve 'uuid prefix in the second profile' \
    "${glob_qual_test_id[1,8]}" 0 "${glob_qual_test_transcript}"
glob-qual-test-expect-resolve 'unknown uuid' \
    99999999-9999-4999-8999-999999999999 1 ''

## Structure: every agent tooling function with a bare qualifier restores it.
#: A function counts as guarded when it sets bareglobqual or runs under
#: `emulate -L zsh'; option names are matched with case and underscores
#: ignored, as zsh does. Only `(N...)' qualifiers are looked for: every
#: qualifier in these files has used the null-glob flag so far.
typeset -g glob_qual_test_unguarded
glob_qual_test_unguarded="$(
    builtin cd -q -- "${glob_qual_test_root}" &&
    command perl - zshlang/auto-load/others/{agent-*,agents,agents-md,agy-session,claude-session,codex-session,hold}.zsh \
        zshlang/plugins/agent-session/*.zsh <<'EOF'
use strict; use warnings;
for my $file (@ARGV) {
    open my $fh, '<', $file or die "$file: $!\n";
    my ($fn, $guard, $use);
    while (my $line = <$fh>) {
        if ($line =~ /^function\s+(\S+)\s*\{/) { ($fn, $guard, $use) = ($1, 0, undef); next }
        next unless defined $fn;
        next if $line =~ /^\s*#/;
        (my $norm = lc $line) =~ tr/_//d;
        $guard = 1 if $norm =~ /\bsetopt\b.*\bbareglobqual\b/ || $norm =~ /\bemulate\s+-l\s+zsh\b/;
        #: A qualifier follows the pattern directly; `${(N...' and `$(N' are not one.
        $use //= $. if $line =~ /[^\s{\$(]\(N[^()\s]*\)/;
        if ($line =~ /^\}/) {
            print "$file:$use: $fn\n" if defined $use && !$guard;
            undef $fn;
        }
    }
}
EOF
)"
[[ -z "${glob_qual_test_unguarded}" ]] ||
    glob-qual-test-fail $'functions glob with (N) without `setopt localoptions bareglobqual\':\n'"${glob_qual_test_unguarded}"

print -r -- 'ok: agent glob qualifiers'
