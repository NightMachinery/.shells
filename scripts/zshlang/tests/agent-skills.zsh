#!/usr/bin/env zsh
# Regression tests for the shared-skill linker. Run with: zsh -f this-file

setopt errexit nounset pipefail

typeset -gr agent_skills_test_root="${0:A:h:h:h}"
typeset -g agent_skills_test_tmp
agent_skills_test_tmp="$(command mktemp -d "${TMPDIR:-/tmp}/agent-skills-test.XXXXXX")"

function agent-skills-test-cleanup {
    if [[ -n ${agent_skills_test_tmp:-} && -d ${agent_skills_test_tmp} && ${agent_skills_test_tmp:t} == agent-skills-test.* ]] ; then
        command rm -rf -- "${agent_skills_test_tmp}"
    fi
}
trap agent-skills-test-cleanup EXIT

function agent-skills-test-fail {
    print -ru2 -- "FAIL: $*"
    return 1
}

function agent-skills-test-assert {
    local description="${1}"
    shift
    "$@" || agent-skills-test-fail "${description}"
}

# agent-done.zsh only uses aliasfn while it is being sourced; aliases outside
# this test's subject do not need to be installed.
function aliasfn { :; }
typeset -g NIGHTDIR="${agent_skills_test_root}"
typeset -g agent_skills_src_dir="${agent_skills_test_tmp}/source"
typeset -g agent_skills_notes_dir="${agent_skills_test_tmp}/notes skills"
# Real machines default this to ~/.night-gcp/skills, which exists on some of
# them. The fixture below is the whole world as far as these assertions are
# concerned, so opt out rather than let a host's own skills leak into it.
typeset -ga agent_skills_extra_roots=()
typeset -ga agent_skills_repo_roots=()
source "${agent_skills_test_root}/zshlang/basic/basic.plugin.zsh"
# The minimal plugin deliberately excludes presentation helpers used only for
# diagnostics by agent-done.zsh.
function ec { print -r -- "$*"; }
function ecerr { print -ru2 -- "$*"; }
function ecgray { print -r -- "$*"; }
function ecbold { print -r -- "$*"; }
source "${agent_skills_test_root}/zshlang/auto-load/others/agent-done.zsh"

typeset -g agent_skills_codex_dir="${agent_skills_test_tmp}/new-codex-skills"
typeset -g agent_skills_test_codex_home="${agent_skills_test_tmp}/codex-home"
typeset -g agy_config_dir="${agent_skills_test_tmp}/missing-agy"
typeset -ga claude_code_profile_order=()
typeset -gA claude_code_profiles=()

function h-codex-session-home {
    print -r -- "${agent_skills_test_codex_home}"
}

command mkdir -p -- "${agent_skills_src_dir}" "${agent_skills_test_codex_home}"

typeset name
typeset linked_path expected_path
for name in clean extra wrong real absent dirlink ; do
    command mkdir -p -- "${agent_skills_src_dir}/${name}"
    command cp -- "${agent_skills_test_root}/configFiles/agent-skills/done/SKILL.md" \
        "${agent_skills_src_dir}/${name}/SKILL.md"
done
command mkdir -p -- "${agent_skills_src_dir}/clean/scripts"
command cp -- "${agent_skills_test_root}/zshlang/basic/basic.plugin.zsh" \
    "${agent_skills_src_dir}/clean/scripts/helper.zsh"

typeset dirs
dirs="$(h-agent-skills-dirs)"
[[ ${dirs} == $'codex\t'"${agent_skills_codex_dir}" ]] || \
    agent-skills-test-fail "Codex should use agent_skills_codex_dir"

command mv -- "${agent_skills_test_codex_home}" "${agent_skills_test_codex_home}.away"
[[ -z $(h-agent-skills-dirs) ]] || \
    agent-skills-test-fail "Codex should not be emitted when its session home is absent"
command mv -- "${agent_skills_test_codex_home}.away" "${agent_skills_test_codex_home}"

agent-skills-link
for name in clean extra wrong real absent dirlink ; do
    agent-skills-test-assert "${name} should be a whole-directory link" \
        test -L "${agent_skills_codex_dir}/${name}"
    linked_path="${agent_skills_codex_dir}/${name}"
    expected_path="${agent_skills_src_dir}/${name}"
    [[ "${linked_path:A}" == "${expected_path:A}" ]] || \
        agent-skills-test-fail "${name} should resolve to its tracked source directory"
done
agent-skills-test-assert "Codex directory links should expose sibling resources" \
    test -r "${agent_skills_codex_dir}/clean/scripts/helper.zsh"

# Re-running the linker must leave an exact link untouched.
typeset clean_link_before
clean_link_before="$(command readlink -- "${agent_skills_codex_dir}/clean")"
agent-skills-link
[[ $(command readlink -- "${agent_skills_codex_dir}/clean") == ${clean_link_before} ]] || \
    agent-skills-test-fail "an exact Codex link should be idempotent"

typeset doctor_output
doctor_output="$(h-agent-skills-doctor 2>&1)"
[[ ${doctor_output} == *'skill clean:'* && ${doctor_output} != *'WRONG TARGET'* && ${doctor_output} != *'UNTRACKED'* ]] || \
    agent-skills-test-fail "doctor should recognize whole-directory Codex links"

# A real target and a wrong symlink are user data: preserve both and fail.
typeset -g agent_skills_codex_dir="${agent_skills_test_tmp}/collision-real"
command mkdir -p -- "${agent_skills_codex_dir}/clean"
if agent-skills-link >/dev/null 2>&1 ; then
    agent-skills-test-fail "a real Codex target should make linking fail"
fi
agent-skills-test-assert "real Codex target should be preserved" test -d "${agent_skills_codex_dir}/clean"

typeset -g agent_skills_codex_dir="${agent_skills_test_tmp}/collision-link"
command mkdir -p -- "${agent_skills_codex_dir}"
command ln -s -- "${agent_skills_src_dir}/extra" "${agent_skills_codex_dir}/clean"
if agent-skills-link >/dev/null 2>&1 ; then
    agent-skills-test-fail "a wrong Codex symlink should make linking fail"
fi
linked_path="${agent_skills_codex_dir}/clean"
expected_path="${agent_skills_src_dir}/extra"
[[ "${linked_path:A}" == "${expected_path:A}" ]] || \
    agent-skills-test-fail "wrong Codex symlink should be preserved"

# Restore valid new links and populate the legacy ~/.codex/skills layout.
typeset -g agent_skills_codex_dir="${agent_skills_test_tmp}/new-codex-skills"
typeset legacy="${agent_skills_test_codex_home}/skills"
command mkdir -p -- "${legacy}/"{clean,extra,wrong,real,absent,.system,unknown} \
    "${agent_skills_test_tmp}/legacy-dirlink-target"
for name in clean extra absent ; do
    command ln -s -- "${agent_skills_src_dir}/${name}/SKILL.md" "${legacy}/${name}/SKILL.md"
done
command touch -- "${legacy}/extra/keep.txt" "${legacy}/.system/keep.txt" "${legacy}/unknown/keep.txt"
command ln -s -- "${agent_skills_test_root}/configFiles/agent-skills/done/SKILL.md" "${legacy}/wrong/SKILL.md"
command cp -- "${agent_skills_src_dir}/real/SKILL.md" "${legacy}/real/SKILL.md"
command ln -s -- "${agent_skills_test_tmp}/legacy-dirlink-target" "${legacy}/dirlink"
command rm -- "${agent_skills_codex_dir}/absent"

agent-skills-prune-legacy-codex

[[ ! -e ${legacy}/clean && ! -L ${legacy}/clean ]] || \
    agent-skills-test-fail "an empty legacy managed skill directory should be removed"
[[ -d ${legacy}/extra && ! -e ${legacy}/extra/SKILL.md && -e ${legacy}/extra/keep.txt ]] || \
    agent-skills-test-fail "only the managed link should be removed from a nonempty legacy directory"
agent-skills-test-assert "wrong legacy link should be preserved" test -L "${legacy}/wrong/SKILL.md"
agent-skills-test-assert "real legacy file should be preserved" test -f "${legacy}/real/SKILL.md"
agent-skills-test-assert "legacy directory symlink should be preserved" test -L "${legacy}/dirlink"
agent-skills-test-assert "legacy link without a verified new link should be preserved" test -L "${legacy}/absent/SKILL.md"
agent-skills-test-assert ".system should be preserved" test -e "${legacy}/.system/keep.txt"
agent-skills-test-assert "unknown skills should be preserved" test -e "${legacy}/unknown/keep.txt"

# A private checkout is optional; when present its skills reach both clients.
command mkdir -p -- "${agent_skills_notes_dir}/note" "${agent_skills_test_tmp}/claude"
command cp -- "${agent_skills_src_dir}/clean/SKILL.md" "${agent_skills_notes_dir}/note/SKILL.md"
claude_code_profile_order=(test)
claude_code_profiles[test]="${agent_skills_test_tmp}/claude"
agent-skills-link
linked_path="${agent_skills_codex_dir}/note"
expected_path="${agent_skills_notes_dir}/note"
[[ -L ${linked_path} && ${linked_path:A} == ${expected_path:A} ]] || \
    agent-skills-test-fail "Codex should link the private skill directory"
linked_path="${agent_skills_test_tmp}/claude/skills/note"
[[ -L ${linked_path} && ${linked_path:A} == ${expected_path:A} ]] || \
    agent-skills-test-fail "Claude should link the private skill directory"
agent-skills-test-assert "Claude directory links should expose sibling resources" \
    test -r "${agent_skills_test_tmp}/claude/skills/clean/scripts/helper.zsh"
expected_path="${agent_skills_notes_dir}/note/SKILL.md"
doctor_output="$(h-agent-skills-doctor 2>&1)"
[[ ${doctor_output} == *'skill note:'* && ${doctor_output} != *'WRONG TARGET'* ]] || \
    agent-skills-test-fail "doctor should resolve private sources"
command mkdir -p -- "${legacy}/note"
command ln -s -- "${expected_path}" "${legacy}/note/SKILL.md"
agent-skills-prune-legacy-codex
[[ ! -e ${legacy}/note ]] || agent-skills-test-fail "private legacy link should be pruned"

# The Claude seat refuses a target it does not own, exactly as Codex does above.
# Each case gets its own profile directory, so a refusal cannot be a leftover
# from the assertions before it.
typeset claude_case foreign
claude_case="${agent_skills_test_tmp}/claude-wrong-link"
command mkdir -p -- "${claude_case}/skills/clean"
command ln -s -- "${agent_skills_src_dir}/extra/SKILL.md" "${claude_case}/skills/clean/SKILL.md"
claude_code_profiles[test]="${claude_case}"
if agent-skills-link >/dev/null 2>&1 ; then
    agent-skills-test-fail "a wrong Claude symlink should make linking fail"
fi
linked_path="${claude_case}/skills/clean/SKILL.md"
expected_path="${agent_skills_src_dir}/extra/SKILL.md"
[[ -L ${linked_path} && ${linked_path:A} == ${expected_path:A} ]] || \
    agent-skills-test-fail "wrong Claude symlink should be preserved"

claude_case="${agent_skills_test_tmp}/claude-plain-file"
command mkdir -p -- "${claude_case}/skills/clean"
print -r -- 'mine' > "${claude_case}/skills/clean/SKILL.md"
claude_code_profiles[test]="${claude_case}"
if agent-skills-link >/dev/null 2>&1 ; then
    agent-skills-test-fail "a plain Claude file should make linking fail"
fi
[[ -f ${claude_case}/skills/clean/SKILL.md && ! -L ${claude_case}/skills/clean/SKILL.md ]] || \
    agent-skills-test-fail "plain Claude file should be preserved"
[[ "$(<${claude_case}/skills/clean/SKILL.md)" == mine ]] || \
    agent-skills-test-fail "plain Claude file should keep its contents"

# A skill directory link pointing somewhere else is somebody else's: it is
# preserved, and nothing is written through it.
claude_case="${agent_skills_test_tmp}/claude-dir-link"
foreign="${agent_skills_test_tmp}/claude-foreign"
command mkdir -p -- "${claude_case}/skills" "${foreign}"
command ln -s -- "${foreign}" "${claude_case}/skills/clean"
claude_code_profiles[test]="${claude_case}"
if agent-skills-link >/dev/null 2>&1 ; then
    agent-skills-test-fail "a symlinked Claude skill directory should make linking fail"
fi
agent-skills-test-assert "symlinked Claude skill directory should be preserved" \
    test -L "${claude_case}/skills/clean"
[[ ! -e ${foreign}/SKILL.md ]] || \
    agent-skills-test-fail "nothing should be written inside the foreign directory"

# A whole-directory link to the very skill we would link is another installer's
# layout for the same thing: accepted as it is, and reported as linked.
claude_case="${agent_skills_test_tmp}/claude-equivalent-dir-link"
command mkdir -p -- "${claude_case}/skills"
command ln -s -- "${agent_skills_src_dir}/clean" "${claude_case}/skills/clean"
claude_code_profiles[test]="${claude_case}"
agent-skills-link >/dev/null 2>&1 || \
    agent-skills-test-fail "an equivalent Claude directory link should be accepted"
linked_path="${claude_case}/skills/clean"
expected_path="${agent_skills_src_dir}/clean"
[[ -L ${linked_path} && ${linked_path:A} == ${expected_path:A} ]] || \
    agent-skills-test-fail "an equivalent Claude directory link should be left as it is"
doctor_output="$(h-agent-skills-doctor 2>&1)"
[[ ${doctor_output} == *'= symlinked'* && ${doctor_output} != *'UNTRACKED'* ]] || \
    agent-skills-test-fail "doctor should accept an equivalent Claude directory link"

# The file-link layout, a real directory holding only a SKILL.md link to the
# source, is replaced by a directory link. Anything else in that directory,
# hidden files included, keeps it from being touched.
claude_case="${agent_skills_test_tmp}/claude-file-link-layout"
command mkdir -p -- "${claude_case}/skills/"{clean,extra,wrong}
for name in clean extra wrong ; do
    command ln -s -- "${agent_skills_src_dir}/${name}/SKILL.md" "${claude_case}/skills/${name}/SKILL.md"
done
command touch -- "${claude_case}/skills/extra/keep.txt" "${claude_case}/skills/wrong/.keep"
claude_code_profiles[test]="${claude_case}"
doctor_output="$(h-agent-skills-doctor 2>&1)"
[[ ${doctor_output} == *'FILE LINK ONLY'* ]] || \
    agent-skills-test-fail "doctor should name the file-link layout"
if agent-skills-link >/dev/null 2>&1 ; then
    agent-skills-test-fail "a file-link directory with other contents should make linking fail"
fi
linked_path="${claude_case}/skills/clean"
expected_path="${agent_skills_src_dir}/clean"
[[ -L ${linked_path} && ${linked_path:A} == ${expected_path:A} ]] || \
    agent-skills-test-fail "a file-link-only directory should become a directory link"
agent-skills-test-assert "a migrated link should expose sibling resources" \
    test -r "${claude_case}/skills/clean/scripts/helper.zsh"
for name in extra wrong ; do
    [[ -d ${claude_case}/skills/${name} && ! -L ${claude_case}/skills/${name} && -L ${claude_case}/skills/${name}/SKILL.md ]] || \
        agent-skills-test-fail "a file-link directory with other contents (${name}) should be preserved"
done
agent-skills-test-assert "the extra file should be preserved" test -e "${claude_case}/skills/extra/keep.txt"
agent-skills-test-assert "the hidden file should be preserved" test -e "${claude_case}/skills/wrong/.keep"

# Dangling links, to the skill directory or to its file, are what a skill that
# moved between roots leaves behind; there is nothing behind them to lose.
claude_case="${agent_skills_test_tmp}/claude-dangling"
command mkdir -p -- "${claude_case}/skills/clean"
command ln -s -- "${agent_skills_test_tmp}/moved-away/clean/SKILL.md" "${claude_case}/skills/clean/SKILL.md"
command ln -s -- "${agent_skills_test_tmp}/moved-away/extra" "${claude_case}/skills/extra"
claude_code_profiles[test]="${claude_case}"
doctor_output="$(h-agent-skills-doctor 2>&1)"
[[ ${doctor_output} == *'DANGLING'* ]] || \
    agent-skills-test-fail "doctor should name a dangling Claude link"
agent-skills-link >/dev/null 2>&1 || \
    agent-skills-test-fail "dangling Claude links should be replaced, not refused"
for name in clean extra ; do
    linked_path="${claude_case}/skills/${name}"
    expected_path="${agent_skills_src_dir}/${name}"
    [[ -L ${linked_path} && ${linked_path:A} == ${expected_path:A} ]] || \
        agent-skills-test-fail "dangling Claude link for ${name} should be relinked to its source"
done

claude_code_profiles[test]="${agent_skills_test_tmp}/claude"

# The Codex seat replaces a dangling directory link the same way.
command rm -- "${agent_skills_codex_dir}/extra"
command ln -s -- "${agent_skills_test_tmp}/moved-away/extra" "${agent_skills_codex_dir}/extra"
agent-skills-link >/dev/null 2>&1 || \
    agent-skills-test-fail "a dangling Codex link should be replaced, not refused"
linked_path="${agent_skills_codex_dir}/extra"
expected_path="${agent_skills_src_dir}/extra"
[[ -L ${linked_path} && ${linked_path:A} == ${expected_path:A} ]] || \
    agent-skills-test-fail "dangling Codex link should be relinked to its source"

# Duplicate names must fail before any target mutation, rather than picking a root.
command mkdir -p -- "${agent_skills_notes_dir}/clean"
command cp -- "${agent_skills_src_dir}/clean/SKILL.md" "${agent_skills_notes_dir}/clean/SKILL.md"
agent_skills_codex_dir="${agent_skills_test_tmp}/duplicate-target"
if agent-skills-link >/dev/null 2>&1 ; then
    agent-skills-test-fail "duplicate names must fail"
fi
[[ ! -e ${agent_skills_codex_dir} ]] || agent-skills-test-fail "duplicates must not partially link"
if h-agent-skills-doctor >/dev/null 2>&1 ; then
    agent-skills-test-fail "doctor must report duplicate names"
fi
command rm -- "${agent_skills_notes_dir}/clean/SKILL.md"

typeset -g agent_skills_codex_dir="${legacy}"
if agent-skills-prune-legacy-codex >/dev/null 2>&1 ; then
    agent-skills-test-fail "cleanup should refuse coincident legacy and current roots"
fi

print -r -- 'ok: agent skills linker and legacy pruning'
