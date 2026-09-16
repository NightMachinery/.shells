#!/usr/bin/env zsh
# Regression test for the bash-compatibility contract in zshlang. Run with: zsh -f this-file

setopt errexit nounset pipefail

typeset -gr bash_compatible_test_root="${0:A:h:h:h}"
typeset -gr bash_compatible_scan_dir="${bash_compatible_test_root}/zshlang"
#: The marker is kept in a variable, and never written at the start of a line
#: in this file, so that the scan below does not match its own source.
typeset -gr bash_compatible_marker='^### BASH COMPATIBLE'

if ! command -v bash > /dev/null 2>&1 ; then
    print -r -- 'skip: no bash on this host, cannot check the bash-compatibility contract'
    exit 0
fi

#: Subjects are discovered, not listed: any file that declares the marker in
#: its header is enrolled automatically, and a hardcoded list would go stale.
typeset -a bash_compatible_subjects
bash_compatible_subjects=( ${(f)"$(command grep -rlI -- "${bash_compatible_marker}" "${bash_compatible_scan_dir}" 2>/dev/null | command sort || true)"} )
bash_compatible_subjects=( ${bash_compatible_subjects:#} )

if (( ${#bash_compatible_subjects} == 0 )) ; then
    #: Without this the test would pass vacuously after a typo in the marker or
    #: a move of the tree, which is exactly the failure it exists to prevent.
    print -ru2 -- "FAIL: no file under ${bash_compatible_scan_dir} declares the bash-compatibility marker; the scan found nothing to check"
    exit 1
fi

typeset -i bash_compatible_failures=0
typeset bash_compatible_subject bash_compatible_rel bash_compatible_output
typeset -i bash_compatible_rc

for bash_compatible_subject in "${bash_compatible_subjects[@]}" ; do
    bash_compatible_rc=0
    bash_compatible_output="$(command bash -n "${bash_compatible_subject}" 2>&1)" || bash_compatible_rc=$?

    bash_compatible_rel="${bash_compatible_subject#${bash_compatible_test_root}/}"

    #: Some bash builds report a `[[ ... ]]' parse error on stderr and still
    #: exit 0, so any output at all counts as a failure, not just the status.
    if (( bash_compatible_rc != 0 )) || [[ -n "${bash_compatible_output}" ]] ; then
        print -ru2 -- "FAIL: ${bash_compatible_rel} does not parse under bash (bash -n exited ${bash_compatible_rc})"
        if [[ -n "${bash_compatible_output}" ]] ; then
            print -ru2 -- "${bash_compatible_output}"
        fi
        bash_compatible_failures=$(( bash_compatible_failures + 1 ))
    fi
done

if (( bash_compatible_failures )) ; then
    print -ru2 -- "FAIL: ${bash_compatible_failures} of ${#bash_compatible_subjects} bash-compatible file(s) do not parse under bash"
    exit 1
fi

print -r -- "ok: ${#bash_compatible_subjects} bash-compatible zshlang file(s) parse under bash"
