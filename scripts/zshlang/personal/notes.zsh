##
typeset -g nightNotesDue=()
function nt-due-init {
    # @warn This function's sourcecode is public
    ##
    if isDeus || bool "$1" || (( ${#nightNotesDue} == 0 )) ; then
        typeset -g nightNotesDueDir=(~nt/private/wallet)
        typeset -ag nightNotesDue=(
            "$nightNotesDueDir"/**/*bills/**/(log|gen).org
            #: Use =finished log.org=, =log disabled.org= to exclude from here.
        )
    fi
}

function nt-due {
    nt-due-init

    local query=("$@")

    @opts d "$nightNotes" query [ "$query[@]" ] @ org-date-extract-due "$nightNotesDue[@]"
}

function h-nt-due-oh {
    org_date_extract_due_what='org-highlighter' nt-due "$@"
}

function org-sort-date {
    #: @C++ =org_sort_date=
    #: @Rust =org_sort_date.rs=
    org_sort_date.rs "$@"
}

function nt-due-oh {
    nt-due-init # can't do it in the pipeline, as forks can't change our env

    h-nt-due-oh "$@" |
        org-sort-date |
        prefix="$nightNotesDueDir" perl -pe 's|(\[)\Q$ENV{prefix}\E/*([^]]+\]\])|${1}${2}|g' |
        if-out-tty emc-pager-highlighter
}
alias bills-due='nt-due-oh'

function nt-due-oh-ugrep {
    nt-due-oh |
    org_grouper --out-replace-nulls-with='' -- ugrep --null-data "$@"  |
    if-out-tty emc-pager-highlighter
}

function org-grep {
    cat-paste-if-tty |
        org_grouper --out-replace-nulls-with='' -- ugrep --null-data "$@" |
        cat-copy-if-tty
}

function org-strip-no-export {
    local levels=({1..20})
    #: assuming no more levels

    local text
    text="$(cat-paste-if-tty "$@")" @RET

    local level
    for level in ${levels[@]} ; do
        text="$(ec "$text" | org_grouper --group-headings-at="$level" --out-replace-nulls-with='' -- perl -0lne 'print unless /^[^\n]*:noexport:/')"
        #: [[id:44ca24ea-a7ff-41fe-880c-e3aa965d8d04][regex tests]]
    done

    ec "$text" |
        cat-copy-if-tty

}

function nt-due-sees {
    nt-due-init # can't do it in the pipeline, as forks can't change our env

    # local nt_due_sees_fz_query=("$@")
    ## @duplicateCode/e8a79c74c906643ade4a17454e668666
    local dir_main="${nightNotesDueDir}"
    local nightNotes="${dir_main}"
    assert-args dir_main @RET
    ##

    @opts engine [ h-nt-due-nts-engine "$@" ]  @ ntsearch-lines
}

function h-nt-due-nts-engine {
    local query=("$@") fz_query
    fz_query="$(fzp_ug="${fzp_ug:-ni}" fz-createquery "${nt_due_sees_fz_query[@]}")" @TRET

    ## @duplicateCode/e8a79c74c906643ade4a17454e668666
    local dir_main="${nightNotesDueDir}"
    local nightNotes="${dir_main}"
    assert-args dir_main @RET
    ##

    org_date_extract_due_what='%f%s%n%s%[1]#%~' \
        nt-due "$query[@]" \
        | prefix-rm "${dir_main}/" \
        | @opts \
        rtl y \
        dir_main "$dir_main" \
        opts [ --no-sort ] \
        query "$fz_query" \
        @ h-grep-output-to-fz
}
##
