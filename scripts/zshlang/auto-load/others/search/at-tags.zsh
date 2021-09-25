##
function tag-filter-date-past-stdin {
    local tag="${1:-futureCron}"

    tag-filter-date.lispexe "$tag"
}

function tag-filter-date-past {
    local dirs=(${tag_filter_date_past_dirs[@]})
    assert-args dirs @RET
    local dir_main="${dirs[1]}"

    local tags=(${tag_filter_date_past_tags[@]})
    if (( ${#tags} == 0 )) ; then
        tags=('@futurecron')
    fi

    ensure-array tag_filter_date_past_opts
    local opts=("${tag_filter_date_past_opts[@]}")

    revaldbg @opts opts [ --no-binary --smart-case --engine auto --no-messages --with-filename --line-number "$opts[@]" "$dirs[@]" ] @ rg-literal-or "$tags[@]" | tag-filter-date-past-stdin | prefix-rm "$dir_main" | {
        if isOutTty ; then
            # @todo1 make the results TTY links to their locations, and color them

            double-newlines
        else
            cat
        fi
    }
}
##
function tag-filter-date-past-fz {
    @opts engine [ h-tag-filter-date-fz-nts-engine "$@" ]  @ ntsearch-lines
}
@opts-setprefix tag-filter-date-past-fz tag-filter-date-past

function h-tag-filter-date-fz-nts-engine {
    local dirs=(${tag_filter_date_past_dirs[@]})
    assert-args dirs @RET
    local dir_main="${dirs[1]}"

    tag-filter-date-past "$@" | @opts dir_main "$dir_main" @ h-ntsearch-fz
}
@opts-setprefix h-tag-filter-date-fz-nts-engine tag-filter-date-past
##
function nt-todos-past {
    local engine=("${nt_todos_past_engine[@]:-tag-filter-date-past}")

    @opts dirs [ "$nightNotes" ~tmp/ ] @ "$engine[@]" "$@"
}

function nt-todos-past-fz {
    @opts engine tag-filter-date-past-fz @ nt-todos-past
}
# @opts-setprefix nt-todos-past-fz nt-todos-past
##
