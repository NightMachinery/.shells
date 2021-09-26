##
function tag-filter-date-past-stdin {
    local tags=(${tag_filter_date_past_tags[@]})
    assert-args tags @RET

    revaldbg tag-filter-date.lispexe "$tags[@]"
}
@opts-setprefix tag-filter-date-past-stdin tag-filter-date-past

function tag-filter-date-past {
    local dirs=(${tag_filter_date_past_dirs[@]})
    assert-args dirs @RET
    local dir_main="${dirs[1]}"

    local tags=(${tag_filter_date_past_tags[@]})
    if (( ${#tags} == 0 )) ; then
        tags=('futurecron' 'tofuture' 'garbageCollectMe' 'GCMe' 'toGarbageCollect' 'toGC') # case-insensitive
    fi

    local i tags_with_prefix=()
    for i in ${tags[@]} ; do
        tags_with_prefix+="@${i}"
    done

    ensure-array tag_filter_date_past_opts
    local opts=("${tag_filter_date_past_opts[@]}")

    revaldbg @opts opts [ --no-binary --smart-case --engine auto --no-messages --with-filename --line-number "$opts[@]" "$dirs[@]" ] @ rg-literal-or "${tags_with_prefix[@]}" | {
        @opts tags [ "$tags[@]" ] @ tag-filter-date-past-stdin
    } | prefix-rm "$dir_main" | {
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
    tag_filter_date_past_fz_query=("$@") @opts engine [ h-tag-filter-date-fz-nts-engine ]  @ ntsearch-lines
}
@opts-setprefix tag-filter-date-past-fz tag-filter-date-past

function h-tag-filter-date-fz-nts-engine {
    local fz_query
    fz_query="$(fzp_ug="${fzp_ug:-ni}" fz-createquery "${tag_filter_date_past_fz_query[@]}")"
    local dirs=(${tag_filter_date_past_dirs[@]})
    assert-args dirs @RET
    local dir_main="${dirs[1]}"

    tag-filter-date-past "$@" | @opts dir_main "$dir_main" query "$fz_query" @ h-ntsearch-fz
    # @maybe add ugbool_query between these two
}
@opts-setprefix h-tag-filter-date-fz-nts-engine tag-filter-date-past
##
function nt-todos-past {
    local engine=("${nt_todos_past_engine[@]:-tag-filter-date-past}")

    @opts dirs [ "$nightNotes" ~tmp/ ] @ "$engine[@]" "$@"
}

function nt-todos-past-fz {
    @opts engine tag-filter-date-past-fz @ nt-todos-past "$@"
}
# @opts-setprefix nt-todos-past-fz nt-todos-past
##
