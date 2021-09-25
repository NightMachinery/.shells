##
function tag-filter-date-past-stdin {
    local tag="${1:-futureCron}"

    tag-filter-date.lispexe "$tag"
}

function tag-filter-date-past {
    ensure-array tag_filter_date_past_opts
    local dirs=(${tag_filter_date_past_dirs[@]}) tags=(${tag_filter_date_past_tags[@]}) opts=("${tag_filter_date_past_opts[@]}")
    assert-args dirs @RET
    if (( ${#tags} == 0 )) ; then
        tags=('@futurecron')
    fi

    revaldbg @opts opts [ --no-binary --smart-case --engine auto --no-messages --with-filename --line-number "$opts[@]" "$dirs[@]" ] @ rg-literal-or "$tags[@]" | tag-filter-date-past-stdin | prefix-rm "${dirs[1]}" | {
        if isOutTty ; then
            # @todo1 make the results TTY links to their locations, and color them

            double-newlines
        else
            cat
        fi
    }
}

function tag-filter-date-past-fz {
    @opts engine [ h-tag-filter-date-fz-nts-engine "$@" ]  @ ntsearch-lines
}
@opts-setprefix tag-filter-date-past-fz tag-filter-date-past

function h-tag-filter-date-fz-nts-engine {
    local
    out=(${(@f)"$(tag-filter-date-past "$@")"}) @TRET
}
@opts-setprefix h-tag-filter-date-fz-nts-engine tag-filter-date-past

function nt-todos-past {
    @opts dirs [ "$nightNotes" ~tmp/ ] @ tag-filter-date-past "$@"
}

function rg-literal-or {
    ensure-array rg_literal_or_opts
    local patterns=("$@") opts=("${rg_literal_or_opts[@]}")
    local engine=("${rg_literal_or[@]:-rg}")
    # ugrep also supported, slower though. (Especially as rg seems to go instant with cache, while ugrep doesn't benefit as much.)
    #   You might want to use '--dereference-recursive' with ugrep.

    local opts_pats=() i
    for i in "$patterns[@]" ; do
        opts_pats+=(-e "$i")
    done
    revaldbg "$engine[@]" --fixed-strings "$opts_pats[@]" "$opts[@]"
}
##
