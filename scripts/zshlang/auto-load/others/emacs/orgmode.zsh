##
function org-watch() {
    local fd_in file

    fswatch --exclude='.' --include='\.org$' "$nightNotes" | {
        # protect our stdin:
        exec {fd_in}<&0
        exec </dev/null
        while read -d $'\n' -r file <&${fd_in} ; do
            ##
            # this won't work till the upstream bug of hardlink detection gets fixed:
            # dbg org-update-files "$file"
            ##
            org-update-files-all
        done
    } always {
        exec {fd_in}<&-
    }
}

function org-update-files() {
    local cmd
    cmd="(night/update-files $(emc-quote "$@"))"

    time2 revaldbg emc-eval "$cmd"
}
function org-update-files-all() {
    ##
    # fd --extension org --type f . "$nightNotes" | emc_eval_in=y emc-eval '(apply night/update-files lines)'
    ##
    {
        local d
        for d in "$nightNotes" ~tmp/ ; do
            fd --extension org --extension org_archive --type f . "$d"
        done
     } | inargsf org-update-files
    # emc-eval "(org-id-locations-save)" # @idk if this is needed, I think it's done automatically
    ##
    # org-update-files "$nightNotes"/**/*.org
    ##
}
##
function ntt-org() {
    q="${*}"

    local opts=()
    opts+=( "--file-extension=${(j.,.)note_formats%%,}")

    {
        revaldbg ugbase --group-separator=$'\n|-------------------------------------------|\n' --sort --context=3 --regexp="$q" --recursive --heading --color=always "$opts[@]" "$nightNotes" | dir-rmprefix "$nightNotes"

        # --line-number -T
        #  | sdlit $'\t' '   '

    } | command sd '^(\s*(?:\x1b\[33m)?)(\*|#)' '$1,$2' | command sd --flags=i --string-mode "$q" "$(ecn "$q" | mimic -m 100)"
    # -m CHANCE, --me-harder=CHANCE
    #   forward replacement percent, default 1
    #
    # without making the query text 'invisible' via 'mimic', the reevaluation of the org-babel source blocks will re-find the query in its own results, causing a loop
    #
    # @warn you still need to use quoting shenanigans when you call ntt-org itself so as to not find yourself, but it will not cause a loop at least
}
function ntt-org1() {
    # DEPRECATED
    q="${*}" metadata="${ntt_org_md:-y}"
    {
        if bool $metadata ; then
            local out
            silent ntt "$q"
            ecn ${(@F)out} | double-newlines
        else
            ntt "$q"
        fi
    } | sd --flags=i "$q" "$(ecn "$q" | mimic -m 100)"
}
##
function h_org_unt {
    local i
    for i in $@ ; do
        reval-ec unt "$i" # errors are implicitly ignored
        ec $'\n'
    done
}

function links2org-dir {
    # @examples
    # `links2org-dir ./posts/ ./analysis/links-by-frequency.txt`
    ##
    local dir="${1}" o="${2}"

    assert-args dir o @RET

    assert ensure-dir "$o" @RET

    rget "$nightUrlRegex" "$dir" | unalix | prefixer --skip-empty | by-freq --reverse > "$o" @TRET

    cat "$o" | awkn 2 | ghead -n 3000 | parallel_halt='soon,fail=99%' parallel_jobs=100 para -k h_org_unt ::: > "${o:r}.org" || ectrace "$0: failed"

    notif "finished org-links-by-freq $(retcode 2>&1)"
}
##
function pathtree2org {
    local dir="$1"
    assert-args dir @RET ; shift
    assert test -d "$dir" @RET

    local err_skip
    if isI; then
        err_skip=(cat)
    else
        err_skip=("rg -v '^INFO: ' | sd '\n\n+' '\n\n'")
    fi

    (
        mark-me "$0" # useful for forcefully interrupting this

        pathtree2org.lisp "$dir" '(?i)/\.((trash(es)?|(_\.)?DS_STORE|mypy_cache)/?$|git|Spotlight-|trash-)' '/System Volume Information/' '/(\$RECYCLE.BIN|_gsdata_|found\.\d+)/' '\.(sparsebundle|photoslibrary|app)/.+' "$@" 2> >(eval "$err_skip[@]" >&2)
    )
}
##
function org-rm-header-content {
    # @alt/elisp select the region and use `keep-lines` with the pattern "^\*"
    ##
    perl -ne '/^(?!\*+).*\S+.*/ || print'
}

aliasfn p-org-rm-header-content pbpaste-transform org-rm-header-content
##
function org-header-indent {
    local n="${1:-1}"

    local i
    for i in {1..$n} ; do
        perl -pe 's/^(\*+\s+)/*$1/' @RET
    done
}

function org-header-indent-to-current {
    local lv
    lv="$(org-header-current-level)" @TRET
    cat | org-header-indent "$lv"
}

##
function org-header-current-level {
    emc-eval "(org-current-level)"
}
##
function org-manning-toc() {
    local res
    res="$(html2org | org-rm-header-content | perl -pe 's/(\[\d+)/${1}. /' | org-header-indent)" @RET

    res="* TOC${org_props_folded}${res:l}"

    ec $res
}

function p-org-manning-toc() {
    local res
    res="$(pbpaste | org-manning-toc)" @TRET

    res="$(ec $res | org-header-indent-to-current)" @TRET

    ec $res | pbcopy-ask
}
##
function p-org-fanfic {
    local d
    d="$(html2org)" @TRET

    local url
    url="$(browser-current-url)" @TRET

    ec $d | org-fanfic "$url" | cat-copy-if-tty
}

function org-fanfic {
    local d url="$1"
    assert-args url @RET
    d="$(cat)" @RET

    local title author_line author body tags=''
    title="$(ec $d | ghead -1 | org2plain | str2orgtitle)" @TRET
    author_line="$(ec $d | perl -ne "$. == 3 && print && exit")" @TRET
    author_line="$(ec $author_line | sd '\s*\S+$' '')" @TRET
    author="$(ec $author_line | rget '(\[.*\])')" @TRET
    body="$(ec $d | perl -ne "$. > 3 && print")" @TRET

    local complete_p
    if ec $body | rg -q -- '-\s*(Status:\s*)?Complete' ; then
        complete_p=y
    fi

    local words words_slashed
    if words="$(ec $body | rget 'words:\s*(\S+)' | gtr -d ',')" ; then
        words="$(ec $words | numfmt-humanfriendly)" @TRET
        words_slashed="/${words:l}"
    fi

    local updated updated_year
    if updated="$(ec $body | rget 'updated:\s*([^-]+)\s')" ; then
        if updated_year="$(ec $updated | rget '(\d{4})')" ; then
            # tags+="@updated/${updated_year} "
        fi
    fi

    local published published_year
    if published="$(ec $body | rget 'published:\s*([^-]+)\s')" ; then
        if published_year="$(ec $published | rget '(\d{4})')" ; then
            # tags+="@published/${published_year} "
        fi
    fi

    if test -n "$published_year" ; then
        local date_tag
        date_tag="@${published_year}"
        if test -n "$updated_year" && [[ "$published_year" != "$updated_year" ]] ; then
            date_tag+="-${updated_year}"
        fi
        tags+="${date_tag} "
    fi

    local completion_status
    if bool $complete_p ; then
        completion_status='complete'
    else
        if test -z "$updated_year" || (( ($(now-year) - updated_year) >= 1 )) ; then
            completion_status="abandoned"
        else
            completion_status="WIP"
        fi
    fi
    if test -n "$completion_status" ; then
        tags="@${completion_status}${words_slashed} ${tags}"
    fi

    ec "${tags}[[${url}][${title}]]${org_props_folded}- @author ${author}"$'\n'"#+begin_quote${body}"$'\n'"#+end_quote"
}
##
