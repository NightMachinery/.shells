##
function str2orgtitle {
    in-or-args "$@" | gtr '[]' '{}' | cat-copy-if-tty
}
aliasfn org-escape-title str2orgtitle

function org-escape-link {
    in-or-args "$@" | perl -pe 's/(\[|\])/\\${1}/g' | cat-copy-if-tty
}

function org-escape-block {
    in-or-args "$@" | sd '^(\s*(?:\x1b\[33m)?)(\*|#)' '$1,$2' | cat-copy-if-tty
}

function org-link-create {
    local url="$1" title="$2"
    assert-args url @RET

    if test -z "$title" ; then
        ec "[[$(org-escape-link "$url")]]"
    else
        ec "[[$(org-escape-link "$url")][$(org-escape-title "$title")]]"
    fi
}
##
function org-watch {
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

function org-update-files {
    local files=($@)
    if (( ${#files} == 0 )) ; then
        ecgray "$0: called with zero inputs."
    fi

    local cmd
    cmd="(night/update-files $(emc-quote "$files[@]"))"

    time2 revaldbg emc-eval "$cmd"
}

function fd-org {
    fd --absolute-path --extension org --extension org_archive --type f "$@"
}

function org-update-files-all {
    ##
    # fd --extension org --type f . "$nightNotes" | emc_eval_in=y emc-eval '(apply night/update-files lines)'
    ##
    local places
    places=(
        "$nightNotes"
        ~tmp/
        ##
        # ~cod/ #: adds about 15s
        ~cod/uni
        ~cod/python
        ##
    )
    {
        local d
        for d in ${places[@]} ; do
            fd-org . "$d"
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

    } | org-escape-block | command sd --flags=i --string-mode "$q" "$(ecn "$q" | mimic -m 100)"
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
        else
            updated_year="$(now-year)"
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
function org-link-extract-title {
    rget '(?<!\\)\[(?<!\\)\[(?:[^][]|\\\[|\\\])+(?<!\\)\](?<!\\)\[((?:[^][]|\\\[|\\\])+)(?<!\\)\](?<!\\)\]'
}

function org-link-extract {
    ensure-array org_link_extract_opts
    local opts=("$org_link_extract_opts[@]")
    local link_type="$org_link_extract_type"
    if test -n "$link_type" && [[ "$link_type" != *: ]] ; then
        link_type+=":"
    fi

    local what="${org_link_extract_what:-link}" rget_e=('ugrep_get')
    local format
    if true ; then
        rget_e='ugrep_get'
        if [[ "$what" == 'link' ]] ; then
            format='%[2]#%~'
        elif [[ "$what" == 'heading' ]] ; then
            format='%[1]#%~'
        else
            format="${what}"
        fi
    else
        rget_e='rget'
        if [[ "$what" == 'link' ]] ; then
            format='$2'
        elif [[ "$what" == 'heading' ]] ; then
            format='$1'
        else
            @NA
        fi
    fi

    cat-paste-if-tty |
        rget_replace="$format" ugrep_get_format="$format" revaldbg "$rget_e[@]" "${opts[@]}" \
            -e '(?:\s*(?:\*|-|\+)\s*)?(.*(?<!\\)(?:\[|<)('${link_type}'(?:[^][]|\\\[|\\\])+)(?<!\\)(?:\]|>).*)' |
        cat-copy-if-tty
}

function org-link-extract-url {
    local link_type="$org_link_extract_type"

    #: We can also change the regex used in org-link-extract to directly capture the link URL, but this is more backwards compatible.
    org-link-extract "$@" |
        prefixer --remove-prefix="${link_type}:" |
        cat-copy-if-tty
}
@opts-setprefixas org-link-extract-url org-link-extract
##
aliasfn org-link-extract-http org_link_extract_type='https?' org-link-extract
aliasfn org-link-extract-file org_link_extract_type=file org-link-extract-url
aliasfn org-link-extract-id org_link_extract_type=id org-link-extract-url


function org-export-recursive {
    #: stdout: the paths of exported files
    #: The caching system is only sensitive to the roots. If a leave changes, we won't detect the change.
    #:
    #: @warning This function exports links that are in =:noexport:= subtrees!
    ##
    local storage_key_root='org_export_rec'
    local storage_key_hashes="${storage_key_root}_hashes"
    local storage_key_outs="${storage_key_root}_outs"

    if isDeus ; then
        #: @warning This removes the cache for all files, not just the ones affected by the current job. We can make `storage_key_hashes' dependent on our inputs here, but I think the complexity is not worth it.
        assert sout redism del "$storage_key_hashes" @RET
    fi

    h-org-export-recursive "$@"
}

function org-export-raw {
    local ret=0 inargs=()
    in-or-args3 "$@" @RET

    local f f_q res
    for f in ${inargs[@]} ; do
        f_q="$(emc-quote "$f")" @TRET

        if res="$(emc-eval "(night/org-export-file-to-html ${f_q})")" ; then
            if [[ "${res}" == 'nil' ]] ; then
                ecerr "$0: returned nil: ${f}"
                ret=1
            else
                ec "${res}"

                git-rm-smart "${res}"
                night_git_ignore_file="${res:h}/.gitignore" assert git-ignore "${res:t}" @RET
            fi
        else
            ret=$?
        fi
    done

    return $ret
}

function html-favicon-add {
    local favicon="${1}" html_input="${2}"
    local html_output="${3:-${html_input}}"
    assert-args favicon html_input html_output @RET

    local root_dir="${html_output:h}"
    (
        cdm "${root_dir}" @TRET
        reval-ecgray html_favicon.py "${favicon}" --input="${html_input}" --output="${html_output}" @TRET
    ) @RET
}

function h-org-export-recursive {
    #: This function cannot be directly called. Use `org-export-recursive'.
    assert test -n "$storage_key_root" @RET
    local root_dir="${org_export_root_dir:-.}"
    local root_dir="${root_dir:a}" #: =:a= normalizes the path, returns empty string for empty input
    dact var-show root_dir

    local fs=($@)

    local f f_hashed id_link file_link file_links text h outs exported_file
    for f in ${fs[@]} ; do
        f="$(realpath "$f")" @TRET

        file_links=()

        f_hashed="$(md5m "$f")" @TRET
        h="$(md5-file "$f")" @TRET
        h_last="$(redism hget "${storage_key_hashes}" "${f_hashed}")" || true
        if [[ "$h" == "$h_last" ]] ; then
            ecgray "skipped $(gquote-dq $f)"

            assert redism hget "${storage_key_outs}" "${f_hashed}" @RET
            #: This will not return an error even if the key does not exist.

            continue
        fi

        ecbold "exporting ${f}"
        exported_file="$(org-export-raw "$f")" @TRET
        dact var-show exported_file
        assert test -e "$exported_file" @RET

        outs="${exported_file}"$'\n'

        cat "$exported_file" |
            org-html-postprocess |
            sponge "$exported_file" @RET

        local favicon
        for favicon in "${exported_file:h}/favicon."{svg,png} ; do
            if test -e "${favicon}" ; then
                html-favicon-add "${favicon}" "${exported_file}" @RET

                break
            fi
        done


        assert sout redism hset "${storage_key_hashes}" "${f_hashed}" "${h}" @RET
        assert sout redism hset "${storage_key_outs}" "${f_hashed}" "" @RET
        #: Deleting/resetting this key will potentially make parallel executions of this function problematic. But not deleting it can leak deleted links, which is more dangerous.

        assert sout redism hset "${storage_key_hashes}" "${f_hashed}" "${h}" @RET

        text="$(cat "$f")" @TRET

        id_links=( ${(@f)"$(ec "$text" | { org-link-extract-id || true } )"}) @TRET
        for id_link in ${id_links[@]} ; do
            file_link="$(emc-eval "(night/org-id-path-get $(emc-quote "$id_link"))")" @TRET

            if [[ "$file_link" == nil ]] ; then
                ecerr "$0: ID not found: ${id_link}"
                return 1
            fi

            file_link="${file_link:a}"

            if test -n "${root_dir}" && [[ ! "${file_link}" == "${root_dir}"* ]] ; then
                ecgray "$0: skipped file out of root dir: ${file_link}"
                continue
            fi

            outs+="$("$0" "$file_link")"$'\n' @TRET
        done

        file_links+=( ${(@f)"$(ec "$text" | { org-link-extract-file || true } | { rg '\.org(?:::.*)?$' || true } )"}) @TRET
        for file_link in ${file_links[@]} ; do
            outs+="$("$0" "$file_link")"$'\n' @TRET
        done

        outs+="$(org-img-used "$f")" @TRET
        outs="$(ec "$outs" | duplicates-clean)"$'\n' @TRET
        assert sout redism hset "${storage_key_outs}" "${f_hashed}" "${outs}" @RET
        ec "$outs"
    done
}
##
function org-id-line-get-all {
    local text
    text="$(in-or-args "$@")" @RET

    id_links=( ${(@f)"$(ec "$text" | { org-link-extract-id || true } )"}) @TRET
    for id_link in ${id_links[@]} ; do
        line="$(emc-eval "(night/org-id-line-get $(emc-quote "$id_link"))")" @TRET

        if [[ "$line" == nil ]] ; then
            ecerr "$0: got a nil line for id: ${id_link}"
            return 1
        fi

        ec "$line"
    done
}

function org-id2ss {
    local text
    text="$(in-or-args "$@" | rg -v '^\s*url=\{')" @RET
    #: '  url={' lines indicate the URL in bibtex.

    local semanticscholar_url_regex
    semanticscholar_url_regex='^https?://(?:[^/]+\.)*semanticscholar\.org/'

    local line m
    {
        for line in "${(@f)text}" ; do
            #: @assumes each line contains either an SS URL or an ID link, not both
            if m="$(ec "$line" | urls-extract | rg "${semanticscholar_url_regex}")" ; then
                ec "$m"
            else
                ec "$line" |
                    org-id-line-get-all |
                    org-link-extract-http |
                    { rg "${semanticscholar_url_regex}" || true }
            fi
        done
    } |
        duplicates-clean |
        cat-copy-if-tty
}

function id2ss-csv {
    bella_zsh_disable1

    withemcgui org-id2ss "$@" |
        inargsf semantic-scholar-get --adder FM --format csv --flat |
        pbcopy

    bello
}
##
function org-html-postprocess {
    #: [[file:~/code/hugo/notes-hugo/themes/cortex/assets/js/page.js::function highlighterReplacer(text) {][js/page.js::function highlighterReplacer(text)]]
    #: @idempotent
    ##
    #: * @tests
    #: ** `ec '<h3 id="@org6d5682c"><span class="s@ection-number-3">@1.3.</span>@hi @seeAlso</h3>'|org-html-postprocess|org-html-postprocess`
    #:
    #: * @performance
    #: `hfd "cat '${nightNotesPublic}/subjects/math/AI/ML/NLP/attention, transformers/papers/GlobEnc/gen.html' | org_html_postprocess.pl" "cat '${nightNotesPublic}/subjects/math/AI/ML/NLP/attention, transformers/papers/GlobEnc/gen.html' | org_html_postprocess.py"`
    #:
    #: The perl version is 1.2 times faster (680ms vs 736ms).
    ##
    cat-paste-if-tty |
        org_html_postprocess.py |
        html_copy_button.py |
        cat-copy-if-tty
    ##
    #: @works @deprecated

    # cat-paste-if-tty |
    #     perl -CS "${commands[org_html_postprocess.pl]}" |
    #     cat-copy-if-tty
    ##
    #: @works @deprecated

    #: perl v5.34: Lookbehind longer than 255 not implemented
    # cat-paste-if-tty |
    #     perl -CS -Mexperimental=vlb -ple \
        #         's/(?<=\s|(*nlb:class="(?:[^"]{0,75}\s)?todo(?:\s[^"]{0,75})?"[^>]{0,75})>)(@(?:\w|\/|\d|[][(),.;'\''])+)(?=\s|<)/<span class="todo at_tag">$1<\/span>/g' |
    #     cat-copy-if-tty
    ##
}

##
aliasfn org-link-extract-audiofile org_link_extract_type=audiofile org-link-extract-url

function org-link-extract-audiofile-abs {
    local fs
    fs="$(org-link-extract-audiofile)" @TRET
    fs="$(ec "$fs" | inargsf path-unabbrev)" @TRET

    ec "$fs"
}

function org-audiofile-play {
    local fs
    fs="$(org-link-extract-audiofile-abs)" @TRET

    ec "$fs" | inargsf rgeval hear
}
##
aliasfn org-link-extract-jalali org_link_extract_type=jalali org-link-extract
aliasfn org-date-extract org-link-extract-jalali

function org-date-extract-first {
    org-date-extract "$@" | ghead -n 1
}
##
function org-log-date-get {
    : "currently, returns the first heading of the supplied orgmode files. We can do this with regexes better, so using this function is pretty useless now."

    local input_files
    input_files="$(in-or-args "$@")" @TRET
    input_files=(${(@f)input_files})
    assert-args input_files @RET
    local garden="${org_log_date_get_garden:-y}"
    local already_loaded="${org_log_date_get_al:-n}"
    local script
    script=~[org-log-date-get]/src/NightMachinary/org_log_date_get.clj @TRET

    # @todo avoid reloading the code, it can reduce the time to ~0.9s, which is the best we can get with the Clojure kernel
    local code=''
    if ! bool "$already_loaded" ; then
        code+="$(ec ; cat "$script")" @TRET
    else
        ecgray "$0: skipping (re)loading"
    fi

    code+=$'\n'"(do (org-log-date-get $(lisp-quote "$input_files[@]")))"

    if [[ "$garden" == y ]] ; then
        @opts kernel_name 'conda-clojupyter' session 's1' @ jg_eval.sh "$code" @TRET
    else
        # ecbold "$code"

        ot-rep "$code" @TRET
    fi | {
        if [[ "$garden" == y ]] ; then
            jqm .out
        else
            rep-clean
        fi
    }
    ## tests
    # `redo2 10 ec "${nightNotesPrivate}/wallet/houses/sabzevar/apartment/elevator/bills/electricity/1472555905222/log.org" | time2 org-log-date-get`
    ##
}

function rep-clean {
    rg -v "^(?:#?'|nil$)"
    # removes:
    #   `#'user/needs-reload`
    #   `nil`
}
##
function cutestarsabove {
    ensure-array cutestarsabove_po
    local query=("$@") \
        p_opts=("${cutestarsabove_po[@]}") \
        n="${cutestarsabove_n:-4000}"
    fd_query="${cutestarsabove_fq:-.}"

    {
        reval-ec fd-org "${fd_query}" "$PWD" | reval-ec parad -N "$n" --pipe "$p_opts[@]" cutestarsabove.pl "$query[@]"
        # add --dry-run to see how many jobs it runs
    } always {
        bell-lm-diary-search-fx
    }

    ## @examples
    # `time2 @opts fq 'recommendation' @ cutestarsabove 'time.?travel' > ../search_results/timetravel_rec..org-highlighter..org`
    ##
}

function emc-pager-highlighter {
    # @todo1 rewrite this using [agfi:emc-less], with a general tag-adder
    ##
    local tmp
    tmp="$(gmktemp --suffix="${ntag_sep}org-highlighter${ntag_sep}.org")" @TRET

    cat > "$tmp" @RET

    emc-open "$tmp"
}
aliasfn emc-less-highlighter emc-pager-highlighter
aliasfn cutestarsabove-open-pipe emc-pager-highlighter

function cutestarsabove-open {
    cutestarsabove "$@" | cutestarsabove-open-pipe
}
@opts-setprefix cutestarsabove-open cutestarsabove
alias seec='cutestarsabove-open'
alias 'seec-pc'='cutestarsabove_print_children=y cutestarsabove-open'
##
function org-date-extract-due {
    local files
    files=( "$@" )
    assert-args files @RET

    local what="${org_date_extract_due_what}"
    if test -z "$what" ; then
        what='%f%s%n%~%t%[1]#%~'
        ##
        # what='org-highlighter'
    fi
    if [[ "$what" == 'org-highlighter' ]] ; then
        what='* [[highlight:,default,file_point:%f::%b][%f]]%~** %[1]#%~'
        # [[https://github.com/Genivia/ugrep/issues/159][Genivia/ugrep#159 {FR} Make `%b` and `%e` accept `{NUM}` arguments for captur...]]
    fi

    # @todo1 [[https://github.com/Genivia/ugrep/issues/157][Genivia/ugrep#157 How to add ANSI escape codes to `--format`?]]

    local main_dir="${org_date_extract_due_d}"
    ensure-array org_date_extract_due_query
    local query=("${org_date_extract_due_query[@]}")

    files="$(arrNN "$files[@]" | ugbool "$query[@]")" @TRET
    files=("${(@f)files}")
    ##
    local opts=(--line-number --with-filename --max-count=1)
    if isColorTty ; then
        opts+=(--color=always)
    fi

    if true ; then
        {
            org_link_extract_opts=("$opts[@]" "$files[@]") \
                org_link_extract_what="$what" \
                org-date-extract \
                } || true
    else
        local f date
        for f in $files[@] ; do
            {
                org_link_extract_opts=("$opts[@]" "$f") \
                    org_link_extract_what="$what" \
                    org-date-extract \
                    } || true
            ##
            # date="$(cat "$f" | org-date-extract-first)" || continue

            # local f_a="$f"
            # f_a="$(ec $f_a | dir-rmprefix "$main_dir")"
            # # f_a="$(path-abbrev "$f")" @TRET # @slow (doubles execution time)
            # ec "$(resetcolor)${f_a}:"$'\n\t'"$(colorbg 255 255 255)$(colorfg 0 0 0)$(Bold)${date}$(resetcolor)"
            ##
        done
    fi
    ##
}
##
function org-link-browser-current {
    local url title
    url="$(browser-current-url)" @TRET
    title="$(browser-current-title)" @TRET
    local raw_p="${org_link_browser_raw_p:-n}"
    local success_p=''

    {
        if ! bool "$raw_p" ; then
            if [[ "${url}" =~ '^https://scholar\.google\.[^/]*/citations\?user' ]] ; then
                scholar-org-link-current
                success_p=y

            elif [[ "${url}" =~ '^https://scholar\.google\.[^/]*/citations\?view' ]] ; then
                local title_scholar citations link
                if title_scholar="$(scholar-title-current)" ; then
                    link="$(org-link-create "${url}" "${title_scholar}")" @TRET

                    if citations="$(scholar-paper-citations-current)" ; then
                        ec "@citations/${citations} ${link}"
                    else
                        ec "${link}"
                    fi
                    success_p=y
                fi
            fi
        fi

        if ! bool "$success_p" ; then
            org-link-create "${url}" "${title}"
        fi
    } | cat-copy-if-tty
}
alias lbc='org-link-browser-current'

function md-link-browser-current {
    org-link-browser-current "$@" |
        org2md
}
##
function org-toman-get {
    : 'Use with "| in-sum | numfmt-humanfriendly"'

    ##
    # rget '\[toman:(\d+)\]'
    ##
    org_link_extract_type='toman' org-link-extract-url "$@"
    ##
}
##
function h-org-cp-file {
    local prefix_arg="${1}" files=(${@[2,-2]}) dest="${@[-1]}"
    assert-args files dest

    assert ensure-dir "$dest" @RET

    if bool "$prefix_arg" ; then
        assert gmv --backup='numbered' --verbose "${files[@]}" "${dest}" @RET

        bell-files-moved
    else
        assert gcp --archive --backup='numbered' --link --verbose "${files[@]}" "${dest}" @RET
    fi

    bell-hp3-platform-movement

    assert h-org-mark-redundant "${files[@]}" @RET
}

function h-org-mark-redundant {
    in-or-args "$@" |
        path-abbrev |
        sync-append-in "${nightNotes}/private/internal/redundant.txt"
}
##
function org-header-indent-v0 {
    local n="${1:-1}"

    local i
    for i in {1..$n} ; do
        perl -pe 's/^(\*+\s+)/*$1/' @RET
    done
}

function org-header-indent {
    local lv="${1:-1}"

    local -x prefix
    prefix="$(str_repeat_sep='' str-repeat "$lv" "*")" @TRET
    perl -lpe 's/^(?=\*+\s+)/$ENV{prefix}/g'
}

function org-header-indent-to-current {
    #: * @seeAlso
    #:  ** [help:night/org-insert-and-fix-levels]
    #:  *** [agfi:h-org-insert-and-fix-levels]
    #:
    #:  ** [help:org-yank-adjusted-subtrees]
    ##
    local lv
    lv="$(org-header-current-level)" @TRET
    h-org-insert-and-fix-levels "$lv"
}

function h-org-insert-and-fix-levels {
    local text lv="$1"
    assert-args lv @RET
    text="$(cat)" @RET

    text="$(ec "$text" | org-header-rm-shared-level)" @TRET

    ec "$text" |
        if (( $lv > 0 )) ; then
            org-header-indent "$lv"
        else
            cat
        fi |
        cat-copy-if-tty
}

function org-header-rm-shared-level {
    local text
    # Read input from stdin or arguments, handle potential errors with @TRET
    text="$(in-or-args "$@")" @RET

    # Use Perl to find the minimum level and strip the shared prefix from headers
    # -l: auto-chomp input lines and add newline to print
    # -e: execute the following script
    # The script:
    # 1. Reads all lines into @lines.
    # 2. Iterates through @lines to find the minimum asterisk level ($min_level)
    #    for lines matching the header pattern / ^(\*+)\s /x.
    # 3. If a minimum level is found ($min_level is defined and > 0):
    #    Iterates through @lines again.
    #    For lines matching the header pattern, it removes exactly $min_level
    #    asterisks from the beginning using substitution s/^\*{$min_level}//.
    #    Prints each (potentially modified) line.
    # 4. If no headers were found, it prints the original lines unchanged.
    local modified_text
    modified_text=$(ec "$text" | perl -e '
        my @lines = <>; # Slurp all lines into an array
        my $min_level = undef;

        # Pass 1: Find minimum header level
        for my $line (@lines) {
            if ($line =~ /^(\*+)\s/) {
                my $level = length($1);
                if (!defined $min_level || $level < $min_level) {
                    $min_level = $level;
                }
            }
        }

        # Pass 2: Remove minimum prefix from headers and print all lines
        if (defined $min_level && $min_level > 1) {
            $min_level -= 1; # Adjust to remove one less than the minimum found

            # uncomment for debugging:
            # warn "Minimum Level Found: $min_level";
            for my $line (@lines) {
                # Only modify lines that are actual headers
                if ($line =~ /^(\*+)\s/) {
                    # Remove exactly min_level asterisks from the start
                    $line =~ s/^\*{$min_level}//;
                }
                print $line;
            }
        } else {
            # No headers found, or min_level was somehow invalid, print original lines
            print @lines;
        }
    ')
    # Check Perl exit status if needed, though command substitution captures output
    # local perl_exit_status=$?
    # if (( perl_exit_status != 0 )); then ... handle error ...; fi

    # Output the modified text, using cat-copy-if-tty
    ec "$modified_text" | cat-copy-if-tty
}

function org-header-rm-shared-level-v1 {
    #: This version has this bug:
    #: Instead of using prefixer to strip the star levels, use perl. Note that ***sth is not a header, and should not be part of the strip. Headers are \*+\s.
    ##
    local text
    text="$(in-or-args "$@")" @TRET

    local min_level
    if min_level="$(ec "$text" | perl -lne 'm/^(\*+)\s/ && print length $1' | num-min)" ; then
        dact var-show min_level

        min_level=$(( min_level - 1 ))

        local min_heading=''
        while (( $min_level > 0 )) ; do
            min_level=$(( min_level - 1 ))

            min_heading+='*'
        done

        if test -n "$min_heading" ; then
            text="$(ec "$text" | prefixer --remove-prefix="$min_heading")" @TRET
        fi
    fi

    ec "$text" | cat-copy-if-tty
}
##
function url2org {
    : "@seeAlso readmoz-org"

    url2note "$1" org
}
renog url2org
@opts-setprefix url2org url2note

function url2org-emc {
    url2note_emacs=y cmd=(url2org) no_newline_before_first_child=y h-reval-to-org-headings "$@"
}

function fanficfare2org-emc {
    cmd=(fanficfare2org) no_newline_before_first_child=n h-reval-to-org-headings "$@"
}

function semantic-scholar-to-org-emc {
    cmd=(semantic-scholar-to-org) no_newline_before_first_child=n h-reval-to-org-headings "$@"
}

function h-reval-to-org-headings {
    local cmd=("${cmd[@]}") second_p="${no_newline_before_first_child:-y}"
    local inargs
    in-or-args3 "$@" @RET

    local arg ret=0 first_p=y
    for arg in ${inargs[@]} ; do
        if ! bool "$first_p" ; then
            if bool "$second_p"; then
                second_p=''
            else
                ec
            fi
            ecn "* "
        fi

        reval "${cmd[@]}" "$arg" || ret=$?

        first_p=''
    done

    return $ret
}
##
function org-img-unused-trs-i {
    in-or-args "$@" |
        inargsf org-img-unused |
        fz | inargsf trs
}

function org-img-used {
    local fs=($@)

    local f files used text
    for f in ${fs[@]} ; do
        text="$(cat "$f")"

        if used="$(ec "${text}" |
                     org-link-extract-file |
                     rg "\.(${(j.|.)image_formats})\$")" ; then
            used=(${(@f)"$(ec "${used}" |
                     { cd "${f:h}" && inargsf re 'grealpath --canonicalize-missing --' } )"}) @TRET
        else
            used=()
        fi

        arrnn "${used[@]}"
    done
}

function org-img-unused {
    #: * @seeAlso
    #: ** [agfi:org-img-unused-trs-i]
    #: ** [help:night/org-img-unused-trs]
    ##
    local fs=($@)

    local f files used
    for f in ${fs[@]} ; do
        files=({"${f}_imgs","${f:h}/.ob-jupyter"}/*(D.N))
        files=(${(@f)"$(arrnn "${files[@]}" |
                          inargsf re 'grealpath --')"}) @TRET

        used=(${(@f)"$(org-img-used "$f")"}) @RET

        arrnn ${(@)files:|used}
    done
}
##
function strip-prefixed-hash-comment {
    in-or-args "$@" |
        perl -CS -lpe 's/^(\s*)#:? ?/$1/g' |
        #: ` +` is for unified diff.
        cat-copy-if-tty
}

function strip-prefixed-plus-minus {
    in-or-args "$@" |
        perl -CS -lpe 's/^(?:\+|-)//g' |
        #: `+`, `-` is for unified diff.
        cat-copy-if-tty
}

function strip-prefixed-colons {
    in-or-args "$@" |
        perl -CS -lpe 's/^:(?: \+?|$)//g' |
        #: ` +` is for unified diff.
        cat-copy-if-tty
}

function strip-prefixed-ge {
    in-or-args "$@" |
        perl -CS -lpe 's/^>+//g' |
        cat-copy-if-tty
}

function strip-prefixed-dollar {
    in-or-args "$@" |
        perl -CS -lpe 's/^\s*\$ //g' |
        cat-copy-if-tty
}

function strip-prefixed-ic {
    in-or-args "$@" |
        perl -CS -lpe 's/^ic\| //g' |
        cat-copy-if-tty
}

function strip-ic-lines {
    in-or-args "$@" |
        perl -CS -lne 'm/^ic\|\s/ || print' |
        cat-copy-if-tty
}
##
function strip-wikipedia-citations {
    in-or-args "$@" |
        perl -CS -lpe 's/\[\d+\]//g' |
        cat-copy-if-tty
}
##
function py-escape-triple-quotes {
    #: @seeAlso [help:night/py-escape-triple-quotes-elisp]
    #:
    #: @tests
    #: `ec "hi''''" | py-escape-triple-quotes`
    ##
    cat-paste-if-tty |
        # perl -lpe 's/(*nlb:\\)'"((?:\"|'){3})"'/\\$1/g' |
        perl -lpe 's/(*nlb:\\)'"(*pla:(?:\"|'){3})"'/\\/g' |
        cat-copy-if-tty
}

function py-unescape-triple-quotes {
    #: @seeAlso [help:night/py-escape-triple-quotes-elisp]
    #:
    #: @tests
    #: `ec "hi''''" | py-escape-triple-quotes`
    ##
    cat-paste-if-tty |
        perl -lpe 's/(*nlb:\\)\\'"(*pla:(?:\"|'){3})"'/ /g' |
        cat-copy-if-tty
}

function org2md-escape-triple-quotes {
    org2md "$@" |
        py-escape-triple-quotes |
        cat-copy-if-tty
}
##
function org-jupyter-with {
    local session="${emc_jupyter_with_session}"
    local kernel="${emc_jupyter_with_kernel}"

    local f="$1"
    local input
    if test -z "$f" ; then
        input="$(pbpaste)" @TRET
    else
        input="$(cat "$f")" @TRET
    fi

    {
        ec "${input}"
    } | session="$session" kernel="$kernel" perl -CS -0777 -pe '
BEGIN { use utf8; use open qw/:std :utf8/; } ;

        if ($ENV{session}) {
#: Only do the initial echo if the file does not start with a properties block with the key `jupyter-python-session` already. (The property drawer might have other keys though and the order of keys does not matter.)
            if (/^\s*:PROPERTIES:\s*\n((?:.*\n)*?):END:/m) {
                my $props = $1;
                if ($props !~ /^\s*:jupyter-python-session:/m) {
                    s/(:PROPERTIES:\s*\n)/$1:jupyter-python-session: $ENV{session}\n/;
                }
            } else {
                $_ = ":PROPERTIES:\n:jupyter-python-session: $ENV{session}\n:END:\n" . $_;
            }

            s{:jupyter-python-session:\s+\S+}{:jupyter-python-session: $ENV{session}}g;
            s{:session\s+\S+}{:session $ENV{session}}g;
        }

        s{:kernel\s+\S+}{:kernel $ENV{kernel}}g if $ENV{kernel};
    ' |
        cat-copy-if-tty
}
@opts-setprefix org-jupyter-with emc-jupyter-with

function org-jupyter-with-inplace {
    local f="$1"
    assert-args f @RET

    local res
    res="$(org-jupyter-with "$f")" @TRET
    assert cp "$f" ~/tmp/backup/ @RET
    ec "${res}" > "$f"
}
@opts-setprefix org-jupyter-with-inplace emc-jupyter-with

function emc-jupyter-with {
    local f="$1"
    local engine=("${emc_jupyter_with_engine[@]:-emc-open-no-server-tui}")
    assert-args f @RET

    cdtmp_engine=pushf cdtmp
    {
        local f_copied
        f_copied="./${f:t:r}..no_recent..${f:e}"
        assert reval-not test -e "${f_copied}" @RET

        org-jupyter-with "$f" > "${f_copied}" @RET

        reval "${engine}" "${f_copied}"
    } always {
        # popf
    }
}
##
function org-remove-inline-images {
    in-or-args "$@" |
        perl -ple 's/\[\[data:image.*?\]\]//g' |
        # perl -nle 'print unless m/^\s*\Q[[data:image\E/' |
        cat-copy-if-tty
}
##
function org-cache-latex-rm {
    trs ~/.emacs.d/.local/cache/org/latex/
}
##
