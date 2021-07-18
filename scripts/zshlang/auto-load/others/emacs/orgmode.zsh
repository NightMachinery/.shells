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
    fd --extension org --extension org_archive --type f . "$nightNotes" | inargsf org-update-files
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
