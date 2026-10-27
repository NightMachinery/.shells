alias tsmf='tsendf ${me_tlg}'
###
function tlg-dest-assert {
    local dest="${1}"
    local label="${2:-dest}"
    local caller="${funcstack[2]:-$0}"

    if isSpace "$dest" ; then
        ecerr "$caller: ${label} is empty/whitespace; aborting."
        return 1
    fi
}
###
function tsend-retry {
    retry tsend "$@"
}
###
function tsend-live {
    : "usage: [tsend_live_alert_p=y] tsend-live NAME DEST MARKDOWN
Keeps one live rich message per NAME in DEST, for a status that changes: edits it in place, which does not notify. With tsend_live_alert_p=y, or when the edit fails, sends a fresh message, which notifies, and removes the old one."
    local alert_p="${tsend_live_alert_p:-n}"
    local name="${1}" dest="${2}" markdown="${3}"
    assert-args name dest markdown @RET
    if ! [[ "${name}" =~ '^[[:alnum:]._-]+$' ]] ; then
        ecerr "$0: NAME names a state file, so only letters, digits, '.', '_' and '-': ${name}"
        return 1
    fi
    local state_dir="${XDG_STATE_HOME:-${HOME}/.local/state}/tsend-live"
    local state_file="${state_dir}/${name}.tsv"

    local old_dest='' old_id=''
    if test -s "${state_file}" ; then
        IFS=$'\t' read -r old_dest old_id < "${state_file}"
    fi
    if [[ "${old_dest}" != "${dest}" ]] ; then
        old_id=''
    fi

    if test -n "${old_id}" && ! bool "${alert_p}" ; then
        if tsend edit --parse-mode=rich -- "${dest}" "${old_id}" "${markdown}" ; then
            return 0
        fi
        #: Someone deleted it, or it cannot be edited: a fresh message follows,
        #: and the old one is cleared away below, if it still exists.
    fi

    local out new_id
    local -a ids
    out="$(tsend --print-ids --parse-mode=rich -- "${dest}" "${markdown}")" @TRET
    #: Into an array first: a nested expansion with one element is a string,
    #: and [1] would then take its first character.
    ids=( "${(@M)${(@f)out}:#<->}" )
    new_id="${ids[1]}"
    if test -z "${new_id}" ; then
        ecerr "$0: tsend printed no message ID; the next update will be a new message"
        return 1
    fi
    assert mkdir -p "${state_dir}" @RET
    ec "${dest}"$'\t'"${new_id}" > "${state_file}" @RET

    if test -n "${old_id}" ; then
        #: Bots cannot delete messages older than 48 hours; such a message is
        #: shrunk to a pointer instead, so only one full copy remains.
        tsend delete -- "${dest}" "${old_id}" ||
            tsend edit --parse-mode=rich -- "${dest}" "${old_id}" "_Replaced by a newer message, below._" ||
            true
    fi
}
###
function tnotif() {
    # @alt notif
    ##
    ensure-array tnotif_opts
    local msg="$*" opts=("${tnotif_opts[@]}")

    # matrix-send-self "$msg"
    tsend --parse-mode markdown "$opts[@]" -- "$tlg_notifs" "$msg"
}

function tnotif-casual() {
    # CASUAL Mode
    ##
    tlg_notifs="$tlg_notifc" tnotif "$@"
}
@opts-setprefix tnotif-casual tnotif
aliasfn tnotifc tnotif-casual
@opts-setprefix tnotifc tnotif
##
function tsendf-book {
    local dest="${tlg_dest:-$tlg_ch_books}"
    tlg-dest-assert "$dest" @RET
    local inargs
    in-or-args3 "$@" @RET

    local tmp
    for f in ${inargs[@]} ; do
        tmp="$(gmktemp --suffix='.png')"
        ebook-cover "$f" "$tmp" @RET

        reval-ec tsendf-discrete "$dest" "$f" "$tmp"
    done
}
@opts-setprefix tsendf-book tsend
##
function tsendf {
    local dest="${1}"
    tlg-dest-assert "$dest" @RET
    local fs=("${@:2}")
    local album_p="${tsend_album_p:-y}"

    local opts=()
    if ! bool "$album_p" ; then
       opts+="--no-album"
    fi

    local f
    for f in ${fs[@]}
    do
        opts+=(--file "$f")
    done

    revaldbg tsend "$opts[@]" -- "$dest" ''
}

function tsendf-discrete {
    tsend_album_p=n tsendf "$@"
    # local f
    # for f in "${@:2}"
    # do
    #     tsend -f "$f" -- "$1" ''
    # done
}

function tsend-url {
    local dest="${1}" url="${2:?}" msg="$3"
    tlg-dest-assert "$dest" @RET

    local tmp=~/tmp/"$(uuidm)"
    pushf $tmp || return $?
    {
        aa-insecure "$url" || return $?
        local f fs=(*(DN))
        for f in ${fs[@]} ; do
            if test -z "$msg" ; then
               msg="${url}"
            elif [[ "$msg" == 'MAGIC_FILE_TAIL' ]] ; then
               msg="${f:t}"
            fi

            tsend --file "$f" -- "$dest" "${msg}"
        done
    } always { popf ; command rm -rf "$tmp" }
}

function tsend-urls {
    local dest="${1}" urls=("${@[2,-1]}")
    tlg-dest-assert "$dest" @RET
    assert-args urls @RET

    local url
    for url in ${urls[@]} ; do
        reval-ec tsend-url "$dest" "$url" 'MAGIC_FILE_TAIL'
    done
}
##
function air {
    local i
    for i in $@ ; do
        reval-ec tsendf "$i" "$(hear-get)"
    done
}

function hear-copy {
    reval-ecgray pbadd "$(hear-get)"
}
alias air-copy="hear-copy"

function hear-cd {
    cdd "$(hear-get)"
}
alias air-cd='hear-cd'
##
typeset -ga tsend_main_variants=(
    tsend
    tsend-retry
    tsendf
    tsendf-discrete
    tsendf-book
    tsend-url
    tsend-urls
    air
)

function tlg-main-run {
    #: Runs its arguments as the main account instead of the bot.
    #: [agfi:tsend] reads `TELEGRAM_SESSION`, and the export reaches nested
    #: callers too, so wrapping [agfi:tsendf] or [agfi:air] needs nothing more.
    #:
    #: `telegram_session_main` is host-specific and set privately, so we abort
    #: instead of silently falling back to the bot session.
    ##
    local session="${telegram_session_main}"

    if isSpace "$session" ; then
        ecerr "${funcstack[2]:-$0}: telegram_session_main is unset; aborting."
        return 1
    fi

    TELEGRAM_SESSION="$session" "$@"
}

() {
    #: Defines `tsend-main`, `tsendf-main`, `air-main`, ... .
    ##
    local fn
    for fn in ${tsend_main_variants[@]} ; do
        aliasfn "${fn}-main" tlg-main-run "$fn"
        @opts-setprefixas "${fn}-main" "$fn"
    done
}
##
function reval-tlg {
    local rec="${reval_tlg_receiver:-${reval_tlg_r:-$me}}"
    local out="$(eval "$(gquote "$@")" 2>&1)"

    ec "$out"
    tsend -- "$rec" "$out"
}
# aliasfn reval-tlg enve

function tlg-file-captioned {
    local files=("$@") file
    local rec="$me"

    for file in $files[@] ; do
        file="$(realpath-relchild $PWD "$file")"
        tsend --file "$file" -- "$rec" "$file"
        ec "Sent $file to $rec"
    done
}
###
# function tlg-clean-copied {
#     #: @see [agfi:tlg-strip-metadata]
#     ##
#     ecgray "$0: deprecated; use 'tlg-strip-metadata'"

#     h_tlg-clean-copied "$(h_tlg-clean-copied "$*")" # telegram sometimes uses two of these tags
# }

# function h_tlg-clean-copied {
#     local text="$*"
#     if [[ "$text" =~ '\[[^]]*\]\s*((.|\n)*)' ]] ; then
#        text="$match[1]"
#     fi
#     print -nr -- "$text"
# }
# function tlg-clean-paste {
#     tlg-clean-copied "$(pbpaste)"
# }
##
function tlg-strip-metadata {
    in-or-args "$@" |
        perl -CSD -pe 's/^\[\d{1,2}\/\d{1,2}\/\d{4}\s+\d{1,2}:\d{2}\]\s+[^:\r\n]+:\s*//;' |
        cat-copy-rtl-if-tty
}
aliasfn tlg-clean-copied tlg-strip-metadata
###
function podcast2tel() {
    local dest="${podcast2tel_dest:-${me_tlg}}"
    tlg-dest-assert "$dest" @RET
    local l="$1"
    local title="$rssTitle" # from rss-tsend

    # tsend --file "$l" -- "$dest" "$title" # sometimes did not work
    tsend-url "$dest" "$l" "$title"
}
##
function md2tlg {
    ensure-array tsend_opts
    local tsend_opts=(
        "${tsend_opts[@]}"
        --parse-mode=md
    )

    local inargs
    in-or-args2 "$@" @RET

    revaldbg tsend "${tsend_opts[@]}" -- "${me_tlg}" "${inargs[*]}"
}

function md2tlg2 {
    : "usage: md2tlg2 [<markdown>...]
Like md2tlg, but as a rich message: Telegram itself renders the tables, headings, task lists and math."

    ensure-array tsend_opts
    local tsend_opts=(
        "${tsend_opts[@]}"
        --parse-mode=rich
    )

    local inargs
    in-or-args2 "$@" @RET

    revaldbg tsend "${tsend_opts[@]}" -- "${me_tlg}" "${inargs[*]}"
}

function org2tlg {
    local dest="${1:-${me_tlg}}"
    tlg-dest-assert "$dest" @RET
    local text
    text="$(cat-paste-if-tty)" @TRET
    text="$(ec "$text" | org-header-rm-shared-level)" @TRET

    if false ; then
       ensure-array pandoc_opts
       local pandoc_opts=("${pandoc_opts[@]}" --markdown-headings=setext)
       #: =setext= means headers are marked with `====`s and `----`s.
       #: It doesn't look pretty in Telegram, as the underline wraps and becomes multiple lines.
    fi

    local text_md
    text_md="$(ec "$text" | org2md | sd '\\'"('|\"|#|\|)" '$1')" @TRET

    tsend-retry --parse-mode=md -- "${dest}" "$text_md" @RET
}

function org2tlg2 {
    : "usage: org2tlg2 [<dest>]  (reads org from stdin, or the clipboard)
Like org2tlg, but as a rich message: Telegram itself renders the tables, headings, task lists and math."

    ensure-array tsend_opts
    local opts=("${tsend_opts[@]}")
    local dest="${1:-${me_tlg}}"
    tlg-dest-assert "$dest" @RET
    local text
    text="$(cat-paste-if-tty)" @TRET
    text="$(ec "$text" | org-header-rm-shared-level)" @TRET

    #: GitHub-flavored Markdown, which is what Telegram's rich parser reads:
    #: pipe tables instead of the space-aligned tables [agfi:org2md] writes, and
    #: `$...$` math instead of GitHub's dollar-backtick form (`-tex_math_gfm`).
    #: The spelling with an extension also keeps [agfi:pandoc-convert] from
    #: adding md_code_blocks.lua, which would drop the language of code blocks.
    #: The perl pass drops the `example` language pandoc gives example blocks.
    #:
    #: Unlike org2tlg, nothing is unescaped afterwards. Its `sd` pass exists only
    #: because Telethon's Markdown parser shows backslash escapes verbatim; this
    #: parser consumes them, and `\|` inside a table cell is load-bearing.
    local text_md
    text_md="$(ec "$text" |
        @opts from org to gfm-tex_math_gfm @ pandoc-convert |
        perl -lpe 's/^(```+)\s*example$/$1/')" @TRET

    #: Plain tsend rather than [agfi:tsend-retry]: tsend already retries
    #: transient failures, and what is left (no rich support, text over the
    #: limit) is permanent, so retrying forever would hang.
    revaldbg tsend "${opts[@]}" --parse-mode=rich -- "${dest}" "$text_md"
}

function org2tlg-with-props {
    cat-paste-if-tty \
        | sd --flags i '^:PROPERTIES:$' '' \
        | sd --flags i '^:END:$' '' \
        | sd --flags i '^#\+begin_quote$' '' \
        | sd --flags i '^#\+end_quote$' '' \
        | sd --flags i '^:visibility: folded$' '' \
        | sd -- '^:' '- :' \
        | org2tlg "$@"
    #   | double-newlines \ # inserted paragraphs between the properties which was not nice
}
##
