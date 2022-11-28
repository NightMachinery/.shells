export CLIPBOARD_RECORD_FILE=~/tmp/.clipboard
##
function clipboard-record() {
    ##
    # @codetoread https://github.com/ms-jpq/isomorphic_copy
    # might have a better way than polling
    ##
    local sleep=0.25

    ecdate "$0: Started; file=$(gq $CLIPBOARD_RECORD_FILE)"
    clipboard-removedups

    local old paste i counter=0
    while true ; do
        assert pbpaste-plus
        if test -n "$paste[*]" && [[ "$paste[*]" != "$old" ]] ; then
            for i in $paste[@] ; do
                ecdate "Recorded: "
                ec "$i" | prefixer --skip-empty -a $'\t\t\t\t\t| '
                ec
                clipboard-add "$i"
            done
            old="$paste[*]"
        fi
        if (( counter == 7200 )) ; then
            counter=0

            ecdate "Maintenance: Deleting duplicates ..."

            clipboard-removedups
        else
            counter=$(( counter + 1 ))
        fi
        sleep $sleep
    done
}

function clipboard-add() {
    local i="$*" file="$CLIPBOARD_RECORD_FILE"
    assert-args file @RET
    if test -z "$i" ; then
        return 0
    fi


    if ! test -e "$file" ; then
        touch "$file"
        clipboard-init
    fi

    sync-append "$file" $'\0'"$i" @TRET
}

function clipboard-delall {
    local file="$CLIPBOARD_RECORD_FILE"
    assert-args file @RET

    reval-ec command rm -rf "$file"
    # reval-ec clipboard-init
}

function clipboard-init {
    local file="$CLIPBOARD_RECORD_FILE"
    touch "$file" # ensures that we won't get into a loop

    local i

    if test -e "$attic_emails"; then
        for i in "${(@ps.\C-^.)$(cat "$attic_emails")}" ; do
            reval-ec clipboard-add "$(trim "$i")"
        done
    fi

    if test -e "$attic_temoji"; then
        for i in "${(@ps.\C-^.)$(cat "$attic_temoji")}" ; do
            i=( "${(@f)i}" )
            clipboard-add "${i[1]}"$'\C-^\t\t'"${i[2]}"
        done
    fi

    while true ; do
        local emoji_json
        # emoji_json="$(ffz-get unicode-emoji-json)/data-by-emoji.json" @TRET
        emoji_json=~cod/misc/unicode-emoji-json/data-by-emoji.json
        if  test -e "$emoji_json" ; then
            # https://github.com/muan/unicode-emoji-json
            # https://gitlab.com/NightMachinary/unicode-emoji-json
            if test -e "$emoji_json" ; then
                clipboard-add "$(cat "$emoji_json" | jq -r --nul-output --arg sep $'\C-^\t\t' 'keys[] as $k | "\($k)\($sep)\(.[$k] | .name + " (" + .group +" EMOJIS)" )"')"
            else
                ectrace "emoji_trace does not exist" "$emoji_json"
            fi

            break
        elif isMe ; then
            pushf ~cod/misc @RET
            {
                assert git clone git@gitlab.com:NightMachinary/unicode-emoji-json.git @RET
                assert z-add "$(realpath ./unicode-emoji-json)" @RET
            } always { popf }
        else
            break
        fi
        done
}

function clipboard-removedups {
    local file="$CLIPBOARD_RECORD_FILE"
    assert-args file @RET

    # `gtac --separator='\0'` was buggy :|
    local tmp="$(gmktemp)"
    cat "$file" | sponge | duplicates-clean-nul > "$tmp" || {
        ectrace
        return $?
    }
    assert flock "$file" mv "$tmp" "$file" @RET # flock is necessary or we can lose the file with parallel 'clipboard-add's
    ecgray "$0: completed"
}
##
function clipboard-fz-raw() {
    local copy="${clipboard_fz_copy}"
    local opts=("${@:-}")
    if [[ "$opts[-1]" == -* ]] ; then
        #: `fzp' needs the last arg to be a query.
        opts+=''
    fi

    local res
    res="$(<$CLIPBOARD_RECORD_FILE fzp --read0 --tac --exact --tiebreak=index --height='80%' "${opts[@]}")" @RET

    if test -z "$copy" ; then
        ecn "$res" | cat-copy-if-tty
    else
        if bool $copy ; then
            assert pbcopy "$res"
        fi

        ecn "$res"
    fi
}
alias cl='clipboard-fz-raw --height="100%"'

function clipboard-fz() {
    unset out # @global

    bella_zsh_disable1
    out=(${(@0)"$(clipboard_fz_copy=no clipboard-fz-raw --print0 "${@:-}")"}) @RET # do NOT preserve empty elements; there are stuff with line endings in a single element in some cases

    local i tmp
    for i in {1..${#out}} ; do
        tmp=( ${(@ps.\C-^\t\t.)out[$i]} )
        if (( $#tmp == 2 )) ; then
            out[$i]="$tmp[1]"
        fi
    done
}

function clipboard-fz-widget {
    local res i tmp

    zle-print-dots
    {
        clipboard-fz
        local res=("${out[@]}") ; unset out

        if (( ${#res} >= 2 )) ; then
            res="$(gq "$res[@]")"
        else
            res=${res[*]}
        fi
        LBUFFER="${LBUFFER}${res}"
        assert pbcopy "$res"
    } always {
        zle redisplay
    }
}
##
function iloop-clipboard() {
    @opts e clipboard-fz en y t Clipper @ iloop "$@"
}

function activate-iloop2-clipboard() {
    kitty-send $'\C-c\n'"iloop2-clipboard"
    input-lang-push en
}

function iloop2-clipboard() {
    sout clipboard-fz --height='100%' "${@:-}" @RET
    out="${(j..)out}"
    if test -n "$out" ; then
        silent pbcopy "$out" || {
            ectrace "pbcopy failed" # do NOT print out to the terminal, it can break it
            continue
        }
    fi
    hs-hyper-m
    input-lang-pop
    if test -n "$out" ; then
        hs-cmd-v
    fi
}
##
function iloop-chis() {
    : "@alt omnibar (o, O) of vimium does pretty much the same thing"
    : "@design we can also use polling and record Chrome's history ourselves ala clipboard-record."

    @opts e [ sout @opts rg '' @ chis ] t CHIS @ iloop "$@"
}
function iloop() {
    local engine=("${iloop_e[@]}") title="${iloop_t:-iloop}" prompt1="${iloop_p1:-ready: }" en_only="${iloop_en}"
    assert-args engine @RET

    tty-title "$title"
    bella_zsh_disable1

    local q sleep
    local rn=1
    # if bool $en_only ; then
    #     rn=3
    # fi
    {
        while true ; do
            if [[ "$prompt1" == "MAGIC_SKIP" ]] ; then
                q=''
                sleep=y
            else
                ecn $'\n'"$prompt1"
                read -k "$rn" q # you can press any whitespace to just trigger fzf
                # reading more allows for changing the letters into en
                sleep 0.2
                q="${q}$(pipe-get-nb < /dev/tty)"
                if bool $en_only ; then
                    # (input-lang-push en)
                    q="$(ecn "$q" | per2en)"
                fi
                sleep=''
            fi
            if [[ "${q[1]}" == ('^'|'\') ]] ; then
                # See kitty-esc
                read -e q # empty the buffer
                continue
                ##
                q=''
            fi
            reval "$engine[@]" "${@}" "$(trim "$q")" || {
            # clipboard-fz --bind "change:reload:$(gq cat "$CLIPBOARD_RECORD_FILE")" "${@:-}" || {
                # reload resets selections https://github.com/junegunn/fzf/issues/2441
                test -n "$sleep" && sleep 0.2
                continue
            }
            out="${(j..)out}"
            if test -n "$out" ; then
                assert pbcopy "$out" || continue
            fi
            if isKitty ; then
                hs-hyper-x
            else
                hs-hyper-x
            fi
            if test -n "$out" ; then
                hs-cmd-v
            fi
        done
    } always {
        tty-title "$PWD"
    }
}
##
function pbpaste-transform() {
    pbpaste | reval "$@" | pbcopy-ask
}
alias pope="pbpaste-transform"

function pbcopy-ask() {
    in-or-args2 "$@"
    ec '==================='$'\n'
    ecn "$inargs[*]" | bat --style=plain
    if ask "Copy?" Y ; then
        pbcopy "$inargs[*]"
        return $?
    else
        return 1
    fi
}
alias pca="pbcopy-ask"
##
function clipboard-add-quoted() {
    local cmd="$(gq "$@")" os_copy="${clipboard_add_quoted_os}"

    if bool $os_copy ; then
        ecbold "Copied: $cmd"
        pbcopy "$cmd"
    else
        clipboard-add "$cmd"
    fi
}

function pbcopy-z {
    if (( ${#@} >= 1 )) ; then
        @opts os y @ clipboard-add-quoted "$@"
    else
        local cmd="$(hist-last 1)"

        ecbold "Copied: $cmd"
        pbcopy "$cmd"
    fi
}
alias pcz='pbcopy-z'
##
function clipboard-add-files() {
    if isDarwin ; then
        pbadd $@
        # will be added to our own clipboard via the recorder
    else
        local i
        for i in $@ ; do
            i="$(grealpath -e "$i")" @TRET

            clipboard-add "$i"
        done
    fi
}
alias pba='clipboard-add-files'
##
