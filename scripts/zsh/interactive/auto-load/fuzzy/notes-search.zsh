function ntl() {
    typeset -a ntsearch_rg_opts
    ntsearch_injector="unseal-get2note" ntsearch_glob='' ntsearch_rg_opts=("$ntsearch_rg_opts[@]" --iglob '*.txt' --iglob '*.md' --iglob '*.org') ntl-rg "$@"
}
noglobfn ntl
aliasfnq-ng ntl. nightNotes=. ntsearch_glob='' ntsearch-lines # ntsearch_glob=$textglob
@opts-setprefix ntl. ntsearch-lines
aliasfn-ng see ntl.
aliasfnq-ng sees nightNotes=. ntsearch_glob='' ntl-rg # ntsearch_glob=$textglob
aliasfn-ng seesall ntsearch_rg_opts=(--no-ignore --hidden) nightNotes=. ntsearch_glob='' ntl-rg # still does not see binaries
function seev() {
    init-vfiles
    ntLines=y nightNotes="/" ntsearch_additional_paths=($vfiles[@]) ntsearch_glob='' ntl-rg "$@" # ntsearch_glob=$textglob
}
noglobfn seev
aliasfn-ng agsi nightNotes="$NIGHTDIR" ntsearch_additional_paths=(~/.zshenv ~/.zshrc ~/.shared.sh ~/.localScripts ~/.glances ~/.vimrc ~/.ideavimrc ~/.tmux.conf ~/.privateBTT.sh ~/.privateShell ~/.privateStartup.sh ~/test_nonexistent) ntsearch_glob='' ntl-rg # ntsearch_glob=$textglob
function agfi() {
    local f="$1"
    ntsearch_query_fzf="'$f '() | 'alias | 'alifn" agsi  # match functions or aliases
}
noglobfn agfi
##
function ntt() {
    local query="$(fz-createquery "$@")"

    ntsearch_query_fzf="$query" ntl
}
###
function rem-fz() {
    local query_pre="$rem_fz_q"
    local query="$(fz-createquery "$@")"

    local fz_opts=( "$fz_opts[@]" --no-sort ) # no-sort is needed to get the items sorted according to their actual date

    local nightNotes="$remindayDir"

    local now=("${(s./.)$(datej)}")
    local cyear="$now[1]"
    local cmonth="$now[2]"
    local cday="$now[3]"
    local ntsearch_additional_paths=( "$remindayBakDir/$cyear/$cmonth/$cday"^*.zsh(N.) )

    typeset -a ntsearch_rg_opts
    ntsearch_glob='' ntsearch_rg_opts=("$ntsearch_rg_opts[@]" --iglob '!*.zsh') ntl-fzf "$query_pre $query"
}
aliasfn ntrem rem-fz
aliasfn rem rem-fz
function remd() {
    ## testing
    # `fnrep datej "ec 1399/12/03" remd`
    ##
    local query="$(fz-createquery "$@")"

    local now=("${(s./.)$(datej)}")
    local cyear="$now[1]"
    local cmonth="$now[2]"
    local cday="$now[3]"

    if (( cmonth == 12 )) ; then
        local nextYear=$((cyear+1))

        @opts q "'$cyear/$cmonth | '$nextYear/01" @ rem-fz $query
    else
        typeset -Z2 nextMonth
        nextMonth=$((cmonth+1))

        @opts q "'$cyear/$cmonth | '$cyear/$nextMonth" @ rem-fz $query
    fi
}
###
function ntimg() {
    innt fdrp --full-path --extension png --extension jpg --extension svg --extension jpeg "$*" | fz | inargsf imgcat
}
aliasfn nti ntimg
###
alias nts='\noglob ntsearch'
###
##
function rgf_() {
    # @todo shortcuts to navigate multiple lines at a time (to skip the context lines) will help

    # local opts=( --delimiter ':|-|\x00' --with-nth 1,3..)
    local opts=( )
    RG_PREFIX="$functions[rgbase] -C 4 --null --line-number --no-heading -e"
    local INITIAL_QUERY=( "$@" )

    FZF_DEFAULT_COMMAND="$RG_PREFIX $(gq ${INITIAL_QUERY:-.})" \
        fz-empty --reverse --bind "change:reload:$RG_PREFIX {q} || true" ${opts[@]}  --ansi --disabled --query "$INITIAL_QUERY" --expect=alt-enter | {
        read -d $'\n' -r acceptor
        outFiles="$(cat)"
        print -r -- "$outFiles"
        outFiles=("${(@f)outFiles}")
    }
}
# @bug `--` won't fit this pattern, though it shouldn't be included in your selections anyways?
aliasfnq rgf @opts engine rgf_ pattern '(.*)\0(\d+)[:-](.*)' @ ntl.
aliasfn seerg rgf
aliasfn rgsee rgf
aliasfn ffrg rgf
##
function emcnt() {
    emc -e "(night/search-notes)"
}
function ugnt() {
    local i args=()
    for i in "$note_formats[@]" ; do
        args+=( -O "$i" )
    done
    ugm "$args[@]" "$@" $nightNotes/
}
function vnt() {
    outFiles=()
    { ntsearch "$@" > /dev/null } || return 1
    reval "${veditor[@]}" "$outFiles[@]"
}
##
function ntl-rg() {
    ntsearch_query_rg="$*" ntsearch-lines
}
function ntl-fzf() {
    ntsearch_query_fzf="$*" ntsearch-lines
}
function ntsearch-lines() {
    : "Remember that ntsearch_ uses eval-memoi"
    : "Note that ntsearch and ntsearch_ use their input as a glob to filter files"
    : "Use alt-enter to jump straight into editing the files! Multiple selection is possible!"

    : "The engine should return output in outFiles."
    local engine=("${(@)ntl_engine:-ntsearch}") pattern="${ntl_pattern}"
    test -z "$pattern" && pattern='^([^:]*):([^:]*):(.*)'

    outFiles=() out=() # out and outFiles contain almost the same data when ntLines=y
    ntLines=y reval "$engine[@]" "$@" > /dev/null  || return 88 # Beware forking here! We need the global vars outFiles and acceptor

    local i files=() linenumbers=() lines=() file
    for i in "${out[@]}" ; do
        unset match
        if [[ "$i" =~ "$pattern" ]] ; then
            file="$match[1]"
            test -e "$file" || {
                # file="$(<<<$file rmprefix "$nightNotes")"
                file="$nightNotes/$file"
            }
            if ! test -e "$file" ; then
                if true || test -z "$ntsearch_injector" ; then
                    ecerr "$0: $(gq "$file") does not exist."
                else
                    color 100 200 200 "$0: File does not exist, probably a selection from the injector."$'\n\n' >&2
                    dvar file
                fi
            else
                files+="$file"
                linenumbers+="$match[2]"
                lines+="$match[3]"
                continue
            fi
        else
            if true || test -z "$ntsearch_injector" ; then
                ecerr "$0: ntsearch has returned illegible data."
            else
                color 100 200 200 "$0: illegible data, probably a selection from the injector."$'\n\n' >&2
            fi
            dvar i
        fi
        ## Old way: Just returning everything
        # ec "$(<<<"${(F)out}" rmprefix "$nightNotes")"
        # return 1
        ## New way: faking data
        files+="~/tmp/madeup.txt"
        linenumbers+="1"
        lines+="$i"
    done

    if [[ "$acceptor" == '' ]] ; then
        ecdbg "$0: Outputting selections ..."
        local sel="${(F)lines}"
        <<<"${sel}" urls-copy
        return 0
    else
        ecdbg "$0: Opening editor ..."
        if [[ "$EDITOR" =~ '^emacs' ]] ; then
            local cmd='(progn '
            for i in {1..$#files} ; do
                # (forward-char $col)
                cmd+=" (find-file \"${files[$i]}\") (goto-line ${linenumbers[$i]}) "
                cmd+=' (recenter)'
            done
            # lower than this 1.5 delay will not work. More delay might be necessary for edge cases.
            # The first time we use this in a zsh session, the highlight sometimes does not work. Idk why.
            cmd+="(run-at-time 0.15 nil #'+nav-flash-blink-cursor-h)"
            cmd+=')'
            revaldbg emacsclient -t -e "$cmd"
        else
            # should work with both emacs and vim
            # VSCode: code --goto <file:line[:character]> Open a file at the path on the specified line and character position.--goto file:line[:col]
            # I don't know about opening multiple files on vscode (we can always run the command multiple times)
            local cmd="$EDITOR "
            for i in {1..$#files} ; do
                cmd+="+${linenumbers[$i]} $(gq "${files[$i]}") "
            done
            eval "$cmd"
        fi
    fi
}
function ntsearch() {
    : "GLOBAL: out outFiles"

    out=''
    outFiles=()
    local nightNotes="$nightNotes"
    export nightNotes="$(realpath --canonicalize-existing "$nightNotes")" || { # we need to export this because the preview shell accesses it.
        ecerr "$0: nightNotes dir does not exist"
        return 1
    }

    ntsearch_ "$@" > /dev/null || {
        ecerr "$0: ntsearch_ failed $?."
        unset out
        return 1
    } # don't fork here. Receiving globals vars out and acceptor.

    out=( "${(@0)out}" )
    out=( "${(@)out[1,-2]}" ) # remove empty last element that \0 causes

    if test -z "$ntLines" ; then
        local i
        for i in "$out[@]"
        do
            outFiles+="$nightNotes/$(<<<"$i" head -n 1)"
        done
        ec "${(@F)out}"
    fi
}
function ntsearch_() {
    : "See vnt, ntsearch-lines"
    : "INPUT: ntsearch_query_rg, ntsearch_query_fzf, ntsearch_additional_paths, nightNotes, ntLines , GLOBAL: acceptor out"

    acceptor=''
    out=''

    local ntLines="$ntLines"

    local additional_paths=(${(@)ntsearch_additional_paths})
    # Checking for existence introduces noticeable delay, but has little benefit. So we don't.
    # local additional_paths=("${(@f)$(<<<${(F)ntsearch_additional_paths} filter test -e)}")

    dvar additional_paths
    local glob="*$ntsearch_glob"
    local files
    # files=( "${(@f)$(fd -e md -e txt -e org --full-path ${pattern} $nightNotes )}" )
    files=(${additional_paths[@]})
    [[ "$nightNotes" != '/' ]] && {
        if test -n "$ntsearch_glob" ; then
            files+=($nightNotes/**/${~glob}) # `/` means we are only interested in additional_paths, but since the machinary relies on nightNotes existing, we use `/`.
        else
            files+="$nightNotes"
        fi
    }
    (( $#files == 0 )) && {
        ecerr "$0: No valid paths supplied. Aborting."
        return 1
    }
    local query_rg="${ntsearch_query_rg:-\S+}" # removes empty lines
    local query="$ntsearch_query_fzf"
    # test -z "$query" || query="'$query"
    # local pattern="."

    local fzopts=()
    local previewcode="$FZF_SIMPLE_PREVIEW"
    if test -z "$ntLines" ; then
        fzopts+='--read0'
    else
        fzopts+=(--read0 --delimiter : --with-nth '1,3..' --nth '..') # nth only works on with-nth fields
        ## Old previewer
        # local FZF_SHELL='zshplain.dash'
#         previewcode=( 'ln={2} file={1} match={s3..} ; fileabs="$nightNotes/$file" ;
# { print -r -- "$file" '
#                       "\$'\\n'$(gq $(colorbg 200 255 200 ; colorfg 0 0 0))\$match$(gq $reset_color)\$'\\n\\n' ; "
#                       # 'echo ln: $ln ; '
#                       '[[ $ln == 1 ]] || gsed -n $(( i=ln-6 , i >= 1 ? i : 1 )),$(( i=ln-1 , i >= 1 ? i : 1 ))p $fileabs ; '
#                       "print -r -- $(gq $(colorbg 255 255 255 ; colorfg 255 120 0))\$match$(gq $reset_color) ; "

#                       '
# gsed -n $((ln+1)),$((ln+50))p $fileabs
# } |& ' "$(gq "$(realpath2 ansifold)")" '-s -w $(($FZF_PREVIEW_COLUMNS - 1))' )
        # install location is at perl -V:'installbin' , link it
        # cpanm App::ansifold
        # https://metacpan.org/pod/Text::ANSI::WideUtil
        ##
        ##
        # https://github.com/junegunn/fzf/issues/2373 preview header
        # remove `:+{2}-5` from preview-window and use mode=0 to revert to the previous behavior
        previewcode="ntom {1} {2} {s3..} $(gq $nightNotes) 1 || printf -- \"\n\n%s \" {}"
        ##
        # previewcode="cat $(gq $nightNotes)/{1} || printf -- error5"
    fi

    # we no longer need caching, it's fast enough
    # memoi_expire=$((3600*24)) memoi_key="${files[*]}:${ntLines}:$nightNotes:$query_rg" eval-memoi
    ntsearch_fd | fz_empty=y fz --preview-window 'right:50%:wrap:nohidden:+{2}-/2' --preview "$previewcode[*]" --ansi ${fzopts[@]} --print0 --query "$query"  --expect=alt-enter | {   # right:hidden to hide preview
        read -d $'\0' -r acceptor
        out="$(cat)"
        ec "$out"
    } # | gawk 'BEGIN { RS = "\0" ; ORS = RS  } ;  NF' # to remove empty lines (I don't know why I commented this, or if it actually works. But I now use rg itself to filter empty lines out.

}
function ntsearch_fd() {
    : "INPUT VARS: ntsearch_injector ntsearch_rg_opts files ntLines nightNotes query_rg"

    if test -n "$ntLines" ; then
        {
            # no-messages suppresses IO errors
            # xargs is needed to avoid argument list too long error
            { print -nr -- "${(@pj.\0.)files}" | gxargs -0 rg --no-binary --smart-case --engine auto --no-messages --with-filename --line-number --sort path $ntsearch_rg_opts[@] -- "$query_rg" | prefixer --skip-empty -r "$nightNotes" -i $'\n' -o '\x00' }
            # Old way: rmprefix "$nightNotes" $'\n' '\x00'
        }
        reval "$ntsearch_injector[@]" | command rg --null-data --smart-case --engine auto --no-messages $ntsearch_rg_opts[@] -- "$query_rg"
        return 0 # rg exits nonzero if some of its paths don't exist, so we need to explicitly ignore it.
    else
        local first='y' file filename content line i
        for file in "$files[@]"
        do
            test -f "$file" || continue

            test -n "$first" || {
                if test -z "$ntLines" ; then
                    print -n $'\0'
                else

                fi
            }
            first=''
            filename="$(realpath-relchild $nightNotes $file)" # idk if realpath-relchild is appropriate here, we might have assumed that the path is really relative to nightNotes
            content="$(< $file)"
            color 30 90 255 $filename$'\n'
            ec $content
        done
    fi
}
