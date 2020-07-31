aliasfn-ng ntl. ntLines=y nightNotes=. noteglob=$codeglob ntl
aliasfn-ng see ntl.
aliasfn-ng sees ntLines=y nightNotes=. noteglob=$codeglob ntl-rg
function seev() {
    init-vfiles
    ntLines=y nightNotes="/" ntsearch_additional_paths=($vfiles[@]) noteglob=$codeglob ntl-rg "$@"
}
noglobfn seev
aliasfn-ng agsi ntLines=y nightNotes="$NIGHTDIR" ntsearch_additional_paths=(~/.zshenv ~/.zshrc ~/.privateBTT.sh ~/.privateShell ~/.privateStartup.sh ~/test_nonexistent) noteglob=$codeglob ntl-rg
function agfi() {
    local f="$1"
    ntsearch_query_fzf="'$f '() | 'alias | 'alifn" agsi  # match functions or aliases
}
noglobfn agfi
###
alias imd='img2md-imgur'
alias nts='\noglob ntsearch'
###
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
    ntsearch_query_rg="$*" ntl
}
function ntl-fzf() {
    ntsearch_query_fzf="$*" ntl
}
function ntl() {
    : "Remember that ntsearch_ uses eval-memoi"
    : "Note that ntsearch and ntsearch_ use their input as a glob to filter files"
    : "Use alt-enter to jump straight into editing the files! Multiple selection is possible!"

    outFiles=() # out and outFiles contain the same data when ntLines=y
    ntLines=y ntsearch "$@" > /dev/null  || return 1 # Beware forking here! We need the global vars outFiles and acceptor

    local i files=() linenumbers=() lines=()
    for i in "${outFiles[@]}" ; do
        unset match
        if [[ "$i" =~ '^([^:]*):([^:]*):(.*)' ]] ; then
            files+="$match[1]"
            linenumbers+="$match[2]"
            lines+="$match[3]"
        else
            ecerr "$0: ntsearch has returned illegible data. Aborting."
            return 1
        fi
    done

    if [[ "$acceptor" == '' ]] ; then
        local sel="${(F)lines}"
        <<<"${sel}" fnswap rg rgm match-url-rg --passthru && {
            local url="$(<<<"${sel}" match-url-rg --only-matching --replace '$1')"
            pbcopy "$url"
        }
        return 0
    else
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
            emacsclient -t -e "$cmd"
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

    ntsearch_ "$@" > /dev/null || { unset out ; return 1 } # don't fork here. Receiving globals vars out and acceptor.

    out=( "${(@0)out}" )
    out=( "${(@)out[1,-2]}" ) # remove empty last element that \0 causes
    local i
    for i in "$out[@]"
    do
        outFiles+="$nightNotes/$(<<<"$i" head -n 1)"
    done
    ec "${(@F)out}"
}
function ntsearch_() {
    : "See vnt, ntl"
    : "INPUT: ntsearch_query_rg, ntsearch_query_fzf, ntsearch_additional_paths, nightNotes, ntLines , GLOBAL: acceptor out"

    acceptor=''
    out=''

    local ntLines="$ntLines"

    local additional_paths=(${(@)ntsearch_additional_paths})
    # Checking for existence introduces noticeable delay, but has little benefit. So we don't.
    # local additional_paths=("${(@f)$(<<<${(F)ntsearch_additional_paths} filter test -e)}")

    dvar additional_paths
    local glob="*${*}$noteglob"
    local files
    # files=( "${(@f)$(fd -e md -e txt -e org --full-path ${pattern} $nightNotes )}" )
    files=(${additional_paths[@]})
    [[ "$nightNotes" != '/' ]] && files+=($nightNotes/**/${~glob}) # `/` means we are only interested in additional_paths, but since the machinary relies on nightNotes existing, we use `/`.
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
        fzopts+=(--delimiter : --with-nth '1,3..' --nth '..') # nth only works on with-nth fields
        local FZF_SHELL='zshplain.dash'
        # FZF_SHELL=zsh
        previewcode=( 'ln={2} file={1} match={3..} ; fileabs="$nightNotes/$file" ;
{ print -r -- "$file" '
                      "\$'\\n'$(gq $(colorbg 200 255 200 ; colorfg 0 0 0))\$match$(gq $reset_color)\$'\\n\\n' ; "
                      # 'echo ln: $ln ; '
                      '[[ $ln == 1 ]] || gsed -n $(( i=ln-6 , i >= 1 ? i : 1 )),$(( i=ln-1 , i >= 1 ? i : 1 ))p $fileabs ; '
                      "print -r -- $(gq $(colorbg 255 255 255 ; colorfg 255 120 0))\$match$(gq $reset_color) ; "

                      '
gsed -n $((ln+1)),$((ln+50))p $fileabs
} |& ' "$(gq "$(rp ansifold)")" '-s -w $FZF_PREVIEW_COLUMNS' )
        # install location is at perl -V:'installbin' , link it
        # cpanm App::ansifold
        # https://metacpan.org/pod/Text::ANSI::WideUtil
    fi

    # we no longer need caching, it's fast enough
    # memoi_expire=$((3600*24)) memoi_key="${files[*]}:${ntLines}:$nightNotes:$query_rg" eval-memoi
    ntsearch_fd | fz --preview-window right --preview "$previewcode[*]" --ansi ${fzopts[@]} --print0 --query "$query"  --expect=alt-enter | {   # right:hidden to hide preview
        read -d $'\0' -r acceptor
        out="$(cat)"
        ec "$out"
    } # | gawk 'BEGIN { RS = "\0" ; ORS = RS  } ;  NF' # to remove empty lines (I don't know why I commented this, or if it actually works. But I now use rg itself to filter empty lines out.

}
function ntsearch_fd() {
    : "INPUT VARS: files ntLines nightNotes query_rg"

    if test -n "$ntLines" ; then
        {
            # no-messages suppresses IO errors
            command rg --no-messages --with-filename --line-number "$query_rg" "${files[@]}" || true # rg exits nonzero if some of its paths don't exist, so we need to explicitly ignore it.
        } | rmprefix "$nightNotes"
            # sd "^$nightNotes" '' # sd is doing the work of `realpath --relative-to` . TODONE this doesn't quote and so is buggy
        ## old way (uses vars now not in scope)
        # i=1
        # for line in "${(@f)content}" ; do
        #     if test -n "$line" ; then
        #         ec "${filename}:${i}:${line}"
        #     fi
        #     i=$((i+1))
        # done
        ##
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
            filename="$(realpath --relative-to $nightNotes $file)"
            content="$(< $file)"
            color 30 90 255 $filename$'\n'
            ec $content
        done
    fi
}
function jrlt() {
    local today="$(date +"%Y.%b.%d") $(datej|tr / -)"
    local dest="$nightJournal/$today.md"
    test -e "$dest" || {
        ec "# $today"$'\n\n'"* " >> $dest
    }
    ! isI || $EDITOR[@] $dest
}
function img2md() {
    mdoc "$0 <picture-file> [<description>]
Outputs the image in markdown format, hardcoded in base64. Large images (~0.3 MB) will probably crash the system though." MAGIC

    jglob

    local file="$1" desc="$2"
    local compressed="$(gmktemp --suffix .jpg)" # ".${file:e}"
    convert $file -define jpeg:extent=150kb $compressed # 200kb didn't work
    file=$compressed
    doc use base64 from brew to ensure consistency
    ## python base64 (might work in eva):
    # encoded_string= base64.b64encode(img_file.read())
    # print(encoded_string.decode('utf-8'))

    # somehow breaks in eva_aget ...
    print -r -- "![$desc](data:$(file -b --mime-type $file);base64,$(base64 "$file" | tr -d '\r\n'))"
}
function img2md-imgur() {
    mdoc "$0 <picture-file> [<description>]
Outputs the image in markdown format, hosted on imgur." MAGIC

    jglob
    local file="$1" desc="$2"

    print -r -- "![$desc]($(imgurNoD=y imgur.bash $file))"
}
function unt() {
    # isI &&
    test -z "$*" && set -- "$(pbpaste)"
    local note="$(url2md "$@")"
    ec $note
    if isI ; then
        pbcopy $note
    fi
}
noglobfn unt
