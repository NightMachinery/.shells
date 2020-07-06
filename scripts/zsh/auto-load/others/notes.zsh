###
alias imd='img2md-imgur'
alias nts='\noglob ntsearch'
alias cellar-getcron='crontab -l > $cellar/notes/dev/snippets/crontabs/$(whoami)'
###

function emcnt() {
    emc -e "(night/search-notes)"
}
function ntl() {
    local sel
    sel="$(ntLines=y ntsearch| gcut -d: -f3-)" || return 1
    <<<"$sel" fnswap rg rgm match-url-rg --passthru && {
        local url="$(<<<"$sel" match-url-rg --only-matching --replace '$1')"
        pbcopy "$url"
    }
    return 0
}
function ugnt() {
    local i args=()
    for i in "$note_formats[@]" ; do
        args+=( -O "$i" )
    done
    ugm "$args[@]" "$@" $nightNotes/
}
function vnt() {
    ntsearch "$@" || return 1
    reval "${veditor[@]}" "$outFiles[@]"
}
function ntsearch() {
    out=''
    out=( "${(@0)$(ntsearch_ "$@")}" ) || return 1
    out=( "${(@)out[1,-2]}" ) # remove empty last element that \0 causes
    outFiles=()
    local i
    for i in "$out[@]"
    do
        outFiles+="$nightNotes$(<<<"$i" head -n 1)"
    done
    ec "${(@F)out}"
}
function ntsearch_() {
    local ntLines="$ntLines"
    local glob="*${*}$noteglob"

    local query=""
    # test -z "$query" || query="'$query"
    # local pattern="."

    local fzopts=()
    local previewcode="$FZF_SIMPLE_PREVIEW"
    if test -z "$ntLines" ; then
        fzopts+='--read0'
    else
        fzopts+=(--delimiter : --with-nth '1,3..' --nth '..' ) # nth only works on with-nth fields
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
} |& /usr/local/Cellar/perl/5.30.1/bin/ansifold -s -w $FZF_PREVIEW_COLUMNS' )
        # cpanm App::ansifold
        # https://metacpan.org/pod/Text::ANSI::WideUtil
    fi
    local file files
    # files=( "${(@f)$(fd -e md -e txt -e org --full-path ${pattern} $nightNotes )}" )
    files=($nightNotes/**/${~glob})
    local first='y' filename content line i
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
        if test -z "$ntLines" ; then
            color 30 90 255 $filename$'\n'
            ec $content
        else
            i=1
            for line in "${(@f)content}" ; do
                if test -n "$line" ; then
                    ec "${filename}:${i}:${line}"
                fi
                i=$((i+1))
            done
        fi
    done  | fz --preview-window right --preview "$previewcode[*]" --ansi ${fzopts[@]} --print0 --query "$query"
    # right:hidden to hide preview
    # | gawk 'BEGIN { RS = "\0" ; ORS = RS  } ;  NF'
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
     isI && test -z "$*" && set -- "$(pbpaste)"
     local note="$(url2md "$@")"
     ec $note
     if isI ; then
         pbcopy $note
     fi
 }
 noglobfn unt
