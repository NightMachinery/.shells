##
function kindle-p {
    ! bool "$kindle_disabled"
}

function without-kindle {
    kindle_disabled=y reval-env "$@"
}
aliasfn pkno without-kindle
aliasfn nokindle without-kindle
##
function 2mobi() {
    : "Usage: FILE calibre-options ..."

    jglob
    ebook-convert "$1" "${1:r}.mobi" "${@:2}"
}
alias 2m='2mobi'

function 2m2k() {
    : "Usage: FILE calibre-options ..."

    jglob
    [[ "$1" =~ 'mobi.az1$' ]] && {
        mv "$1" "${1:r}"
        ecdbg "az1 detected; Renaming to ${1:r}"
        set -- "${1:r}"
    } || if test "${1:e}" != mobi ; then
        2mobi "$@"
        set -- "${1:r}.mobi"
    fi
    2kindle "$1"
}

function mv2ko() {
    jej
    mv * "$1"
    2ko *
}

function aacrop() {
    aa "$@" --on-download-complete aa-crop
}

function aap() {
    aa "$@" --on-download-complete aa-pToKindle
}

function aab() {
    local url="$1"
    assert-args url @RET;

    local aaMark="$(uuidm)"
    aaMark="$aaMark" aa-remotename --on-download-complete aa-toKindle.zsh "$url"
    till-file "$aaMark"
}
renog aab

function 2kindle() {
    jglob
    mutt -s "${2:-convert}" -a "$1" -- "${3:-$kindle_email}" <<<hi
}

function 2ko() {
    mdoc "2kindle-original; Sends to Kindle without conversion.
Usage: $0 <file> [<kindle-email>]
Uses 2kindle under the hood." MAGIC

    jglob
    2kindle "$1" "some_subject" "$2"
}
function 2p2k() {
    jglob
    k2pdf "$1"
    2ko "${1:r}_k2opt.pdf"
}
2m2k2h() { 2m2k "$@" && { trs "$1"
                          trs "${1:r}.mobi" } }

function 2epub() {
    jglob
    ebook-convert "$1" "${1:r}.epub" "$@[2,-1]"
}

function dir2k() {
    local dir="${1:-.}/"
    local p
    p=($dir/*.pdf(N))
    # skipglob "re pdf-crop-margins-inplace" "${(@)p}"
    skipglob "re p2k" $dir/*.(epub|mobi|azw(|?))(N)
    skipglob "re p2ko" $dir/*.pdf(N)
}

function p2k() {
    : "possibly send to kindle"
    : "stdout: the new name of the input file, if not deleted"

    jglob
    local delOrig="${pkDel}"
    # local delConverted="${pkDelC}"

    if kindle-p ; then
        sout 2m2k "$@"
        if ! [[ "$1" =~ '.*\.mobi$' ]] ; then
            silent trs-rm "${1:r}.mobi"
        fi
    fi

    if test -z "$delOrig" ; then
        silent ebook-cover "$1" "${1:r}".jpg
        local nn
        nn="$(rename-ebook "$1")" @TRET

        ## Telegram or Telethon doesn't support filenames bigger than 64.
        local nnt="$nn:t"
        ecdbg "nnt: $nnt"
        if (( ${#nnt} > 64 )) ; then
            local dest
            dest="${nn:h}/$(<<<${nnt[1,59]} trimsed).${nn:e}" @TRET
            assert command mv "$nn" "$dest" @RET
            nn="$dest"
        fi
        ##

        ec "$nn"
        return 0
    else
        silent trs-rm "$1"
    fi
}

function p2ko() {
    silent pdf-cover "$1"

    if kindle-p ; then
        sout 2ko "$@"
    fi
}
##
function getpdfs() {
    zargs -i _ -- "$@" -- getlinks _ '\.pdf$' | inargsf aacrop -Z
}
noglobfn getpdfs
##
function jgetktmp() {
    local num="${1:-3}"
    ktmp
    lm|filter testre '\.epub$' | tail -n "$num" |inargsf rexa 'cp _ $jd'
}
aliasfn ktmp-get jgetktmp
aliasfn ktmpg jgetktmp
##
function 2pdf() {
    jglob
    local f="$1"
    local margin=1
    local scale="${2:-1.75}"
    ebook-convert $f ${f:r}.pdf --custom-size=$((4.6/$scale))x$((6.7/$scale)) --pdf-page-margin-left=$margin --pdf-page-margin-right=$margin --pdf-page-margin-top=$margin --pdf-page-margin-bottom=$margin
}
##
function kindle-sdr-backup() {
    command gcp -r -v ~vol/Kindle/documents/**/*.sdr ~base/Backup/Kindle/sdr
}

function kindle-clippings-backup() {
    # reval-ec kindle-sdr-backup

    local d="$kindle_clippings_dir"
    assert mkdir -p "$d" @RET
    local o="${d}/My Clippings.txt"

    assert command gcp -v ~vol/"Kindle/documents/My Clippings.txt" "$o" @RET

    kindle-clippings-orgify
}

function kindle-clippings-orgify() {
    local d="$kindle_clippings_dir"
    local o="${d}/My Clippings.txt"
    assert test -e "$o" @RET

    trs "$kindle_clippings_org_dir"
    mkdir -p "$kindle_clippings_org_dir"
    assert indir "$d" fyodor "$o" "$kindle_clippings_org_dir" @RET
}

function kindle-clippings-diff() {
    indir "$kindle_clippings_dir" git-diff HEAD -- "$kindle_clippings_org_dir"
}
alias kcd=kindle-clippings-diff
##
function kfx-open() {
    assert isDarwin @RET

    local f="$1"
    assert test -e "$f" @RET

    local tmp
    tmp="$(gmktemp --suffix='.azw8')" @TRET

    assert cp "$f" "$tmp" @RET
    open -a "Kindle Previewer 3" "$tmp"
}
##
