function 2mobi() {
    doc usage: FILE calibre-options
    jglob
    ebook-convert "$1" "${1:r}.mobi" "${@:2}"
}
function 2m2k() {
    doc usage: FILE calibre-options
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
    aa "$@" --on-download-complete aa-toKindle
}
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

2epub() {
	jglob
ebook-convert "$1" "${1:r}.epub" "$@[2,-1]"
}
"jfic"() {
	jee
	re "fanficfare --non-interactive" "$@"
	sout re p2k *.epub
}
dir2k() {
	local dir="${1:-.}/"
	local p
    p=($dir/*.pdf(N))
    skipglob "re pdf-crop-margins" "${(@)p}"
    skipglob "re p2k" $dir/*.(epub|mobi|azw(|?))(N)
    skipglob "re p2ko" $dir/*.pdf(N)
}
"jlib"() {
	jee
	serr re "libgen-cli download -o ." "${(f@)$(re libgen2md5 "$@")}"
	dir2k
}
libgen2md5() {
	[[ "$1" =~ '(\w{32})\W*$' ]] && print -r -- "$match[1]"
}
p2k() {
    doc possibly send to kindle
    jglob
    silent ebook-cover $1 "${1:r}".jpg
    [[ -n "$pk_no" ]] || {
        sout 2m2k "$@"
    	[[ "$1" =~ '.*\.mobi' ]] || \rm "${1:r}.mobi"
    }
}
p2ko() {
    doc possibly send to kindle
    silent pdf-cover "$1"
    [[ -n "$pk_no" ]] || {
        sout 2ko "$@"
    }
}
function getpdfs() {
	zargs -i _ -- "$@" -- getlinks _ '\.pdf$' | inargsf aacrop -Z
}
noglobfn getpdfs
