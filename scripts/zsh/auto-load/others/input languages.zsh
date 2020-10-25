# https://stackoverflow.com/questions/64223773/how-to-translate-a-string-from-a-keyboard-layout-to-another
##
# edit this in vim or sth; emacs's RTL support makes it harder.
export persian_exc_chars="ضصثقفغعهخحجچشسیبلاتنمکگظطزرذدپوؤئيإأآة»«؛كٓژٰ‌ٔء؟٬٫﷼٪×،ـ"
export persian_chars="ضصثقفغعهخحجچشسیبلاتنمکگظطزرذدپو.ًٌٍَُِّْ][}{|ؤئيإأآة»«:؛كٓژٰ‌ٔء<>؟٬٫﷼٪×،)(ـ"
export en_chars="qwertyuiop[]asdfghjkl;'zxcvbnm,.QWERTYUIOP{}|ASDFGHJKL:\"ZXCVBNM<>?@#\$%^&()_"
##
function per2en() {
    gsed "y/$persian_chars/$en_chars/"
}
aliasfn p2e per2en
function ppe() { pbpaste | reval-copy per2en }

function en2per() {
    gsed "y/$en_chars/$persian_chars/"
}
aliasfn e2p en2per
function pep() { pbpaste | reval-copy en2per }

function lang-toggle() {
    local out="$*" to=US
    # if [[ "$persian_exc_chars" == *"${out[1]:-A}"* ]]
    case "$(input-lang-get-fast)" in
        U.S.) to='Persian-ISIRI2901' ;;
        Persian*) to='US' ;;
    esac
    case "$to" in
        US)
            ec "$out" | per2en
            input-lang-set en
            ;;
        Persian*)
            ec "$out" | en2per
            input-lang-set per
            ;;
    esac
    # input-lang-set toggle
}
##
input_lang_push_lang_get() redism get input_lang_push_lang
input_lang_push_lang_set() { silent redism set input_lang_push_lang "$@" ; }
input_lang_push_lang_setnx() { silent redism setnx input_lang_push_lang "$@" ; }
input_lang_push_lang_del() { silent redism del input_lang_push_lang ; }
function input-lang-push() {
    local force="$input_lang_push_force"

    if test -n "$input_lang_push_force" ; then
        input_lang_push_lang_setnx "$(input-lang-get-fast)" &
    fi
    @opts nopopreset y @ input-lang-set "$@"
}
function input-lang-pop() {
    local input_lang_push_lang="$(input_lang_push_lang_get)"
    if test -n "$input_lang_push_lang" ; then
        input-lang-set "$input_lang_push_lang"
    fi
}
##
function input-lang-set-darwin-old() {
    # https://apple.stackexchange.com/questions/402855/how-to-switch-the-keyboard-input-language-from-the-terminal
    # https://github.com/Lutzifer/keyboardSwitcher
    ##
    # https://github.com/myshov/xkbswitch-macosx
    # `hyperfine --warmup 5 'xkbswitch -s 3' 'xkbswitch -s 0' 'xkbswitch -se Persian-ISIRI2901' 'xkbswitch -se US'`
    # they all ran about the same 77ms
    local wanted="${1:l}" to='US'
    case "$wanted" in
        en*|us|u.s*) to=US ;;
        fa*|per*) to='Persian-ISIRI2901' ;;
        toggle*)
            case "$(input-lang-get)" in
                U.S.) to='Persian-ISIRI2901' ;;
                Persian*) to='US' ;;
            esac
        ;;
        *) ecerr "Not supported" ; return 1 ;;
    esac
    xkbswitch -se "$to"
    ##
    # toggles:
    # cliclick kd:ctrl kp:space ku:ctrl # takes ~0.4
    ##
}
function input-lang-set-darwin() {
    # `hyperfine --warmup 5 'xkbswitch -se US' "hs -c 'langSetEn()'"` 72 vs 29
    local wanted="${1:l}"
    case "$wanted" in
        en*|us|u.s*) hammerspoon -c 'langSetPer()' ;;
        fa*|per*) hammerspoon -c 'langSetEn()' ;;
        toggle*)  hammerspoon -c 'langSetToggle()' ;;
        *) ecerr "Not supported" ; return 1 ;;
    esac
}
function input-lang-set() {
    # @darwinonly
    local nopopreset="$input_lang_set_nopopreset"

    if isDarwin ; then
        input-lang-set-darwin "$@"
    else
        return 6
    fi
    btt-refresh 623FC96A-0BD0-4463-B186-D4E55024A637
    if test -z "$nopopreset" ; then
        input_lang_push_lang_del
    fi
}
function input-lang-get-darwin-fast() {
    # bug: not updated instantaneously
    ## Perf: (8.5x faster)
    # `hyperfine --warmup 5 "defaults read $HOME/Library/Preferences/com.apple.HIToolbox.plist AppleSelectedInputSources" 'rg --quiet -F "LayoutTU.S." ~/Library/Preferences/com.apple.HIToolbox.plist'`
    ##
    if rg --quiet -F "LayoutTU.S." ~/Library/Preferences/com.apple.HIToolbox.plist ; then
        ec "U.S."
    else
        ec "Persian-ISIRI 2901"
    fi
}
function input-lang-get-darwin-old() {
    defaults read ~/Library/Preferences/com.apple.HIToolbox.plist AppleSelectedInputSources | command rg -e '"KeyboardLayout Name" = "([^"]*)"' --replace '$1' --only-matching --color never
}
function input-lang-get-darwin() {
    ## hammerspoon is quite fast:
    # `hyperfine --warmup 5 "hs -c 'hs.keycodes.currentSourceID()'" "xkbswitch -ge" "input_lang_get_objc"` 28ms vs 71ms
    ##
    if [[ "$(hammerspoon -A -c 'hs.keycodes.currentSourceID()')" =~ 'com.apple.keylayout\.(.*)' ]] ; then
        ec "${match[1]}"
    else
        input_lang_get_objc # @alt: `xkbswitch -ge`
    fi
}
function input-lang-get-fast() {
    # @darwinonly
    # fnswap input-lang-get-darwin input-lang-get-darwin-fast input-lang-get "$@"
    input-lang-get "$@" # fast was too unreliable
}
function input-lang-get() {
    local lang="$(input-lang-get-darwin)" # @darwinonly
    case "${lang:l}" in
        us|u.s.) ec "U.S.";;
        fa*|persian*)
            ec "Persian" ;;
        *) ec "$lang" ;;
    esac

}
function input-lang-get-icon() {
    ##
    # `hyperfine --warmup 5 "input_lang_get_icon" 'brishz.dash input-lang-get-icon'`
    #  33 vs 67
    # Old result (from when the rust version used input_lang_get_objc):
    #  80 vs 71
    #  Using hs will cost us 2.2 cpu percentages.
    ##
    local lang="$(input-lang-get)"
    case "${lang:l}" in
        us|u.s.) ec "🇺🇸";;
        persian*)
            # ec "🇱🇧" ;;
            ec "🇮🇷" ;;
        *) ec "$lang" ;;
    esac
}
##
