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
    case "$(input-lang-get)" in
        U.S.) to='Persian-ISIRI2901' ;;
        Persian*) to='US' ;;
    esac
    case "$to" in
        US) ec "$out" | per2en ;;
        Persian*) ec "$out" | en2per ;;
    esac
    input-lang-set toggle
}
##
input_lang_push_lang_get() redism get input_lang_push_lang
input_lang_push_lang_set() { silent redism set input_lang_push_lang "$@" ; }
function input-lang-push() {
    local force="$input_lang_push_force"

    local input_lang_push_lang="$(input_lang_push_lang_get)"
    if test -n "$input_lang_push_force" || test -z "$input_lang_push_lang" ; then
        input_lang_push_lang_set "$(input-lang-get)"
    fi
    @opts nopopreset y @ input-lang-set "$@"
}
function input-lang-pop() {
    local input_lang_push_lang="$(input_lang_push_lang_get)"
    if test -n "$input_lang_push_lang" ; then
        input-lang-set "$input_lang_push_lang"
        input_lang_push_lang_set ''
    fi
}
##
function input-lang-set-darwin() {
    # https://apple.stackexchange.com/questions/402855/how-to-switch-the-keyboard-input-language-from-the-terminal
    # https://github.com/Lutzifer/keyboardSwitcher
    ##
    # https://github.com/myshov/xkbswitch-macosx
    # `hyperfine --warmup 5 'xkbswitch -s 3' 'xkbswitch -s 0' 'xkbswitch -se Persian-ISIRI2901' 'xkbswitch -se US'`
    # they all ran about the same 77ms
    local wanted="${1:l}" to='US'
    case "$wanted" in
        en*|u.s*) to=US ;;
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
        input_lang_push_lang_set ''
    fi
}
function input-lang-get-darwin() {
    defaults read ~/Library/Preferences/com.apple.HIToolbox.plist AppleSelectedInputSources | command rg -e '"KeyboardLayout Name" = "([^"]*)"' --replace '$1' --only-matching --color never
}
aliasfn input-lang-get input-lang-get-darwin # @darwinonly
function input-lang-get-icon() {
    local lang="$(input-lang-get)"
    case "$lang" in
        U.S.) ec "🇺🇸";;
        Persian*)
            # ec "🇱🇧" ;;
            ec "🇮🇷" ;;
        *) ec "$lang" ;;
    esac
}
##
