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
        en*) to=US ;;
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
