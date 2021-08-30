function ensure-dep-pdftk {
    ensure-dep1 pdftk brew install pdftk-java || return $?
}
##
function ensure-dep-kitty-theme {
    ensure-dep1 kitty-theme pip-install git+git://github.com/fretboardfreak/kitty-theme-changer.git@master || return $?
}
##
