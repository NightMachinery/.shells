transi() { trans "$*" | erase-ansi }
function sdc() {
    it2prof dark
    sdcv --non-interactive --color "$*" | less
    it2prof 'Hotkey Window'
}
pdc() { sdc "$(strip "$(pbpaste)" '\s+')" }
function sp() { ispell<<<"$*" ; }
