isI && {
    source "$NIGHTDIR"/zshlang/widgets.zsh
    precmd_pipestatus() {
        RPROMPT="${(j.|.)pipestatus}"
        if [[ "${(j.|.)pipestatus}" == 0 ]]; then
            RPROMPT=""
        fi
    }
    add-zsh-hook precmd precmd_pipestatus
}

function pp() {
    pngpaste "$1".png
}
function pph() {
    local lastimg="$(l-m)"
    mv "${1:-$(pbpaste)}" "${lastimg:r}.html"
}
