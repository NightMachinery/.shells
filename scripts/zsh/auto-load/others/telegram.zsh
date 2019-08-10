function tsendf() { tsend "$1" '' -f "${@:2}" }
air() { zargs -i _ -- "$@" -- tsendf _ "$(mpv-get)"}
