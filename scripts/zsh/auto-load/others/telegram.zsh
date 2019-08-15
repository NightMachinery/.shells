function tsendf() { tsend "$1" '' -f "${@:2}" }
air() { zargs -i ___ -- "$@" -- tsendf ___ "$(mpv-get)"}
