function tsendf() { tsend "$1" '' -f "${@:2}" }
air() { zargs -i ___ -- "$@" -- tsendf ___ "$(mpv-get)"}
enve() {
    mdoc 'enve; The Enveloper ^_^
An enhancer that sends stdout asynchronously to my Telegram. Also prints stdout.
Usage: me= enve command ...' MAGIC
    local out="$(eval "$(gquote "$@")")"
    ec "$out"
    tsend $me "$out" &
}
