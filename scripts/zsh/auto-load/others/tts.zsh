function tts-gateway() {
    local text="${1:?}"

    ##
    # local tmp="$(gmktemp --suffix .wav)"
    # <<<"$text" ESPnet2-TTS.py > "$tmp"
    ##

    local cache="$HOME/tmp/tts/$(md5m "$text").wav"
    local log="${cache}_stderr.txt"
    ensure-dir "$cache"
    if test -n "$deusvult" || ! test -e "$cache" ; then
        <<<"$text" ESPnet2-TTS.py > "$cache" 2>$log || {
            ecerr "TTS failed with ${?}:"
            cat "$log" >&2
            return 1
        }
    fi

    ecdbg "$0: File generated at $cache"

    hearinvisible "$cache" || ecerr "$0: Playing '$cache' failed with $?"
}
