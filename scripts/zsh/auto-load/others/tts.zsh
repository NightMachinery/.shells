function tts-gateway() {
    local text="${*:?}" engine="${tts_gateway_engine:-${tts_gateway_e:-tts-espnet}}"

    ##
    # local tmp="$(gmktemp --suffix .wav)"
    # <<<"$text" ESPnet2-TTS.py > "$tmp"
    ##

    local cache="$HOME/.chache/tts_gateway/$(md5m "$engine[*] ||| $text").wav"
    local log="${cache}_stderr.txt"
    ensure-dir "$cache"
    if test -n "$deusvult" || ! test -e "$cache" ; then
        reval "$engine[@]" "$text" "$cache" >&$log || {
            ecerr "TTS failed with ${?}:"
            cat "$log" >&2
            return 1
        }
    fi

    ecdbg "$0: File generated at $cache"

    hearinvisible "$cache" || ecerr "$0: Playing '$cache' failed with $?"
}
aliasfn tts-glados1-cached @opts e tts-glados1 @ tts-gateway
##
function tts-espnet() {
    local text="${1}" output="${2:?}"

    <<<"$text" ESPnet2-TTS.py > "$output"
}
##
function tts-glados1() {
    # curl --silent -L --retry 30 --get --fail --data-urlencode "text=$*" "https://glados.c-net.org/generate" | silent hear -

    local text="${1}" output="${2:?}"

    curl --silent -L --retry 30 --get --fail --data-urlencode "text=$text" "https://glados.c-net.org/generate" > "${output:?}"
}
##
