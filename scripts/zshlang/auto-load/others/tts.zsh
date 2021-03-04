function tts-gateway() {
    local text="${*:?}" engine=("${tts_gateway_engine[@]:-${tts_gateway_e[@]:-tts-espnet}}") postproc=("${tts_gateway_postproc[@]}")

    ##
    # local tmp="$(gmktemp --suffix .wav)"
    # <<<"$text" ESPnet2-TTS.py > "$tmp"
    ##

    local cache="$HOME/.chache/tts_gateway/$(md5m "$engine[*] ||| $text").wav"
    local log="${cache}_stderr.txt"
    ensure-dir "$cache"
    if isDeus || ! test -e "$cache" ; then
        reval "$engine[@]" "$text" "$cache" >&$log || {
            ecerr "TTS failed with ${?}:"
            cat "$log" >&2
            return 1
        }
    fi

    if test -n "$postproc[*]" ; then
        local cache_pp="${cache:r}_$(md5m "$postproc[*]").wav"
        if isdbg || isDeus || ! test -e "$cache_pp" ; then
            reval "$postproc[@]" "$cache" "$cache_pp"
        fi
        cache="$cache_pp"
    fi

    ecdbg "$0: File generated at $cache"

    hearinvisible "$cache" || ecerr "$0: Playing '$cache' failed with $?"
}
aliasfn tts-glados1-cached @opts e tts-glados1 @ tts-gateway
aliasfn tts-gateway-i1 @opts postproc audiofx-infantilize1 @ tts-gateway
aliasfn tts-gateway-i2 @opts postproc audiofx-infantilize2 @ tts-gateway
aliasfn tts-gateway-g1 @opts postproc audiofx-gibberish1 @ tts-gateway
##
function audiofx-gibberish1() {
    audiofx-sox "$@" speed 3 : pitch 500 : reverb
}
function audiofx-infantilize1() {
    audiofx-sox "$@" speed 1.3 : pitch 500 : reverb
}
function audiofx-infantilize2() {
    audiofx-sox "$@" speed 1.6 : pitch 900 : reverb
}
function audiofx-sox() {
    # @alt tsox
    local i="${1:?}" o="${2:?}" ; shift 2
    sox "$i" "$o" -G "$@"
}
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
