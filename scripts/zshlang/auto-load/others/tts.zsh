function tts-gateway() {
    # Use tts-say, it's the best of them.
    ##
    local text="${*:?}" engine=("${tts_gateway_engine[@]:-${tts_gateway_e[@]:-tts-espnet}}") postproc=("${tts_gateway_postproc[@]}") ext="${tts_gateway_ext:-wav}"

    bella_zsh_disable1

    if isServer ; then
        ecdbg "$0: skipped because this is a server."
        return 0
    fi

    ##
    # local tmp="$(gmktemp --suffix .wav)"
    # <<<"$text" ESPnet2-TTS.py > "$tmp"
    ##

    local cache="$HOME/.cache/tts_gateway/$(ec $engine[*] | str2filename)/$(ecn $text | ghead -c 40 | str2filename)_$(md5m "$engine[*] ||| $text").${ext}"
    local log="${cache}_stderr.txt"
    ensure-dir "$cache"
    if isDeus || ! test -e "$cache" ; then
        revaldbg "$engine[@]" "$text" "$cache" >&$log || {
            ecerr "TTS failed with ${?}:"
            cat "$log" >&2
            return 1
        }
    fi

    if test -n "$postproc[*]" ; then
        local cache_pp="${cache:r}_$(md5m "$postproc[*]").wav"
        if isDeus || ! test -e "$cache_pp" ; then
            revaldbg "$postproc[@]" "$cache" "$cache_pp"
        fi
        cache="$cache_pp"
    fi

    ecdbg "$0: File generated at $cache"

    assert bell-ringer "$0" "$cache" @RET
}
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
function fsay {
    assert isDarwin @RET

    local voice="${fsay_v}" rate="${fsay_r}"
    if test -z "$voice" ; then
        if macos-ventura-or-higher-p ; then
            # voice='Samantha'
            voice='Moira'
        else
            voice='Fiona'
        fi
    fi
    if test -z "$rate" ; then
        if macos-ventura-or-higher-p ; then
            rate='185'
        else
            rate='30'
        fi
    fi

    bella_zsh_disable1

    say -v $voice -r $rate "$@"
}

function fsay2 {
    local text
    text="$(in-or-args "$@")" @RET

    if butler-p ; then
        bb-say "$text"
    elif isDarwin ; then
        fsay "$text"
    else
        @NA
    fi
}

function fsay-noidle {
    if ! idle-p 15 ; then
        fsay2 "$@"
    fi
}

function tts-say {
    local text="${1}" output="${2}"
    assert-args output @RET

    if isDarwin ; then
        <<<"$text" fsay --input-file -  -o "$output"
    else
        tts-espnet "$@"
    fi
}

aliasfn tts-say-cached @opts ext aiff e tts-say @ tts-gateway

function tts-say-i1 {
    #: fsay_r is interpreted differently in different macOS versions, so it's better to just go with the default.
    # local fsay_r=5 # the range 1-50 is indistinguishable to me

    @opts e tts-say ext aiff postproc audiofx-infantilize1 @ tts-gateway "$@"
}

function tts-say-i2 {
    # local fsay_r=80

    @opts e tts-say ext aiff postproc audiofx-infantilize2 @ tts-gateway "$@"
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

    text="$(ecn "$text" | sdlit $'\n' " , ")" # this API does not support multiline input

    curl --silent -L --retry 30 --get --fail --data-urlencode "text=$text" "https://glados.c-net.org/generate" > "${output:?}"
}
aliasfn tts-glados1-cached @opts e tts-glados1 @ tts-gateway
##
