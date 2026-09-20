#: Speech-to-text from the shell, over Cloud Speech-to-Text v2 with the Chirp 3
#: model. m4a/mp3/wav/ogg/flac are decoded server-side.
#: Credentials are shared with [agfi:gcp-translate] (same project and key; the
#: service account carries the Speech client role too).
#:
#: Language: detection works for English but returned "undetermined" on Farsi
#: speech when measured, so [agfi:stt-file-chirp3-fa] names the language outright.
#: The synchronous call takes at most 60 s or 10 MB of audio, so longer files
#: are cut into pieces with ffmpeg and transcribed in order.

typeset -g gcp_stt_model="${gcp_stt_model:-chirp_3}"
typeset -g gcp_stt_region="${gcp_stt_region:-eu}"
typeset -g gcp_stt_chunk_seconds="${gcp_stt_chunk_seconds:-55}"

function stt-file-chirp3 {
    #: Usage: stt-file-chirp3 [--lang en-US[,fa-IR,...]] <audio-file>
    #: Prints the transcript. Without --lang the language is detected.
    local langs='auto'
    if [[ "$1" == --lang ]] ; then
        langs="$2" ; shift 2
    fi
    local file="${1:?audio file}"
    [[ -r "$file" ]] || { ecerr "$0: cannot read $file" ; return 1 }
    ensure-cmd ffmpeg jq @RET
    [[ -n "${gcp_translate_project}" ]] || { ecerr "$0: set gcp_translate_project in ${gcp_conf_file}" ; return 1 }

    local token
    token="$(gcp-translate-token)" @RET
    local url="https://${gcp_stt_region}-speech.googleapis.com/v2/projects/${gcp_translate_project}/locations/${gcp_stt_region}/recognizers/_:recognize"
    local lang_json
    lang_json="$(print -r -- "$langs" | jq -R 'split(",")')"

    local tmp
    tmp="$(mktemp -d)" || return 1
    {
        #: Mono 16 kHz FLAC keeps every piece well under 10 MB and needs no
        #: per-format handling; the cut is on the decoded stream.
        ffmpeg -loglevel error -y -i "$file" -ar 16000 -ac 1 \
            -f segment -segment_time "${gcp_stt_chunk_seconds}" "${tmp}/part-%04d.flac" @RET
        local part
        for part in "${tmp}"/part-*.flac(N) ; do
            #: The audio goes in through a file: a minute of FLAC is too long
            #: for an argument. /usr/bin/base64, because the Homebrew one on
            #: PATH is a different program.
            /usr/bin/base64 -i "$part" | tr -d '\n' > "${part}.b64" @RET
            jq -n --arg model "${gcp_stt_model}" --argjson langs "$lang_json" \
                --rawfile content "${part}.b64" \
                '{config: {model: $model, languageCodes: $langs, autoDecodingConfig: {}}, content: $content}' \
            | command curl -fsS -X POST \
                -H "Authorization: Bearer ${token}" \
                -H "x-goog-user-project: ${gcp_translate_project}" \
                -H 'Content-Type: application/json' \
                --data @- "$url" \
            | jq -r '[.results[]?.alternatives[0]?.transcript // empty] | join(" ")' @RET
        done
    } always {
        command rm -rf "$tmp"
    }
}

function stt-file-chirp3-fa {
    #: Farsi speech; the language is named because detection misses it.
    stt-file-chirp3 --lang fa-IR "$@"
}

function stt-file-chirp3-2en {
    #: Transcribe (any language), then translate the transcript to English.
    stt-file-chirp3 "$@" | 2en-gcp
}

function stt-file-chirp3-fa-2en {
    stt-file-chirp3-fa "$@" | 2en-gcp
}

#: The `-gcp` names are whichever Google model is currently best; today Chirp 3.
aliasfn stt-file-gcp stt-file-chirp3
aliasfn stt-file-fa-gcp stt-file-chirp3-fa
aliasfn stt-file-2en-gcp stt-file-chirp3-2en
aliasfn stt-file-fa-2en-gcp stt-file-chirp3-fa-2en
