##
function h-suno-audio-get {
    local url="${1}"
    if [[ -z "$url" ]]; then
        ecerr "Usage: suno-audio-get <suno_share_url>"
        return 1
    fi

    local final_url
    # Resolve potential redirects to get the canonical song URL
    final_url="$(url-final3 "$url")" || true
    if [[ -z "$final_url" ]]; then
        ecerr "Error: url-final3 returned an empty string for URL: $url"
        return 1
    fi

    # Pattern to extract the UUID from URLs like:
    # https://suno.com/song/94f21977-95f9-48f0-a02f-eac9f86bd901?sh=...
    # https://suno.com/song/94f21977-95f9-48f0-a02f-eac9f86bd901
    # The UUID consists of hex characters and hyphens.
    # We capture the part after "/song/" until the next "?" or end of string.
    local pattern='\/song\/([a-f0-9-]+)(?:\?|$)'
    local song_id
    # Use rget with default replace '$1' to extract the captured group (the UUID)
    song_id="$(ec "$final_url" | rget "$pattern")" || true
    if [[ -z "$song_id" ]]; then
        ecerr "Error: Could not extract song ID from final URL: $final_url"
        return 1
    fi

    # Construct the direct audio download URL
    local audio_url="https://cdn1.suno.ai/${song_id}.mp3"
    ec "$audio_url" |
        cat-copy-if-tty
}

function suno-audio-get {
    in-or-args "$@" |
        parallel_jobs=4 parallelm h-suno-audio-get |
        cat-copy-if-tty
}

function suno-dl {
    local url="${1}"
    if [[ -z "$url" ]]; then
        ecerr "Usage: suno-dl <suno_share_url>"
        return 1
    fi

    local audio_url
    # Get the direct audio URL using the helper function
    audio_url="$(suno-audio-get "$url")" @RET

    # Download the audio file using aa-gateway
    assert aa-gateway "${audio_url}" @RET
}
##
