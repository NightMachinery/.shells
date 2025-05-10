##
# Gets audio URL, title, and song ID for a Suno share URL.
# Outputs the audio URL on the first line, the cleaned title on the second line,
# and the song ID on the third line.
function suno-audio-get-with-title {
    setopt localoptions errreturn
    local url="${1}"
    if [[ -z "$url" ]]; then
        ecerr "Usage: suno-audio-get-with-title <suno_share_url>"
        return 1
    fi

    local json_output
    # Resolve URL and get title via JSON endpoint
    json_output="$(reval-memoi url-final3-json "$url")" @RET
    if [[ -z "$json_output" ]]; then
        ecerr "Error: url-final3-json failed or returned empty for URL: $url"
        return 1
    fi

    local final_url title
    # Parse JSON using jq; use -e to fail if keys are missing or null
    final_url="$(ec "$json_output" | jq -re '.url')" @RET
    title="$(ec "$json_output" | jq -re '.title')" @RET

    # Clean title: Remove " | Suno" suffix
    title="${title% | Suno}"

    # Extract song ID from the resolved final_url
    local pattern='\/song\/([a-f0-9-]+)(?:\?|$)'
    local song_id
    song_id="$(ec "$final_url" | rget "$pattern")" || true
    if [[ -z "$song_id" ]]; then
        ecerr "Error: Could not extract song ID from final URL: $final_url"
        return 1
    fi

    # Construct the direct audio download URL
    local audio_url="https://cdn1.suno.ai/${song_id}.mp3"

    # Output audio URL, then the title, then the song ID on new lines
    ec "$audio_url"
    ec "$title"
    ec "$song_id"
}


# Downloads Suno audio, naming files based on title and ID.
# Processes URLs sequentially and passes download info to aa-gateway via stdin.
function suno-dl-v2 {
    setopt localoptions errreturn
    local inargs
    # Get input URLs from arguments or standard input
    in-or-args3 "$@" @RET
    if (( ${#inargs} == 0 )); then
        ecerr "Usage: suno-dl-v2 <suno_share_url>..."
        ecerr "       <command> | suno-dl-v2"
        return 1
    fi

    local aria2_input_str="" # String to hold input for aria2c -i -
    local url output info_lines audio_url title song_id sanitized_title filename
    local processed_count=0 # Count successfully processed URLs

    for url in "${inargs[@]}"; do
        # Get audio URL, title, and song ID
        output="$(suno-audio-get-with-title "$url")"
        # Check if the command failed or returned empty output
        if (( $? != 0 )) || [[ -z "$output" ]]; then
            ecerr "Warning: Failed to get information for URL: $url. Skipping."
            continue
        fi
        # var-show output

        # Split output into lines
        info_lines=("${(@f)output}")


        # Ensure we got exactly three lines
        if (( ${#info_lines} != 3 )); then
            ecerr "Warning: Expected 3 lines (URL, Title, ID) from suno-audio-get-with-title for '$url', but got ${#info_lines}. Output: $output. Skipping."
            continue
        fi

        audio_url="${info_lines[1]}"
        title="${info_lines[2]}"
        song_id="${info_lines[3]}"

        # Basic validation on URL format (crude check)
        if [[ "$audio_url" != "https://cdn1.suno.ai/${song_id}.mp3" ]]; then
             ecerr "Warning: Mismatch between expected audio URL format and received URL/ID for '$url':"
             ecerr "  URL: '$audio_url'"
             ecerr "  ID:  '$song_id'"
             ecerr "Skipping."
             continue
        fi
        # Basic validation on song_id format
        if ! [[ "$song_id" =~ ^[a-f0-9-]+$ ]]; then
            ecerr "Warning: Invalid song ID format received for '$url': '$song_id'. Skipping."
            continue
        fi

        # Sanitize title for filename usage using str2filename
        sanitized_title="$(str2filename "$title")" @TRET
        # Use a default if sanitization results in an empty string
        if [[ -z "$sanitized_title" ]]; then
            sanitized_title="untitled"
        fi

        # Construct filename using sanitized title and song ID
        filename="${sanitized_title}_${song_id}.mp3"

        # Append the URL and the 'out=filename' option to the input string
        # Ensure the 'out=' line is indented with at least one space [3, 6]
        aria2_input_str+="${audio_url}"$'\n'" out=${filename}"$'\n'

        processed_count=$(( processed_count + 1 ))
    done

    # Check if any URLs were successfully processed
    if (( processed_count == 0 )); then
        ecerr "Error: No valid URLs could be processed or none resulted in downloadable information."
        return 1
    fi

    # Optional: Show the input string being passed to aa-gateway (if debugging)
    ecgray "Aria2 Input File:"$'\n'"${aria2_input_str}"

    # Download all files using aa-gateway, passing the formatted input via stdin.
    # Assuming aa-gateway passes stdin to aria2c's -i - (or equivalent).
    # We removed the -Z and command-line URL/file arguments.
    local input_file
    input_file="$(gmktemp)" @TRET
    ec "$aria2_input_str" > "$input_file" || return 1
    assert reval-ecgray aa-gateway --input-file="${input_file}" @RET
}
aliasfn suno-dl suno-dl-v2
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

function suno-dl-v1 {
    local inargs
    in-or-args3 "$@" @RET

    dact var-show inargs

    local audio_urls
    # Get the direct audio URL using the helper function
    audio_urls=(${(@f)"$(suno-audio-get "${inargs[@]}")"}) @RET

    dact var-show audio_urls

    # Download the audio file using aa-gateway
    assert reval-ecgray aa-gateway -Z "${audio_urls[@]}" @RET
}
##
