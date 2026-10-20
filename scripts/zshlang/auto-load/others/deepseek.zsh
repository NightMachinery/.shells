##
function h-deepseek-fetch-one-json {
    local url="${1}"
    local api_key="${deepseek_api_key}"
    local timeout="${deepseek_fetch_timeout:-90}"
    if test -z "${api_key}" ; then
        ecerr "$0: deepseek_api_key is not configured"
        return 1
    fi
    ensure-cmd curl jq @RET

    local payload response
    payload="$(command jq --null-input --arg url "${url}" '{url: $url}')" @TRET
    #: Authentication travels over a pipe, never argv or a persistent file.
    if response="$(command curl --silent --show-error --fail-with-body \
            --max-time "${timeout}" \
            --header @<(print -rl -- "Authorization: Bearer ${api_key}" 'Content-Type: application/json') \
            --data "${payload}" 'https://api.deepseek.com/v1/tools/fetch')" ; then
        if ! ec "${response}" | command jq --exit-status --compact-output --arg url "${url}" '
                select((.title | type == "string") and (.content | type == "string" and length > 0))
                | {url: $url, title, content}' ; then
            ecerr "$0: Fetch returned no usable page for ${url}"
            return 1
        fi
    else
        local retcode=$?
        ecerr "$0: Fetch failed for ${url} (curl exit ${retcode})"
        return "${retcode}"
    fi
}

function deepseek-fetch-json {
    : "usage: deepseek-fetch-json <url> ...
Return one JSON object per URL (JSONL): url, title, content."
    if (( $# == 0 )) ; then
        ecerr 'usage: deepseek-fetch-json <url> ...'
        return 1
    fi
    local url page output='' separator=''
    for url in "$@" ; do
        page="$(h-deepseek-fetch-one-json "${url}")" @RET
        output+="${separator}${page}"
        separator=$'\n'
    done
    ec "${output}"
}

function deepseek-fetch {
    : "usage: deepseek-fetch <url> ...
Fetch Markdown with page titles, source URLs and horizontal rules."
    local pages
    pages="$(deepseek-fetch-json "$@")" @RET
    ec "${pages}" | command jq --slurp --raw-output '
        to_entries | map(
            (.value.title | gsub("[\\r\\n\\t]+"; " ") | sub("^ +"; "") | sub(" +$"; "")) as $title
            | "# " + (if $title == "" then "Source " + ((.key + 1) | tostring) else $title end)
              + "\n\nURL: <" + .value.url + ">\n\n"
              + (.value.content | sub("[\\r\\n]+$"; ""))
        ) | join("\n\n---\n\n")'
}
##
function deepseek-cc {
    (
        export ANTHROPIC_BASE_URL="https://api.deepseek.com/anthropic"
        export ANTHROPIC_AUTH_TOKEN="${deepseek_api_key}"

        export ANTHROPIC_MODEL="deepseek-v4-pro[1m]"
        export ANTHROPIC_DEFAULT_OPUS_MODEL="deepseek-v4-pro[1m]"
        export ANTHROPIC_DEFAULT_SONNET_MODEL="deepseek-v4-pro[1m]"
        export ANTHROPIC_DEFAULT_HAIKU_MODEL="deepseek-v4-flash"

        export CLAUDE_CODE_SUBAGENT_MODEL="deepseek-v4-flash"
        export CLAUDE_CODE_EFFORT_LEVEL="max"

        $proxyenv exec claude "$@"
    )
}
##
