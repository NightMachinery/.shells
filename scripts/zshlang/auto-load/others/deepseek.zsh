##
function deepseek-fetch {
    : "usage: deepseek-fetch <url> ...\nFetch public pages as Markdown with the DeepSeek Fetch API."
    local api_key="${deepseek_api_key}"
    local timeout="${deepseek_fetch_timeout:-90}"
    if (( $# == 0 )) ; then
        ecerr 'usage: deepseek-fetch <url> ...'
        return 1
    fi
    if test -z "${api_key}" ; then
        ecerr "$0: deepseek_api_key is not configured"
        return 1
    fi
    ensure-cmd curl jq @RET

    local url payload response content output='' separator=''
    for url in "$@" ; do
        payload="$(command jq --null-input --arg url "${url}" '{url: $url}')" @TRET
        #: Pass authentication over a pipe, keeping the key out of argv and files.
        if response="$(command curl --silent --show-error --fail-with-body \
                --max-time "${timeout}" \
                --header @<(print -rl -- "Authorization: Bearer ${api_key}" 'Content-Type: application/json') \
                --data "${payload}" 'https://api.deepseek.com/v1/tools/fetch')" ; then
            if ! content="$(ec "${response}" | command jq --exit-status --raw-output \
                    '.content | select(type == "string" and length > 0)')" ; then
                ecerr "$0: Fetch returned no usable Markdown for ${url}"
                return 1
            fi
        else
            local retcode=$?
            ecerr "$0: Fetch failed for ${url} (curl exit ${retcode})"
            return "${retcode}"
        fi

        if (( $# > 1 )) ; then
            output+="${separator}URL: ${url}"$'\n\n'"${content}"
            separator=$'\n\n---\n\n'
        else
            output="${content}"
        fi
    done
    ec "${output}"
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
