##
function pioneer-used-today {
    local -x PIONEER_API_KEY="${PIONEER_API_KEY:-${pioneer_api_key}}"
    if [[ -z "${PIONEER_API_KEY}" ]] ; then
        ecerr "pioneer-used-today: no API key (set \$PIONEER_API_KEY or \$pioneer_api_key)"
        return 2
    fi

    # Python owns pagination, UTC cutoff, and output formatting.
    $proxyenv command python3 "${HOME}/scripts/python/pioneer_used_today.py" "$@"
}

function pioneer-models {
    local -x PIONEER_API_KEY="${PIONEER_API_KEY:-${pioneer_api_key}}"
    if [[ -z "${PIONEER_API_KEY}" ]] ; then
        ecerr "pioneer-used-today: no API key (set \$PIONEER_API_KEY or \$pioneer_api_key)"
        return 2
    fi

    curl https://api.pioneer.ai/base-models \
        -H "X-API-Key: ${PIONEER_API_KEY}" | cat-copy-if-tty
}
##
