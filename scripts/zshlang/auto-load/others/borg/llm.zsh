##
function h-llm-api-keys-db-execute {
    local db_path="${llm_api_keys_db_path:-$HOME/.borg/llm_api_keys.db}"

    if ! test -f "${db_path}"; then
        ecerr "Error: Database file not found at '${db_path}'"
        return 1
    fi

    command sqlite3 "${db_path}" "$@" @RET
}

function llm-api-keys-list {
    local table_name="${llm_api_keys_table_name:-user_api_keys}"

    local service="${1}"
    if [[ -z "${service}" ]]; then
        ecerr "Usage: llm-api-keys-list SERVICE"
        return 1
    fi

    local service_quoted="${service//\'/\'\'}"
    local query="SELECT api_key FROM \"${table_name}\" WHERE service = '${service_quoted}';"

    h-llm-api-keys-db-execute "${query}" @RET
}

typeset -g GEMINI_API_KEYS=~/.gemini_api_keys
function llm-api-keys-gemini-save {
    local dest="${GEMINI_API_KEYS}"

    llm-api-keys-list gemini |
        rg -Fv -- "${gemini_api_key}" |
        rg -v '^.*Ngg2qdCUVab4YvDIfSnw$' > "${dest}" @RET
    #: excluding corrupt keys

    local count
    count="$(<$dest wc -l | trimsed)" @TRET

    ecgray "Saved ${count} gemini api keys to ${dest}"
}
##
