## Loaded function catalog, with literal AND terms and shortest-name fallback.
typeset -g omni_prompter_catalog_version=1.1.0

function omni-prompter-functions {
    local query="$*" function_name term matched length
    local -a terms
    terms=(${=query})
    {
        for function_name in ${(k)functions}; do
            case "$function_name" in
                prompt-input-file|prompt-instruction-input|prompt-instruction-input-*|prompt-blockify|\
                snippet-input|snippet-input-file|snippet-chain|snippet-chain-copy|snippet-preamble-gen1|\
                fragment-from-snippet) continue ;;
            esac
            case "$function_name" in
                fragment-*|prompt-*|metaprompt-*|snippet-*) ;;
                *) continue ;;
            esac
            matched=y
            for term in "${terms[@]}"; do
                if [[ "${function_name:l}" != *"${term:l}"* ]]; then
                    matched=n
                    break
                fi
            done
            if [[ "$matched" == y ]]; then
                builtin printf '%08d\t%s\n' "${#function_name}" "$function_name"
            fi
        done
    } | LC_ALL=C command sort -f | while IFS=$'\t' read -r length function_name; do
        builtin print -r -- "$function_name"
    done
}
