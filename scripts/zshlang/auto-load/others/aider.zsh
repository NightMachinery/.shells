##
function aider-m {
    local model="${aider_model}"
    local opts=()

    if git diff-index --quiet HEAD -- ; then
        # The repository is clean

        if [[ "$model" == c3o ]] ; then
            local -x OPENAI_API_KEY="${openrouter_api_key}"
            local -x OPENAI_API_BASE=https://openrouter.ai/api/v1

            opts+=(
                --model
                # anthropic/claude-3-opus
                anthropic/claude-3-opus:beta

                --edit-format diff
            )

        elif [[ "$model" == g1.5 ]] ; then
            local -x OPENAI_API_KEY="${openrouter_api_key}"
            local -x OPENAI_API_BASE=https://openrouter.ai/api/v1

            opts+=(
                --model
                google/gemini-pro-1.5

                # --edit-format diff
            )

        elif [[ "$model" == gq-llama3 ]] ; then
            local -x GROQ_API_KEY="${groq_api_key}"

            opts+=(
                --model
                groq/llama3-70b-8192

                # --edit-format diff
            )

        else
            local -x OPENAI_API_KEY="${openai_api_key}"
        fi

        $proxyenv reval-ecgray command aider "${opts[@]}" "$@"
    else
        ecerr "$0: Repository is dirty"
        return 1
    fi
}
aliasfn aider aider-m
aliasfn aider-c3o aider_model=c3o aider-m
aliasfn aider-g1.5 aider_model=g1.5 aider-m
aliasfn aider-l3 aider_model=gq-llama3 aider-m
##
