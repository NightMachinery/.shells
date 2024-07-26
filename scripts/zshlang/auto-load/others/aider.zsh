##
function aider-m {
    local model="${aider_model}"
    local opts=(
        --map-tokens=0
        #: Max number of tokens to use for repo map, use 0 to  disable (default: 1024) [env var: AIDER_MAP_TOKENS]
    )

    if git-clean-p ; then
        # The repository is clean

        local -x OPENROUTER_API_KEY="${openrouter_api_key}"
        local -x OPENAI_API_KEY="${openai_api_key}"
        #: Some models below replace OPENAI_API_KEY.
        #: This is bad, as aider uses OpenAI for its voice mode, too.

        if [[ "$model" == c3o ]] ; then
            opts+=(
                --model
                openrouter/anthropic/claude-3-opus:beta

                --edit-format diff
            )
            ##
            # local -x OPENAI_API_KEY="${openrouter_api_key}"
            # local -x OPENAI_API_BASE=https://openrouter.ai/api/v1

            # opts+=(
            #     --model
            #     # anthropic/claude-3-opus
            #     anthropic/claude-3-opus:beta

            #     --edit-format diff
            # )
            ##

        elif [[ "$model" == s3 ]] ; then
            opts+=(
                --model
                # openrouter/anthropic/claude-3.5-sonnet:beta
                #: The beta variant is not recognized by aider.
                openrouter/anthropic/claude-3.5-sonnet

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
        fi

        $proxyenv reval-ecgray command aider "${opts[@]}" "$@"
    else
        ecerr "$0: Repository is dirty"
        return 1
    fi
}
aliasfn aider aider-m
aliasfn aider-4 aider_model=gpt-4 aider-m
aliasfn aider-4t aider_model=gpt-4-turbo aider-m
aliasfn aider-s3 aider_model=s3 aider-m
aliasfn aider-c3o aider_model=c3o aider-m
aliasfn aider-g1.5 aider_model=g1.5 aider-m
aliasfn aider-l3 aider_model=gq-llama3 aider-m
##
