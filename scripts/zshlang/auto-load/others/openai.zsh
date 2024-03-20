##
typeset -g openai_completion_engine=(openai-chatgpt)
##
function openai-p {
    test -n "${openai_api_key}"
}
##
function openai-cost-by-tokens {
    local model="$1"
    local mode="${2:-input}"
    local token_count="${3}"
    assert-args model mode token_count @RET

    if [[ "${model}" == gpt-4-1106-preview ]] ; then
        model=gpt-4-turbo
    elif [[ "${model}" =~ '^gpt-4(?:-0314|-0613)?$' ]] ; then
        model=gpt-4
    elif [[ "${model}" =~ '^gpt-3.5-turbo(?:-16k)?$' ]] ; then
        model=gpt-3.5-turbo-0125
    fi

    #: [[https://openai.com/pricing][Pricing]]
    #: [[https://www.anthropic.com/api][Claude API \ Anthropic]]
    #: Per 1K tokens
    typeset -A model_costs
    if [[ "${mode}" == input ]] ; then
        model_costs=(
            [gpt-3.5-turbo-1106]=0.0010
            [gpt-3.5-turbo-0125]=0.0005
            [gpt-3.5-turbo-instruct]=0.0015

            [gpt-4]=0.03
            [gpt-4-32k]=0.06
            [gpt-4-turbo]=0.01

            [claude-3-opus]=0.015
        )
    elif [[ "${mode}" == output ]] ; then
        model_costs=(
            [gpt-3.5-turbo-1106]=0.0020
            [gpt-3.5-turbo-0125]=0.0015
            [gpt-3.5-turbo-instruct]=0.0020

            [gpt-4]=0.06
            [gpt-4-32k]=0.12
            [gpt-4-turbo]=0.03

            [claude-3-opus]=0.075
        )
    else
        ecerr "$0: unknown mode: $mode"
        return 1
    fi

    if [[ -n "${model_costs[$model]}" ]]; then
        cost=$(( (${token_count} / 1000.0) * ${model_costs[$model]})) @TRET
        if isDbg ; then
            ecgray "$0: model: $model, mode: $mode, token_count: $token_count, cost_per_1k: ${model_costs[$model]}, total cost: $cost"
        fi

        ec "$cost"
    else
        ecerr "$0: unknown model: $model"
        return 1
    fi
}
##
function openai-models-list {
    llm openai models
    ##
    # curl --silent --fail -X GET \
    #     "https://api.openai.com/v1/models" \
    #     -H "Authorization: Bearer ${openai_api_key}" |
    #     jq '.data[].id' -r
    ##
}
##
function openai-billing-remaining {
    pxa curl -X GET https://api.openai.com/dashboard/billing/credit_grants \
     -H "Content-Type: application/json" \
     -H "Authorization: Bearer ${openai_api_key}"
}

function openai-billing-limits {
    "$proxyenv" revaldbg curl --silent --fail -X GET \
        "https://api.openai.com/dashboard/billing/subscription" \
        -H "Authorization: Bearer ${openai_api_key}" |
        jq .
}

function h-openai-billing-usage {
    "$proxyenv" revaldbg curl --silent --fail -X GET \
        "https://api.openai.com/dashboard/billing/usage?start_date=2023-04-11&end_date=$(gdate '+%Y-%m-%d')" \
        -H "Authorization: Bearer ${openai_api_key}" |
        jq .
}
##
function openai-complete {
    local input
    input="$(in-or-args "${@}")" @RET
    local input_append_p="${openai_iappend_p}"
    local model="${openai_model:-text-davinci-003}"
    if bool "$input_append_p" ; then
        input="${openai_last_input} ${input}"
        model="${openai_last_model}"

        ecgray "$model"$'\n''-----------'$'\n'"$input"
    fi
    typeset -g openai_last_input="${input}"
    typeset -g openai_last_model="${model}"

    local temperature="${openai_temperature:-0}"
    local max_tokens="${openai_max_tokens:-256}"
    local output_path="${openai_output_path:-.choices[0].text}"
    local api_key="${openai_api_key}"
    assert-args api_key @RET

    local opts=()

    local req
    req="$(printf -- "%s " "$input" |
                  jq --raw-input --slurp --null-input --compact-output --arg model "$model" --arg temperature "$temperature" --arg max_tokens "$max_tokens" 'inputs as $i | {"model": $model, "prompt": $i, "temperature": ($temperature|tonumber), "max_tokens": ($max_tokens|tonumber)}')" @TRET

    if isDbg ; then
        ec "$req" | jq .
    else
        opts+=('--silent')
    fi

    local res
    res="$(revaldbg curl --fail-with-body \
        --header 'Content-Type: application/json' \
        --header "Authorization: Bearer ${api_key}" \
        --request POST \
        --data "$req" \
        "${opts[@]}" \
        "https://api.openai.com/v1/completions")" @TRET

    typeset -g openai_last_res="${res}"
    ec "$res" |
        jqm "${output_path}" |
        cat-copy-if-tty
}
@opts-setprefix openai-complete openai

function davinci-text-3 {
    openai_model='text-davinci-003' openai-complete "$@"
}
# @openai-complete davinci-text-3 openai

aliasfn davinci-text davinci-text-3
# aliasfn davinci-text gpt-3.5-turbo
# @openai-complete davinci-text openai

function davinci-code-2 {
    openai_model='code-davinci-002' openai-complete "$@"
}
# @openai-complete davinci-code-2 openai
aliasfn davinci-code davinci-code-2
# @openai-complete davinci-code openai

function davinci-text-i {
    local input
    input="$(in-or-args "$@")" @RET

    pbcopy "$input" @STRUE

    davinci-text "$input" @RET
}

# alias xx='\noglob davinci-text-i'
# alias xz='openai_iappend_p=y \noglob openai-complete'
# alias xc='\noglob davinci-code'

function openai-complete-with-prompt {
    local engine=("${openai_completion_engine[@]}")
    local verbose="${openai_completion_v}"
    if test -z "$verbose" ; then
        if isOutTty ; then
            verbose=y
        fi
    fi

    local prompt
    prompt="$(reval "$@")" @TRET

    if bool "$verbose" ; then
        ec "$(colorfg "$gray[@]" ; Bold)${engine[@]}:$(colorreset)"$'\n'"$prompt"

        ec-sep-h ; ec
    fi

    reval "${engine[@]}" "$prompt"
}
# alias xc='openai-complete-with-prompt'
##
# function chatgpt-postprocess {
#     html2org |
#         sd '^\[+.*' '_______' |
#         cat-copy-if-tty
# }
##
function openai-chatgpt {
    local proxy=()
    if isLocal && ! proxy-active-p ; then
        proxy=(pxa)
    fi

    local input
    input="$(in-or-args "$@")" @RET

    pbcopy "$input" @STRUE

    ec "$input" |
        reval "${proxy[@]}" openai_chatgpt.py |
        cat-copy-if-tty
}

# alias xx='\noglob openai-chatgpt'
##
function openai-token-count {
    local model="${token_count_model:-gpt-4}"

    in-or-args "$@" |
        # $proxyenv reval-ec ttok -m "${model}"
        openai_token_count.py
}
## * Simon's LLM
function llm-logs {
    #: `llm logs list --help | less`
    ##
    local count="${1:-50}" opts=("${@[2,-1]}")
    #: Use count=0 for all chats.

    reval-ec llm logs list --count "${count}" "${opts[@]}" |
        md2org |
        emc-less-org @RET
    emc-eval '(night/org-go-to-last-heading)'
}
aliassafe llml='llm-logs'

function llm-m {
    local model="${llm_model:-gpt-3.5-turbo}"
    local temp="${llm_temp}"
    local system_prompt="${llm_system}"
    local opts=() log=''

    if [[ "$model" == 'continue' ]] ; then
        model="${llm_last_model}"
    else
        typeset -g llm_last_model="$model"
    fi

    opts+=(-m "${model}")
    log+="LLM Model: ${model}"

    if test -n "$temp" ; then
        opts+=(-o temperature "$temp")
        log+=", Temperature: ${temp}"
    fi

    if test -n "${system_prompt}" ; then
        opts+=(--system "${system_prompt}")
        log+=$'\n'"System Prompt:"$'\t'"${system_prompt}"
    fi

    ecgray "$log"

    $proxyenv revaldbg command llm "$@" "${opts[@]}" |
        cat-copy-streaming
}

function llm-continue {
    llm_model=continue llm-m --continue "$*"
    #: This will re-send the prompts and responses for the previous conversation as part of the call to the language model. Note that this can add up quickly in terms of tokens, especially if you are using expensive models.
    #: --continue will automatically use the same model as the conversation that you are continuing, even if you omit the -m/--model option.
}
aliassafe llmc='\noglob llm-continue'
alias xc='\noglob llm-continue'

function llm-send {
    local model="${llm_model:-gpt-3.5-turbo}"
    typeset -g llm_last_model="$model"

    local max_tokens="${llm_max_tokens:-4000}"
    local token_strategy="${llm_token_limit_strategy:-reject}"

    local input
    if (( $#@ >= 1 )) ; then
        input="$*"
    else
        input="$(cat-paste-if-tty)" @RET
    fi

    local token_count input_cost
    token_count="$(ecn "${input}" | token_count_model="${model}" openai-token-count)" @TRET

    if (( ${token_count} > max_tokens )) ; then
        if [[ "${token_strategy}" == 'reject' ]] ; then
            ecerr "$0: Input (${token_strategy} tokens) exceeds the token limit of ${max_tokens} for the model ${model}."
            return 1
        elif [[ "${token_strategy}" == 'truncate' ]] ; then
            input="$(ecn "${input}" | ttok -m "${model}" -t "${max_tokens}")" @TRET
            if bool "${prompt_code_block_p}" ; then
                input+=$'\n''```'$'\n'
            fi

            token_count="$(ecn "${input}" | token_count_model="${model}" openai-token-count)" @TRET
            ecgray "$0: truncated input to ${token_count} tokens"
        else
            ecerr "$0: Unknown token limit strategy: ${token_strategy}"
            return 1
        fi
    fi
    input_cost="$(openai-cost-by-tokens "${model}" input "${token_count}")" @TRET

    ecgray '-----'
    ecgray "${input}"
    ecgray '-----'
    ecgray "Input Tokens: ${token_count}" #  (\$${input_cost})
    ecgray '-----------'
    local output output_token_count output_cost total_cost
    exec {fd_out}<&1
    {
        #: =fd_out= is a copy of stdout, which we will use to stream the output.
        output="$(ecn "${input}" | llm_model="${model}" llm-m >&1 >&${fd_out})" @TRET
    } always {
        exec {fd_out}<&-
    }
    output_token_count="$(ecn "${output}" | token_count_model="${model}" openai-token-count)" @TRET
    output_cost="$(openai-cost-by-tokens "${model}" output "${output_token_count}")" @TRET
    total_cost=$(( ${input_cost} + ${output_cost} )) @TRET
    local n20=$(( 20 / ${total_cost} ))
    local n20_daily=$(( n20 / 30 ))

    {
        colorfg "$gray[@]"

        #: We could prefix this with another '\n', but it makes things ugly.
        printf '-----------\nOutput Tokens: %d\nTotal Cost: $%.3f = $%.3f + $%.3f, $20 Buys: %.1f (Daily: %.1f)\n' ${output_token_count} ${total_cost} ${input_cost} ${output_cost} ${n20} ${n20_daily} |
            numfmt-comma | cat
        #: =numfmt-comma= will copy if its output is a tty.

        resetcolor
    } >&2
}

function llm-3t {
    llm_model=gpt-3.5-turbo-16k llm-send "$@"
}
aliassafe l3='\noglob llm-3t'
function llm-3t-chat {
    llm_model=gpt-3.5-turbo-16k llm-m chat "$@"
}
aliasfn reval-to-gpt3t reval-to llm-3t
aliassafe rl3t='\noglob reval-to-gpt3t'

function llm-4 {
    #: @alt =gpt-4-0314= =gpt-4-0613=
    llm_model=gpt-4 llm-send "$@"
}
aliassafe l4='\noglob llm-4'
function llm-4-chat {
    llm_model=gpt-4 llm-m chat "$@"
}
aliasfn reval-to-gpt4 reval-to llm-4
aliassafe rl4='\noglob reval-to-gpt4'

function llm-4t {
    llm_model=gpt-4-turbo llm-send "$@"
}
aliassafe l4t='\noglob llm-4t'
function llm-4t-chat {
    llm_model=gpt-4-turbo llm-m chat "$@"
}
aliasfn reval-to-gpt4t reval-to llm-4t
aliassafe rl4t='\noglob reval-to-gpt4t'
aliassafe 4t='\noglob reval-to-gpt4t'

function llm-c3o {
    llm_model=claude-3-opus llm-send "$@"
}
aliassafe c3o='\noglob llm-c3o'
function llm-c3o-chat {
    llm_model=claude-3-opus llm-m chat "$@"
}
aliasfn reval-to-c3o reval-to llm-c3o
aliassafe rc3o='\noglob reval-to-c3o'

function llm-c3h {
    llm_model=claude-3-haiku llm-send "$@"
}
aliassafe c3h='\noglob llm-c3h'
function llm-c3h-chat {
    llm_model=claude-3-haiku llm-m chat "$@"
}
aliasfn reval-to-c3h reval-to llm-c3h
aliassafe rc3h='\noglob reval-to-c3h'

# alias xx='\noglob llm-4'

# aliassafe xx='\noglob llm-4t'
# aliassafe xz='\noglob reval-to-gpt4t'

aliassafe xx='\noglob llm-c3o'
aliassafe xz='\noglob reval-to-c3o'
#: @nameConflict xz, unxz, xzcat, lzma, unlzma, lzcat - Compress or decompress .xz and .lzma  files
##
function sgpt-m {
    OPENAI_API_KEY="${openai_api_key}" $proxyenv reval-ecgray command sgpt --temperature 0 "$@"
    # --stream
}

function sgpt-gpt4v {
    sgpt-m --max-tokens 40000 -m "gpt-4-vision-preview" "$@"
}
alias xv='sgpt-gpt4v'
##
