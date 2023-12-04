##
typeset -g openai_completion_engine=(openai-chatgpt)
##
function openai-p {
    test -n "${openai_api_key}"
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
    local model="gpt-4"

    in-or-args "$@" |
        reval-ec ttok -m "${model}"
        # openai_token_count.py
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
alias llmc='\noglob llm-continue'
alias xc='\noglob llm-continue'

function llm-3t {
    llm_model=gpt-3.5-turbo llm-m "$*"
}
alias l3='\noglob llm-3t'
function llm-3t-chat {
    llm_model=gpt-3.5-turbo llm-m chat "$@"
}

function llm-4 {
    #: @alt =gpt-4-0314= =gpt-4-0613=
    llm_model=gpt-4 llm-m "$*"
}
alias l4='\noglob llm-4'
# alias xx='\noglob llm-4'
function llm-4-chat {
    llm_model=gpt-4 llm-m chat "$@"
}

function llm-4t {
    llm_model=gpt-4-turbo llm-m "$*"
}
alias l4t='\noglob llm-4t'
alias xx='\noglob llm-4t'
function llm-4t-chat {
    llm_model=gpt-4-turbo llm-m chat "$@"
}
##
