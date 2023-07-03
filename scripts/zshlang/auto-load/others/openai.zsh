##
typeset -g openai_completion_engine=(openai-chatgpt)
##
function openai-p {
    test -n "${openai_api_key}"
}
##
function openai-models-list {
    curl --silent --fail -X GET \
        "https://api.openai.com/v1/models" \
        -H "Authorization: Bearer ${openai_api_key}" |
        jq '.data[].id' -r
}
##
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
alias xc='openai-complete-with-prompt'
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

alias xx='\noglob openai-chatgpt'
##
function openai-token-count {
    in-or-args "$@" |
        openai_token_count.py
}
##
