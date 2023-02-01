##
function openai-complete {
    local input
    input="$(in-or-args "${@}")" @RET
    local model="${openai_model:-text-davinci-003}"
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
        jqm "${output_path}"
}
@opts-setprefix openai-complete openai

function davinci-text-3 {
    openai_model='text-davinci-003' openai-complete "$@"
}
# @openai-complete davinci-text-3 openai
aliasfn davinci-text davinci-text-3
# @openai-complete davinci-text openai

function davinci-code-2 {
    openai_model='code-davinci-002' openai-complete "$@"
}
# @openai-complete davinci-code-2 openai
aliasfn davinci-code davinci-code-2
# @openai-complete davinci-code openai

alias xx='\noglob davinci-text'
alias xc='\noglob davinci-code'
##
function chatgpt-postprocess {
    html2org |
        sd '^\[+.*' '_______' |
        cat-copy-if-tty
}
##
