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
            [gpt-4o]=0.005

            [${claude_3_5_sonnet_model_name}]=0.003
            [claude-3-opus]=0.015
            [${claude_3_opus_model}]=0.015
        )
    elif [[ "${mode}" == output ]] ; then
        model_costs=(
            [gpt-3.5-turbo-1106]=0.0020
            [gpt-3.5-turbo-0125]=0.0015
            [gpt-3.5-turbo-instruct]=0.0020

            [gpt-4]=0.06
            [gpt-4-32k]=0.12
            [gpt-4-turbo]=0.03
            [gpt-4o]=0.015

            [${claude_3_5_sonnet_model_name}]=0.015
            [claude-3-opus]=0.075
            [${claude_3_opus_model}]=0.075
        )
    else
        # ecerr "$0: unknown mode: $mode"
        # return 1
        return 0
    fi

    if [[ -n "${model_costs[$model]}" ]]; then
        cost=$(( (${token_count} / 1000.0) * ${model_costs[$model]})) @TRET
        if isDbg ; then
            ecgray "$0: model: $model, mode: $mode, token_count: $token_count, cost_per_1k: ${model_costs[$model]}, total cost: $cost"
        fi

        ec "$cost"
    else
        # ecerr "$0: unknown model: $model"
        # return 1
        return 0
    fi
}
##
function llm-models-list {
    llm models
}

function llm-openai-models-list {
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

    if [[ "${model}" == "${claude_3_5_sonnet_model_name}" ]] ; then
        # model=''
    fi

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
    bella_zsh_disable1

    ensure-array llm_attachments @RET
    local attachments=("${llm_attachments[@]}")
    llm_attachments=() #: clear the attachments

    local model="${llm_model:-${llm_default_model}}"
    local temp="${llm_temp}"
    local system_prompt="${llm_system}"
    local opts=() log=''

    if [[ "$model" == 'continue' ]] ; then
        model="${llm_last_model}"
    else
        typeset -g llm_last_model="$model"
    fi

    ##
    local proxy_p="${llm_models_proxy_p[${model}]:-y}"
    # dact var-show proxy_p model llm_models_proxy_p
    if bool "${proxy_p}" ; then
        if should-proxy-p ; then
            pxa-local

        else
            ecdbg "$0: proxy not wanted or already active"
        fi

        log+="HTTP_PROXY: ${HTTP_PROXY}"$'\n'
    fi
    ##

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

    local a
    for a in "${attachments[@]}" ; do
        if [[ "${a}" == "MAGIC_CLIPBOARD" ]] ; then
            # Save the image from the clipboard to a temp file using `pbpaste-image`:
            a="$(gmktemp --suffix=".png")" @TRET
            assert pbpaste-image "${a}" @RET

            icat-v "${a}" || true
        fi
        
        opts+=(--attachment "$a")
        # log+=$'\n'"Attached:"$'\t'"${a}"
    done

    ecgray "$log"

    revaldbg command llm "$@" "${opts[@]}" |
        cat-rtl-streaming-if-tty
    # cat-copy-streaming
}
alias with-llm-attach-clipboard='llm_attachments=("MAGIC_CLIPBOARD") '

function llm-continue {
    llm_model=continue llm-m --continue "$*"
    #: This will re-send the prompts and responses for the previous conversation as part of the call to the language model. Note that this can add up quickly in terms of tokens, especially if you are using expensive models.
    #: --continue will automatically use the same model as the conversation that you are continuing, even if you omit the -m/--model option.
}
aliassafe llmc='\noglob llm-continue'
alias xc='\noglob llm-continue'

function llm-send {
    bella_zsh_disable1

    local model="${llm_model:-${llm_default_model}}"
    assert-args model @RET
    typeset -g llm_last_model="$model"

    local copy_p="${llm_copy_p:-y}"
    local max_tokens="${llm_max_tokens:-20000}"
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
    ecgray "${input}" |& cat-rtl-if-tty >&2
    ecgray '-----'
    ecgray "Input Tokens: ${token_count}" #  (\$${input_cost})
    ecgray '-----------'
    local output output_token_count output_cost total_cost
    exec {fd_out}<&1
    {
        #: =fd_out= is a copy of stdout, which we will use to stream the output.
        output="$(ecn "${input}" | llm_model="${model}" llm-m >&1 >&${fd_out})" @TRET
        #: `> >(cat-rtl-streaming-if-tty >&${fd_out})` is no good as it needs to wait for the whole line to finish to output, while the default streamer outputs token by token. So we need to explicitly opt in for RTL manually when we need it.
    } always {
        exec {fd_out}<&-
    }

    if bool "${copy_p}" ; then
        pbcopy "${output}" || true
    fi

    output_token_count="$(ecn "${output}" | token_count_model="${model}" openai-token-count)" @TRET
    output_cost="$(openai-cost-by-tokens "${model}" output "${output_token_count}")" @TRET
    if test -n "${output_cost}" && test -n "${input_cost}" ; then
        total_cost=$(( ${input_cost} + ${output_cost} )) @TRET

        local n20
        n20=$(( 20 / ${total_cost} )) @TRET
        local n20_daily=$(( n20 / 30 ))

        {
            colorfg "$gray[@]"

            #: We could prefix this with another '\n', but it makes things ugly.
            printf '-----------\nOutput Tokens: %d\nTotal Cost: $%.3f = $%.3f + $%.3f, $20 Buys: %.1f (Daily: %.1f)\n' ${output_token_count} ${total_cost} ${input_cost} ${output_cost} ${n20} ${n20_daily} |
                numfmt-comma | cat
            #: =numfmt-comma= will copy if its output is a tty.

            resetcolor
        } >&2
    fi
}

function reval-to-llm {
    local to
    to='llm-send'
    # to='revaldbg llm-send'

    reval-to "${to}" "$@"
}
aliasfn llm-chat llm-m chat

#: define a dict for keeping `proxy_p` per model:
typeset -gA llm_models_proxy_p

function define-llm-model {
    local model_name="$1"
    local long_name="${long_name}"
    local short_name="${short_name:-${long_name}}"
    assert-args model_name long_name short_name @RET

    ensure-array reval_to_aliases send_aliases
    local reval_to_aliases=("${reval_to_aliases[@]}")
    local send_aliases=("${send_aliases[@]}")

    local model_var_name="${long_name}_model"
    revaldbg typeset -g "${model_var_name}=${model_name}" @RET

    local proxy_p="${proxy_p}"
    if test -n "${proxy_p}" ; then
        llm_models_proxy_p[${model_name}]="${proxy_p}"
    fi

    local with_alias_name="with-${short_name}"
    revaldbg aliassafe "${with_alias_name}=llm_model=\"\${${model_var_name}}\" " @RET

    local with_maybe_alias_name="with-${short_name}-maybe"
    revaldbg aliassafe "${with_maybe_alias_name}=llm_model=\"\${llm_model:-\${${model_var_name}}}\" " @RET

    local llm_function_name="llm-${short_name}"
    revaldbg aliasfn-ng "${llm_function_name}" "${with_alias_name}" llm-send @RET

    local alias_
    for alias_ in "${send_aliases[@]}"; do
        revaldbg aliassafe "${alias_}"="${llm_function_name}" @RET
    done

    local llm_chat_function_name="${llm_function_name}-chat"
    revaldbg aliasfn "${llm_chat_function_name}" "${with_alias_name}" llm-chat @RET

    local reval_to_function_name="reval-to-${short_name}"
    revaldbg aliasfn-ng "${reval_to_function_name}" "${with_alias_name}" reval-to-llm @RET

    for alias_ in "${reval_to_aliases[@]}"; do
        revaldbg aliassafe "${alias_}"="${reval_to_function_name}" @RET
    done
}

function define-llm-model-v2 {
    local map_name="$1"

    #: Extract values from the hash map using indirect parameter expansion
    local model_name="${${(P)map_name}[model_name]}"
    local long_name="${${(P)map_name}[long_name]}"
    local short_name="${${(P)map_name}[short_name]}"
    local proxy_p="${${(P)map_name}[proxy_p]}"
    local reval_to_aliases=(${(s: :)${(P)map_name}[reval_to_aliases]})
    local send_aliases=(${(s: :)${(P)map_name}[send_aliases]})

    define-llm-model "$model_name"
}
## * Models
## ** Google Gemini
typeset -gA gemini25_obj=(
    ##
    # model_name 'gemini-2.5-pro-preview-03-25'
    model_name 'gemini-2.5-pro-exp-03-25'
    ##
    # model_name 'openrouter/google/gemini-2.5-pro-exp-03-25:free'
    # model_name 'openrouter/google/gemini-2.5-pro-preview-03-25'
    ##
    long_name 'gemini_2_5_pro'
    short_name 'g25'
    reval_to_aliases 'g25 gemini'
    send_aliases 'lg25'
)
define-llm-model-v2 gemini25_obj

typeset -gA gemini15_obj=(
    model_name 'gemini-1.5-pro-latest'
    long_name 'gemini_1_5_pro'
    short_name 'g15'
    reval_to_aliases 'g15'
    send_aliases 'lg15'
)
define-llm-model-v2 gemini15_obj

typeset -A gemini_flash_8b_obj=(
    model_name 'gemini-1.5-flash-8b-latest'
    long_name 'gemini_flash_8b_1_5'
    short_name 'flash-8b'
    reval_to_aliases 'rflash_8b fl8'
    send_aliases 'lflash_8b'
)
define-llm-model-v2 gemini_flash_8b_obj

typeset -A gemini_flash_obj=(
    model_name 'gemini-1.5-flash-latest'
    long_name 'gemini_flash_1_5'
    short_name 'flash'
    reval_to_aliases 'rflash fl'
    send_aliases 'lflash'
)
define-llm-model-v2 gemini_flash_obj
## **** Google Gemini 2
# 2.5 Flash
typeset -A gemini25_flash_obj=(
    model_name 'gemini-2.5-flash'
    # model_name 'gemini-2.5-flash-preview-04-17'
    long_name 'gemini_flash_2_5'
    short_name 'flash25'
    reval_to_aliases 'rflash25 fl25'
    send_aliases 'lflash25'
)
define-llm-model-v2 gemini25_flash_obj

# Gemini 2.0 Flash
typeset -A gemini2_flash_obj=(
    model_name 'gemini-2.0-flash'
    # model_name 'gemini-2.0-flash-exp'
    long_name 'gemini_flash_2_0'
    short_name 'flash2'
    reval_to_aliases 'rflash2 fl2'
    send_aliases 'lflash2'
)
define-llm-model-v2 gemini2_flash_obj

typeset -A or_gemini2_flash_obj=(
    model_name 'openrouter/google/gemini-2.0-flash-001'
    long_name 'or_gemini_flash_2_0'
    short_name 'or_flash2'
    reval_to_aliases 'r-or-flash2 or-fl2'
    send_aliases 'l-or-flash2'
)
define-llm-model-v2 or_gemini2_flash_obj

# Gemini 2.0 Flash Thinking Mode
typeset -A gemini2_flash_thinking_obj=(
    # model_name 'gemini-2.0-flash-thinking-exp-1219'
    model_name 'gemini-2.0-flash-thinking-exp-01-21'
    long_name 'gemini_flash_thinking_2_0'
    short_name 'flash2t'
    reval_to_aliases 'rflash2t fl2t'
    send_aliases 'lflash2t'
)
define-llm-model-v2 gemini2_flash_thinking_obj
## *** OpenRouter: Google Gemini
# OpenRouter Gemini Flash 8B
typeset -A openrouter_gemini_flash_8b_obj=(
    model_name 'openrouter/google/gemini-flash-1.5-8b'
    long_name 'openrouter_gemini_flash_8b'
    short_name 'or-flash-8b'
    reval_to_aliases 'or_rflash_8b orfl8'
    send_aliases 'or_lflash_8b'
)
define-llm-model-v2 openrouter_gemini_flash_8b_obj

# OpenRouter Gemini Flash 8B Experimental
typeset -A openrouter_gemini_flash_8b_exp_obj=(
    model_name 'openrouter/google/gemini-flash-1.5-8b-exp'
    long_name 'openrouter_gemini_flash_8b_exp'
    short_name 'or-flash-8b-exp'
    reval_to_aliases 'or_rflash_8b_exp orfl8e'
    send_aliases 'or_lflash_8b_exp'
)
define-llm-model-v2 openrouter_gemini_flash_8b_exp_obj

# OpenRouter Gemini 1.5
typeset -A openrouter_gemini_15_obj=(
    model_name 'openrouter/google/gemini-pro-1.5'
    long_name 'openrouter_gemini_15'
    short_name 'or-g15'
    reval_to_aliases 'or_rg15 org15'
    send_aliases 'or_lg15'
)
define-llm-model-v2 openrouter_gemini_15_obj

# OpenRouter Gemini 1.5 Experimental
typeset -A openrouter_gemini_15_exp_obj=(
    model_name 'openrouter/google/gemini-pro-1.5-exp'
    long_name 'openrouter_gemini_15_exp'
    short_name 'or-g15-exp'
    reval_to_aliases 'or_rg15_exp org15e'
    send_aliases 'or_lg15_exp'
)

## ** Misc Models
typeset -gA gpt35t16k_obj=(
    model_name 'gpt-3.5-turbo-16k'
    long_name 'gpt_3_5_16k'
    short_name '3t'
    reval_to_aliases 'r3t'
    send_aliases '3t'
)
define-llm-model-v2 gpt35t16k_obj

typeset -gA gpt4_obj=(
    model_name 'gpt-4'
    long_name 'gpt_4'
    short_name '4'
    reval_to_aliases 'rl4'
    send_aliases 'l4'
)
define-llm-model-v2 gpt4_obj

typeset -gA gpt4turbo_obj=(
    model_name 'gpt-4-turbo'
    long_name 'gpt_4_turbo'
    short_name '4t'
    reval_to_aliases 'rl4t 4t'
    send_aliases 'l4t'
)
define-llm-model-v2 gpt4turbo_obj

typeset -gA gpt4o_obj=(
    model_name 'gpt-4o'
    long_name 'gpt_4o'
    short_name '4o'
    reval_to_aliases 'rl4o 4o'
    send_aliases 'l4o'
)
define-llm-model-v2 gpt4o_obj

typeset -gA gpt4o_audio_obj=(
    model_name 'gpt-4o-audio-preview'
    long_name 'gpt_4o_audio'
    short_name '4o-audio'
    # short_name '4o-audio'
    # reval_to_aliases 'rl4o 4o'
    # send_aliases 'l4o'
)
define-llm-model-v2 gpt4o_audio_obj

typeset -gA gpt4omini_obj=(
    model_name 'gpt-4o-mini'
    long_name 'gpt_4o_mini'
    short_name '4om'
    reval_to_aliases 'rl4om 4om'
    send_aliases 'l4om'
)
define-llm-model-v2 gpt4omini_obj

typeset -gA claude35sonnet_obj=(
    model_name 'or:c35s'
    long_name 'claude_3_5_sonnet'
    short_name 's3'
    reval_to_aliases 'rs3'
    send_aliases 's3'
)
define-llm-model-v2 claude35sonnet_obj
typeset -g claude_3_5_sonnet_model_name="${claude_3_5_sonnet_model}" #: @backcompat

typeset -gA claude3o_obj=(
    model_name 'or:c3o'
    long_name 'claude_3_opus'
    short_name 'c3o'
    reval_to_aliases 'rc3o'
    send_aliases 'c3o'
)
define-llm-model-v2 claude3o_obj

typeset -gA claude3haiku_obj=(
    model_name 'claude-3-haiku'
    long_name 'claude_3_haiku'
    short_name 'c3h'
    reval_to_aliases 'rc3h'
    send_aliases 'c3h'
)
define-llm-model-v2 claude3haiku_obj

typeset -gA llama3_obj=(
    model_name 'gq-llama3'
    long_name 'llama3'
    short_name 'l3'
    reval_to_aliases 'rl3'
    send_aliases 'l3'
)
define-llm-model-v2 llama3_obj
##
## * Default Models
typeset -g llm_default_model="${gemini_flash_2_5_model}"
# typeset -g llm_default_model="${gemini_flash_thinking_2_0_model}"
# typeset -g llm_default_model="${gemini_2_5_pro_model}"
# typeset -g llm_default_model="${claude_3_5_sonnet_model_name}"

aliassafe xx='\noglob llm-send'
aliassafe llm-run='\noglob reval-to-llm'
aliassafe xz='\noglob llm-run'
alias xzz='with-llm-attach-clipboard g15'
#: @bug/upstream [[id:f96f4512-7ecc-4bff-8ebb-dfc8d46979d4][OpenRouter Claude does not support images · Issue #602 · simonw/llm]]

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
