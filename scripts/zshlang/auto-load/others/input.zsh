function vared-py {
    #: `vared.py [prompt default_value]`
    ##: @broken @deprecated vared-py doesn't work well.
    # vared.py "$@" # </dev/tty # incompatible with prompt_toolkit
    ##
    vared-m "$@"
    ##
}

function read-input {
    #: @usage `[prompt default_value]`
    #: This is an alternative to =vared= that supports an initial default value.
    readline_prompt "$@"
}
aliasfn vared-m read-input
##
function ask {
    doc 'This is a general-purpose function to ask Yes/No questions in Bash, either with or without a default answer. It keeps repeating the question until it gets a valid answer.'
    : "usage: prompt [default]"

    # forked from https://gist.github.com/davejamesmiller/1965569
    local prompt default reply

    if isBrish && isMe ; then
        tts-glados1-cached "Brish is asking you a question"
    fi

    if [ "${2:l}" = "y" ]; then
        prompt="Y/n"
        default=Y
    elif [ "${2:l}" = "n" ]; then
        prompt="y/N"
        default=N
    else
        prompt="y/n"
        default=
    fi

    while true; do

        # Ask the question (not using "read -p" as it uses stderr not stdout)
        ecn "$1 [$prompt] " >/dev/tty

        # Read the answer (use /dev/tty in case stdin is redirected from somewhere else)
        read reply </dev/tty

        # Default?
        if [ -z "$reply" ]; then
            reply=$default
        fi

        # Check if the reply is valid
        case "$reply" in
            Y*|y*) return 0 ;;
            N*|n*) return 1 ;;
        esac

    done
}
##
function friction-type {
    bella_zsh_disable1

    local input_string="$1"
    local retries_count=-1

    local user_input=""
    local start_time
    local end_time
    local time_taken

    #: If no string is provided, generate one using passgen-words
    if [[ -z "${input_string}" ]]; then
        input_string="$(passgen-words 3 | tr '-' " ")" @TRET
    fi


    #: Print the string with zero-width spaces between each character
    #: @bug? `f​i​t​o​ ​` is displayed as `f to` in Kitty.
    local output_string=""
    for (( i=0; i<${#input_string}; i++ )); do
        output_string+="${input_string:$i:1}"
        if (( i < ${#input_string} - 1 )); then
            output_string+=$'\u200b'
        fi
    done

    while true; do
        ec "Type the following text:"
        ec "${output_string}"$'\n'

        start_time="${EPOCHSECONDS}"
        user_input="$(vared-m "> " "${user_input}")" @RET
        end_time="${EPOCHSECONDS}"
        time_taken="$((end_time - start_time))"

        ec
        #: Check if the input matches

        if [[ "${user_input}" == "${input_string}" ]]; then
            ec "Correct! Time taken: ${time_taken} seconds"
            return 0
        else
            ec "Incorrect. Differences shown below:"
            local tmpdir
            tmpdir="$(mktemp -d)" @RET
            ec "${input_string}" > "${tmpdir}/expected"
            ec "${user_input}" > "${tmpdir}/actual"
            git diff --no-index --color=always --no-prefix --minimal --word-diff --word-diff-regex='.' "${tmpdir}/expected" "${tmpdir}/actual" | tail -n1
            ec ""
            silent trs-rm "${tmpdir}"

            if (( retries_count > 0 )); then
                ((retries_count--))
                if (( retries_count == 0 )); then
                    ec "No more retries left."
                    return 1
                fi
            fi
        fi
    done
}
##
