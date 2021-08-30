function vared-py() {
    vared.py "$@" # </dev/tty # incompatible with prompt_toolkit
}
##
function ask() {
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
