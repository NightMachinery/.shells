#!/usr/bin/env bash
##
# adapted from:
# https://stackoverflow.com/questions/47938109/reading-answer-to-control-string-sent-to-xterm
##
# wrapped by 'escape-code-answer-read' in zsh.
##
# usage examples:
# `printf '\eP+q544e\e\\' | escape_code_answer_read.bash`
##
tty=/dev/tty

cat >$tty
read -rs -t "${escape_code_answer_read_timeout:-0.2}" -d "" <$tty # the timeout needs to be big, so that it would work via SSH, too
echo $REPLY | xxd -r -p
