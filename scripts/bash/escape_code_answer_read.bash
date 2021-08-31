#!/usr/bin/env bash
##
# adapted from:
# https://stackoverflow.com/questions/47938109/reading-answer-to-control-string-sent-to-xterm
##
# usage examples:
# `printf '\eP+q544e\e\\' | escape_code_answer_read.bash`
##
tty=/dev/tty

cat >$tty
read -rs -t 0.2 -d "" <$tty
echo $REPLY | xxd -r -p
