# https://www.zsh.org/mla/workers/2020/msg00638.html
# https://unix.stackexchange.com/questions/585941/zsh-bug-unexpected-process-suspension

mdoc-test () {
    sleep 0 | sleep 0
    cat
}

echo start |VISUAL=vim command vipe|mdoc-test

# zsh: suspended (tty output)
