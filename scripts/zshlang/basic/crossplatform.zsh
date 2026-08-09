isLinux && {
    alias gxargs='xargs'
    alias gfind='find'
    alias gsed=sed
}
isDarwin && {
    alias crontab='VISUAL=vim EDITOR=vim crontab'
}
##
true
#: @warn Keep this `true` last.
#: A sourced file's exit status is that of its last command. On Linux the last
#: command is `isDarwin`, which returns 1, so `source-basic ... crossplatform`
#: failed and [agfi:basic-full.zsh] aborted before loading `debug`, `enhancers`
#: and everything after them. The symptom was not an error message: it was an
#: infinite `command_not_found_handler` recursion (the handler calls `ectrace`,
#: which had never been defined), which forks subshells until the shell hangs.
#: macOS never hit this because there `isDarwin` is the one that returns 0.
