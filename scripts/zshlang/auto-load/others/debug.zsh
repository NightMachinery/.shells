function command_not_found_handler {
    # @unresolved [[https://unix.stackexchange.com/questions/664089/zsh-halt-when-command-not-found][shell script - zsh: Halt when command not found - Unix & Linux Stack Exchange]]
    ##
    # If no external command is found but a function command_not_found_handler exists  the
    # shell  executes this function with all command line arguments.  The return status of
    # the function becomes the status of the command.  If the function wishes to mimic the
    # behaviour  of  the  shell when the command is not found, it should print the message
    # `command not found: cmd' to standard error and return status  127.   Note  that  the
    # handler  is  executed  in  a  subshell  forked to execute an external command, hence
    # changes to directories, shell parameters, etc. have no effect on the main shell.
    ##
    ectrace_ret=127 ectrace "command not found (WARNING: execution not necessarily halted): $(gq "$@")"
    return 127
}
