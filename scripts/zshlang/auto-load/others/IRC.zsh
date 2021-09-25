##
function irc-chatlog-rmself {
    cat-paste-if-tty | sd '^\s*(Lucerne|greyrat)\W*' ' ' | cat-copy-if-tty
}
##
