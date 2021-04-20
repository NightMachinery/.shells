ashL() {
    local a=() i
    for i in "$@[2,-1]"
    do
        a+=(-L "${i}:localhost:${i}")
    done
    ash -NT "$1" "$a[@]"
}
##
function kitty-terminfo-install() {
    infocmp -x xterm-kitty | ssh "$@" tic -x -o \~/.terminfo/ /dev/stdin
}
function ssh() {
  if fn-isTop && isKitty ; then
    # will install the xterm-kitty terminal definition on the remote in your home directory.
    # Only needs to run once per host
    kitty +kitten ssh "$@"
  else
    command ssh "$@"
  fi
}
##
