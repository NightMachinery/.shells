function printz-quoted() {
 printz "$(gq "$@")"
}
function printz() {
 test -n "$*" && {
  isI && print -rz -- "$@" || ec "$@"
 }
}
