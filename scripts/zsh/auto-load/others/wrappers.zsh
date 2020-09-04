function '$'() { eval "$(gquote "$@")" ; }
function tldr() {
  #nig ea  #not needed because of piping autoremoval of color.
  isDarwin && { command tldr "$@" || command tldr -p linux "$@" ; return $? }
  command tldr "$@" | bt
}
function exa() {
  local arg long=''
  for arg in "$@"
  do
    [[ "$arg" == "-l" || "$arg" == "--long" || "$arg" =~ "-(-tree|T)" ]] && long='y'
  done
  if test -z "$long"
  then
    command exa -1 "$@"
  else
    command exa "$@"
  fi
}
# function tsend() {
#   unfunction "$0"
#   pxify-auto
#   tsend "$@"
# }
function k2pdfopt() {
    if isDarwin ; then
        command k2pdfopt_darwin "$@"
    else
        command k2pdfopt_linux "$@"
    fi
}
function mosh() {
  command mosh --server="ITERM_SESSION_ID=$ITERM_SESSION_ID mosh-server" "$@" # -- zsh
}
