function '$' {
  reval "$@"
}
##
function hammerspoon() {
  # -t timeout (default 4)
  assert gtimeout 30s hs -A -t 5 "$@" @RET
}
##
function mcli-getexecpath_h() {
  brew unlink m-cli
  
  mcli_path=("$(brew --cellar m-cli)"/*/bin/m)
  ec "$mcli_path"
}
function mcli-getexecpath() {
  memoi_expire=0 memoi-eval mcli-getexecpath_h "$@"
}
function mcli() {
  # https://github.com/rgcr/m-cli
  command "${mcli_path:-$(mcli-getexecpath)}" "$@"
}
function tldr() {
  #nig ea  #not needed because of piping autoremoval of color.
  isDarwin && { command tldr "$@" || command tldr -p linux "$@" ; return $? }
  command tldr "$@" | bt
}

function eza {
  local exa_command='exa'
  if isdefined-cmd eza ; then
    exa_command='eza'
  fi

  local arg long='' has_path_p=''
  for arg in "$@" ; do
    if [[ "$arg" == "--color" ]] ; then
      continue
      #: skip the value of this option
    fi

    if [[ "$arg" == "-l" || "$arg" == "--long" || "$arg" =~ "-(-tree|T)" ]] ; then
      long='y'
    fi

    if test -n "${arg}" && ! [[ "$arg" =~ "^-" ]] ; then
      has_path_p='y'
    fi
  done

  # typ has_path_p

  if ! bool "${has_path_p}" ; then
    set -- "${@}" .
    #: working around eza bug, to show the CWD
  fi

  if test -z "$long"
  then
    revaldbg command "${exa_command}" -1 "$@"
  else
    revaldbg command "${exa_command}" "$@"
  fi
}
aliasfn exa eza

function k2pdfopt {
    if isDarwin ; then
        command k2pdfopt_darwin "$@"
    else
        command k2pdfopt_linux "$@"
    fi
}
##
function ffmpeg() {
    isI && command ffmpeg "$@" || command ffmpeg -loglevel error "$@"
}
##
function edir() {
  command edir --all --recurse  --suffix '' "$@"
}
##
function watchm() {
    ruu "watch -n $1" "${@:2}"
}
##
function fd {
  command fd -u "$@"
  #: -i,  --ignore-case
  #: -u: include ignored files by default
}
##
# function open() {
#   assert isDarwin @RET
#   if false && [[ "$1" =~ '\.pdf$' ]] ; then
#     ##
#     # command open -a opera "$1"
#     ##
#     assert chrome-open-pdf "$1"
#     ##
#     shift
#     "$0" "$@"
#     return $?
#   elif (( $#@ >= 1 )) ; then
#     command open "$@"
#   fi
# }

function open-with {
  local app="$1" ; shift
  assert-args app @RET

  in-or-args "$@" |
    inargsf grealpath -- |
    inargsf reval-ec open -a "$app" --
}

aliasfn open-preview open-with preview
aliasfn opv open-preview

aliasfn open-skim open-with Skim
aliasfn ops open-skim

function open-sioyek-v1 {
  open-with sioyek "$@" @RET
  sioyek-reload
}
# aliasfn opy open-sioyek
# aliasfn sio open-sioyek

function open-sioyek-v2 {
  local file="${1}" ; shift
  assert-args file @RET

  file="$(path-unabbrev "${file}")" @TRET

  awaysh sioyek "${file}" "$@" @TRET
  sioyek-reload
  sioyek-focus
}
aliasfn open-sioyek open-sioyek-v2
aliasfn sio open-sioyek-v2
##
function mega-get {
  if isBicon ; then
    ecgray "$0: mega-get sometimes hangs when run in Bicon; Wrapping it in a tmux session."
    tshd command mega-get "$@"
  else
    command mega-get "$@"
  fi
}
##
function mipsi-stdin() {
  mipsi =(cat)
}
##
function ssh-p() {
  local password
  vared -p 'Password:' password @RET
  # typeset -g ssh_pass="$password" # @naughty

  if isDarwin ; then
    # https://stackoverflow.com/questions/32255660/how-to-install-sshpass-on-mac/62623099#62623099
    ##
    ssh "$@"
  else
    sshpass -p "$pass" ssh "$@"
  fi
}
##
function pwd {
  if isOutTty ; then
    ecn "${PWD}/" | pbcopy || true
  fi

  builtin pwd
}
##
function titlecase {
  #: @install/pip
  #:
  #: * @seeAlso
  #: ** [agfi:lower]
  ##
  cat-paste-if-tty | command titlecase "$@" | cat-copy-if-tty
}
##
function spotdl {
  local args=("$@")

  invocation-save "$0" "$0" "${args[@]}"

  @opts engine bell-dl @ reval-bell command spotdl "${args[@]}"
}
##
function vcard-to-json {
  # ~[vcard-to-json]/vcard-to-json-0.1.0-SNAPSHOT "$@" |
  java -jar ~[vcard-to-json]/target/vcard-to-json-0.1.0-SNAPSHOT.jar "$@" |
    jq-rtl . #: needed to, e.g., normalize strings
}
##
function zathura {
  local name="${1:r}" args=()
  name="zathura $(str2tmuxname "$name")" @TRET
  for arg in "$@" ; do
    if test -e "$arg" ; then
      args+="$(grealpath -- "$arg")" @TRET
    else
      args+="$arg"
    fi
  done

  reval-ec tmuxnew "$name" command zathura "${args[@]}"
}
##
function tidy-viewer {
  command tidy-viewer --extend-width-and-length --color 5 -n 99999 --upper-column-width 999 "$@" |
    whitespace-shared-rm
}
##
function duh {
  reval-ecgray gdu --exclude='.git' -h "$@"
}
##
alias tzc='timezone_converter.py'
##
function './gradlew' {
  $proxyenv command ./gradlew "$@"
}
##
