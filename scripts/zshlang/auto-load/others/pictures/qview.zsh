##
typeset -g QVIEW_IPC_SOCKET="${TMPDIR:-/tmp/}qview-${UID}.sock}"
##
function qview {
    ##
    # img-gallery-open-app "qView" "$@" @RET
    ##
    # local links_dir
    # links_dir="$(h-img-gallery-links-dir "$@")" @RET

    # h-qview-open -- "${links_dir}" @RET
    ##
    h-qview-open "$@"
}

function h-qview-open {
    local args=("$@")
    args=("${(@f)$(re realpath-ife "$@")}") @TRET

    open -n -a qView --args --ipc-server="${QVIEW_IPC_SOCKET}" "$args[@]"
    #: =-n= if you need to start a new qView instance instead of reusing an existing one.
    #: Everything after =--args= is passed to qView as command-line arguments.
}
##
function qview-path-get {
  local socket="${QVIEW_IPC_SOCKET}"
  assert-args socket @RET

  local response
  response="$(ec '{"method":"currentFilePath"}' |
      socat - "UNIX-CONNECT:${socket}" 2>/dev/null )" || return 1

  dvar response

  local ok qview_path
  ok="$(ec "$response" | command jq -er '.ok')" || return 1
  [[ "$ok" == true ]] || return 1

  qview_path=$(ec "$response" | command jq -er '.path // empty') || return 1
  [[ -n "$qview_path" ]] || return 1

  ec "$qview_path"
}
##
