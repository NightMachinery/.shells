##
typeset -g QVIEW_IPC_SOCKET="/tmp/qview-${UID}.sock"
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

    if test -n "${QVIEW_IPC_SOCKET}" ; then
      silent pkill9 qView
    fi

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
      socat - "UNIX-CONNECT:${socket}" 2>/dev/null )" || {
      ecerr "$0: could not get a response from qView's socket"
      return 1
  }

  dvar response

  local ok qview_path
  ok="$(ec "$response" | command jq -er '.ok')" || return 1
  [[ "$ok" == true ]] || return 1

  qview_path=$(ec "$response" | command jq -er '.path // empty') || return 1
  [[ -n "$qview_path" ]] || return 1

  ec "$qview_path"
}
##
function reval-on-qview {
    reval-ecgray "$@" "$(qview-path-get)"
}

function h-hs-on-qview {
  hs-reval-alert reval-on-qview "$@"
}
##
typeset -g qview_deletion_log="${HOME}/.qview/deletion_log"
typeset -g qview_trash_dir="${HOME}/.qview/trash"

function qview-trs {
    local f
    f="$(qview-path-get)" @RET

    if ! test -e "$f" ; then
        ecerr "$0: nonexistent file: $f"
        return 1
    fi

    sync-append-with-newline "${qview_deletion_log}" "$f"

    local trash_path
    trash_path="${qview_trash_dir}/$f"

    if test -e "${trash_path}" ; then
      trash_path+="_${EPOCHSECONDS}"
    fi

    reval-ecgray mv "$f" "${trash_path}"
}

function qview-restore-last {
   local f
   f="$(tail -n 1 "${qview_deletion_log}")" @RET

   local trash_path
   trash_path="${qview_trash_dir}/$f"

   reval-ecgray mv -i "${trash_path}" "$f"
}
##
