function retry-eval() {
    retry-limited-eval 0 "$@"
}

function retry-limited {
    retry-limited-eval "$1" "$(gquote "${@:2}")"
}

function h-retry-limited-eval {
  if bool "${retry_noidle_p}" ; then
    if idle-p ; then
      ecerr "$0: skipped retrying because the user was idle"

      return 1
    fi
  fi

  eval "${cmd}" && ecode=0 || {
      ecode="$?"
      ecerr "Tried (exited ${ecode}): ${cmd}"
    }

  return "$ecode"
}

function retry-limited-eval {
    local retry_sleep="${retry_sleep:-0.5}"
    local retry_noidle_p="${retry_noidle_p}"
    local retries_left="$1" cmd="${@:2}"

    local limit=0
    local ecode=32
    until { test "${retries_left}" -gt 0 && test "$limit" -ge "${retries_left}" && ecerr "$0: retry limit exceeded" } || h-retry-limited-eval
    do
        sleep $retry_sleep
        limit=$((limit+1))
    done

    # re var-show cmd ecode retries_left

    return "$ecode"
}
alifn retry='retry-limited 0'
##
function failnoisily() {
    reval "$@"
    local r=$?
    if (( r )) ; then
        notif "$0: $(gq "$@") (returned $r) "
    fi
}

function failnoisily-if-net {
    assert-net @RET

    failnoisily "$@"
}
##
