##
function grc-colorize-cmd {
  local cmd_head="$1"

  if isDefined-cmd grc ; then
    aliasfn "${cmd_head}" command grc "${cmd_head}"
  fi
}

grc_commands=(
    ping
    netstat
    ps
    uptime
    du
    # df
)

re grc-colorize-cmd ${grc_commands[@]}
##
function df {
    local cmd="gdf"
    if ! isDefined-cmd "${cmd}" ; then
        cmd="df"
    fi

    if isDefined-cmd grcat ; then
        command "${cmd}" "$@" |
            grcat conf.df
    else
        command "${cmd}" "$@"
    fi
}
##
