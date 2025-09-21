##
function battery-p {
    if isServer ; then
        return 1
    fi

    if isDarwin ; then
        battery-p-darwin
        return $?
    else
        @NA
    fi
}

function battery-p-darwin {
    pmset -g batt | command rg -q -F "drawing from 'Battery Power'"
}

function battery-status-darwin-full {
    assert isDarwin @RET

    pmset -g batt | command rg InternalBattery | command column -t
    ##
    # mcli battery status
}

function battery-status-all-darwin {
    pmset -g accps
}


function battery-percentage-darwin {
    battery-status-darwin-full | rget '(\d+)%;'
}
##
function power-monitor-darwin {
    sudo powermetrics -i 1000 --poweravg 1 | grep 'Average cumulatively decayed power score' -A 20
}
alias energy-monitor-darwin='power-monitor-darwin'
###
# Apple Silicon laptops with firmware > 13.0 have a native charge threshold that does not required any userspace daemon running.
# This native limit works even when the laptop is sleeping or powered off therefore it is preferable to the userspace daemon.
# Nonetheless, it only works with fixed thresholds (80% as upper limit and 70% as lower limit).
# CHWA key is the one used to enable/disable the native limit. 01 = 80% limit, 00 = no limit
##
redis-defvar battery_charge_limit_p

typeset -g smc_command="/usr/local/bin/smc"
typeset -g smc_charge_limit_key="CHWA"
typeset -g smc_charge_limit_status_on="01"
typeset -g smc_charge_limit_status_off="00"

function battery-charge-limit-enable {
    #: [[id:34b5f3ab-988c-4fdb-93d6-1d0ba6b964f2][battery-charge-limit-enable]]
    ##
    if isDarwin ; then
        assert test -e "${smc_command}" @RET

        ecgray "$0"
        reval-ecgray sudo "${smc_command}" -k "${smc_charge_limit_key}" -w "${smc_charge_limit_status_on}" @RET

        assert battery_charge_limit_p_set true @RET
    else
        @NA
    fi
}

function battery-charge-limit-disable {
    if isDarwin ; then
        assert test -e "${smc_command}" @RET

        ecgray "$0"
        reval-ecgray sudo "${smc_command}" -k "${smc_charge_limit_key}" -w "${smc_charge_limit_status_off}" @RET

        assert battery_charge_limit_p_set false @RET
    else
        @NA
    fi
}

function battery-charge-limit-restore-status {
    retry ensure-redis @RET

    local status_p
    status_p="$(battery_charge_limit_p_get)" @TRET

    if bool "${status_p}" ; then
        reval-ecgray battery-charge-limit-enable
    else
        reval-ecgray battery-charge-limit-disable
    fi
}

function battery-charge-limit-status {
    if isDarwin ; then
        assert test -e "${smc_command}" @RET

        local status_raw="$(reval-ec sudo "${smc_command}" -k "${smc_charge_limit_key}" -r)"
        # var-show status_raw
        status_raw="$(ec "$status_raw" | rget '\(bytes (\d+)\)')"
        # var-show status_raw

        case "$status_raw" in
            "${smc_charge_limit_status_on}")
                ec "on"
                ;;
            "${smc_charge_limit_status_off}")
                ec "off"
                ;;
            *)
                ec "$0: unknown ${smc_charge_limit_key} status: $status_raw"
                ;;
        esac
    else
        @NA
    fi
}

aliasfn bls battery-charge-limit-status
aliasfn ble battery-charge-limit-enable
aliasfn bld battery-charge-limit-disable
###
function last-power-AC-get {
  local unplugged_time
  unplugged_time="$(pmset -g log | rg --ignore-case 'using AC\b' | tail -1 | awk '{print $1, $2, $3}')" @TRET

  local unplugged_timestamp
  unplugged_timestamp="$(gdate -d "$unplugged_time" +"%s")" @TRET

  if fn-isTop ; then
      ecgray "${unplugged_time}"
  fi

  ec "${unplugged_timestamp}"
}
##
