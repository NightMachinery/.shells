function parallel() {
    PARALLEL_SHELL="${PARALLEL_SHELL:-brishz_para.dash}" command parallel --will-cite "$@"
    # it seems parallel quotes the command arguments itself, as it IS expecting a shell
}

function parallelm() {
    local disable_opts="${parallel_d}"
    local dry_run="${parallel_dry}"
    local jobs="${parallel_jobs:-12}" # use '+0' to reset it to its default value (which is the number of CPU cores available)
    ##
    local halt="${parallel_halt:-soon,fail=1}"
    # defaults to `never`, which runs all jobs no matter what.
    # soon,fail=1 :
    # returns the correct exit code if any of its jobs fails
    # exit when 1 job(s) fail, but wait for running jobs to complete.
    ##

    local opts=()
    if bool "$dry_run" ; then
        opts+='--dry-run'
    fi

    if test -z "$parallel_disable" ; then
        if test -n "$jobs" ; then
            opts+=( --jobs "$jobs" )
        fi
        if test -n "$halt" ; then
          opts+=( --halt "$halt" )
        fi
    fi
    parallel "$opts[@]" "$@"
    ## useful opts:
    # -k  Keep sequence of output same as the order of input
    ## tests:
    # `time2 para 'sleep 1 # {}' ::: {1..120}`
    # `parallel --halt soon,fail=1 exit.dash 78 ::: {1..50}`
    # `para exit.dash 78 ::: {1..50}`
    ##
}
@opts-setprefix parallelm parallel
aliasfn para parallelm
@opts-setprefix para parallel

function para-dash() {
    PARALLEL_SHELL="$(which dash)" parallel_jobs="${parallel_jobs:-+32}" para --quote "$@"
}
aliasfn parad para-dash
@opts-setprefix para-dash parallel
@opts-setprefix parad parallel
