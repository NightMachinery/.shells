##
function eval-on-fullhosts {
    local code
    code="$(cat-paste-if-tty)" @RET

    local hosts=( $@ )
    if (( ${#hosts} == 0 )) ; then
        hosts=(
            pino
            c0
            t31
            t21
            m15-hpc
            m17-hpc
            mmd1
        )
    fi

    SHARED_ROOT="/opt/decompv"
    local retcode=0 err=""
    for fullhost in ${hosts[@]} ; do
        ec-sep-h
        ec "fullhost: ${fullhost}"$'\n'

        if eval-ec "${code}" ; then
        else
            local msg
            msg="$0: failed for fullhost=${fullhost}."
            ecerr "$msg"
            err+="$msg"$'\n\n'
            retcode=1

            ##
            # if isDeus ; then
            #     ecgray "continuing to the next server (Deus Mode) ..."

            # else
            #     ecgray "aborting"

            #     return 1
            # fi
            ##
        fi
    done

    ecerr "$err"
    return $retcode
}
##
redis-defvar seg_result_input_file

function h-seg-result-load {
    local f="${1}" ; shift
    assert test -d "$f" @RET

    local metrics_t
    # metrics_t="${f}/method2metrics_mean.json"
    metrics_t="${f}/method2metrics_mean_unique.json"

    local metrics_nt="${f}/method2metrics_no_threshold_mean.json"

    # var-show metric
    {
        if test -e "${metrics_t}" ; then
            < "${metrics_t}"  gron --no-sort
        fi

        if test -e "${metrics_nt}" ; then
            < "${metrics_nt}"  gron --no-sort
        fi
    } |
        sort-last-float #: @expensive
        # cat
}

function seg-result-load {
    local f="${1}" ; shift
    assert test -d "$f" @RET

    local metric="${seg_metric:-BAUPRC}"

    assert seg_result_input_file_set "$f" @RET


    # memoi_key="${0}:${seg_metric}:$(hash-file "$f")" \
    memoi_key_mode='custom_only' \
        memoi_key="${0}:$(hash-dir "$f")" \
        reval-memoi h-seg-result-load "$f" |
        rg "(\.${metric}\b|\[\"${metric}\"\])" |
        {
            if (( $# > 0 )) ; then
                # rg "$@"
                ugb "$*"
            else
                cat
            fi
        }

    # NIGHT_POSTMSG="$f"
    #: doesn't work, so we used a hack in [agfi:h-postmsg]
}
@opts-setprefix seg-result-load seg
noglobfn seg-result-load
##
function seg-aggregate-result-load {
    local d="$f"
    assert-args d @RET

    (
        assert cd "$d" @RET
        fd method2metrics_no_threshold_mean.json | jsons_sort_by_best.py "$@" |& less
    )
}
##
function faith-result-load {
    #: @inputs ff, name
    ##
    local base_dir="$ff"
    assert-args base_dir name @RET

    local input_files=() i
    if (( $#@ == 0 )) ; then
        input_files=(${base_dir}/{nratio,topratio}/{accuracy,aopc_mean}/${name}.csv)
    else
        for i in $@ ; do
            # if test -e "$i" ; then
            #     input_files+="$i"
            # else
                input_files+=${base_dir}/${i}/${name}.csv
            # fi
        done
    fi

    memoi_key_mode='custom_only' \
        memoi_key="${0}:$(hash-files "${input_files[@]}"):${name}" \
        reval-memoi h-faith-result-load "${input_files[@]}" |
        {
            if isTty ; then
                 tac
            else
                cat
            fi
        }
}
noglobfn faith-result-load

function h-faith-result-load {
    local input_files=("$@")

    csv_filter.py --input "${input_files[@]}" \
--exclude-row-fn 'row["area-under-curve"] == ""' \
--preprocess-fn \
'nratio/accuracy:(area-under-curve|average):(1-((c-min(a))/(max(a)-min(a))))' \
'nratio/(aopc|lodds):(area-under-curve|average):(c/max(a))' \
'topratio/accuracy:(area-under-curve|average):(c/max(a))' \
'topratio/(aopc|lodds):(area-under-curve|average):(1-((c-min(a))/(max(a)-min(a))))' \
--join-column 'method' --join-fn 'sum(v)/len(v)' \
-i 'method' 'area-under-curve' 'average' \
-s 'area-under-curve:d' |
    tidy-viewer
}

function h-faith-result-load-v1 {
    #: V1: averages AOPC and LOdds, which is the reason we manually do `sum(v)/4`.
    ##
    csv_filter.py --input ${base_dir}/{nratio,topratio}/{accuracy,aopc_mean,lodds_mean}/${name}.csv \
--exclude-row-fn 'row["area-under-curve"] == ""' \
--preprocess-fn \
'nratio/accuracy:(area-under-curve|average):(1-((c-min(a))/(max(a)-min(a))))' \
'nratio/(aopc|lodds):(area-under-curve|average):0.5*(c/max(a))' \
'topratio/accuracy:(area-under-curve|average):(c/max(a))' \
'topratio/(aopc|lodds):(area-under-curve|average):0.5*(1-((c-min(a))/(max(a)-min(a))))' \
--join-column 'method' --join-fn 'sum(v)/4' \
-i 'method' 'area-under-curve' 'average' \
-s 'area-under-curve:d' |
    tidy-viewer | tac
}
##
function decompv-sort-by-block-index {
    floatsort -e 'blocks__(\d+)'
}
alias SB='decompv-sort-by-block-index'
##
function decompv-channel-mixer-best {
    unique_by.rs -r=-1 -e '_s:([^_"]*)'
}

function decompv-channel-mixer-unique {
    unique_by.rs -r=-1 -e '(attributions_.*)_s:[^_".]*([^".]*)'
}
##
