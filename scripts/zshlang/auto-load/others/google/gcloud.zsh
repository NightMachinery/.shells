##
#: GPU workstation on GCP, project `relation-neuron-detection`.
#:
#: The prefix is two-level on purpose. `gcp-` names the *platform*, not the
#: CLI: much of what is below is not gcloud at all (BigQuery, bucket syncs,
#: ssh), which leaves `gcloud-` free for thin wrappers around the binary. The
#: `gpu-` level reserves room for the `gcp-quota-*`, `gcp-iam-*` and
#: `gcp-keys-*` helpers that will land in this same file later.
#:
#: `relation-neuron-detection` is a *shared* lab project: seven personal gmail
#: accounts hold `roles/editor`, there is no organization parent, and there is
#: no central admin. Everything here therefore filters on
#: `owner=${gcp_gpu_owner}` and refuses to touch anything else. See
#: `~[nt]/private/research/Hinrich Schutze/GCloud/GPU/usage.org`.
##
typeset -g gcp_gpu_project="${gcp_gpu_project:-relation-neuron-detection}"
typeset -g gcp_gpu_zone="${gcp_gpu_zone:-europe-west4-a}"
typeset -g gcp_gpu_region="${gcp_gpu_region:-europe-west4}"
typeset -g gcp_gpu_instance="${gcp_gpu_instance:-rnd-gpu}"
typeset -g gcp_gpu_machine="${gcp_gpu_machine:-g2-standard-8}"
typeset -g gcp_gpu_boot_gb="${gcp_gpu_boot_gb:-100}"
#: G2 does not support `pd-standard`.
typeset -g gcp_gpu_boot_type="${gcp_gpu_boot_type:-pd-balanced}"
typeset -g gcp_gpu_image_family="${gcp_gpu_image_family:-ubuntu-2404-lts-amd64}"
typeset -g gcp_gpu_image_project="${gcp_gpu_image_project:-ubuntu-os-cloud}"
#: Persistent disks are zonal. This disk pins the instance to `$gcp_gpu_zone`.
typeset -g gcp_gpu_data_disk="${gcp_gpu_data_disk:-rnd-data}"
typeset -g gcp_gpu_data_gb="${gcp_gpu_data_gb:-300}"
typeset -g gcp_gpu_data_type="${gcp_gpu_data_type:-pd-balanced}"
#: `$USERNAME` rather than `$USER`: zsh maintains it from the effective UID, so
#: it is always set. `$USER` is empty in a non-login shell, which would silently
#: turn every `owner=` filter below into `owner=` and match nothing.
typeset -g gcp_gpu_owner="${gcp_gpu_owner:-${USERNAME:-${USER}}}"
#: Never the default compute SA: it holds `roles/editor`, and anything on the
#: box could mint an editor token from the metadata server.
typeset -g gcp_gpu_sa="${gcp_gpu_sa:-gpu-runner@${gcp_gpu_project}.iam.gserviceaccount.com}"
typeset -g gcp_gpu_bucket="${gcp_gpu_bucket:-gs://rnd-results-${gcp_gpu_owner}}"
typeset -g gcp_gpu_max_run="${gcp_gpu_max_run:-8h}"
typeset -g gcp_gpu_idle_min="${gcp_gpu_idle_min:-30}"
typeset -g gcp_gpu_budget_config="${gcp_gpu_budget_config:-${HOME}/.config/gcp-gpu-budget}"
typeset -g gcp_gpu_budget_default="${gcp_gpu_budget_default:-50}"
#: How far before a spend window to scan the audit log, and how long to cache
#: the scan. Both exist because `gcloud logging read` is slow enough to matter
#: in `gcp-gpu-status`. See `h-gcp-gpu-audit-events`.
typeset -g gcp_gpu_audit_lookback_days="${gcp_gpu_audit_lookback_days:-7}"
typeset -g gcp_gpu_audit_cache_ttl="${gcp_gpu_audit_cache_ttl:-300}"
typeset -g gcp_gpu_retry_sleep="${gcp_gpu_retry_sleep:-60}"
typeset -g gcp_gpu_retry_max="${gcp_gpu_retry_max:-60}"
typeset -g gcp_gpu_tmux_session="${gcp_gpu_tmux_session:-work}"
##
#: Reaper thresholds. These exist to catch a FAILURE of the two mechanisms that
#: normally stop this box (GCE's --max-run-duration and the on-VM idle timer),
#: not to compete with them -- see `gcp-gpu-reap`.
#:
#: The grace period is added to --max-run-duration before the reaper considers
#: layer 1 to have failed; GCE is not instantaneous.
typeset -g gcp_gpu_reap_grace_min="${gcp_gpu_reap_grace_min:-30}"
#: Consider CPU "flat" below this mean percentage. A g2-standard-8 driving an
#: L4 sits well above it -- you cannot feed a GPU with zero host CPU -- so this
#: is a conservative superset of "GPU idle" and will not kill a CPU-bound
#: preprocessing or download stage.
typeset -g gcp_gpu_reap_cpu_pct="${gcp_gpu_reap_cpu_pct:-3}"
#: How long CPU must stay flat. Longer than the on-VM idle threshold on
#: purpose: if the timer were alive, the box would already be gone.
typeset -g gcp_gpu_reap_idle_min="${gcp_gpu_reap_idle_min:-60}"
#: A heartbeat older than this means the on-VM idle timer is not running.
typeset -g gcp_gpu_reap_heartbeat_max_min="${gcp_gpu_reap_heartbeat_max_min:-10}"
##
#: ===========================================================================
#: PRICE TABLE -- EUR/hour, list price, region `europe-west4`.
#:
#: @warn THESE GO STALE. Checked <2026-08-11 Tue>. They drive every estimate
#: printed by `gcp-gpu-burn`, `gcp-gpu-budget` and the audit-log fallback in
#: `gcp-gpu-spend`; none of those are billed euros. Re-check against
#: https://cloud.google.com/compute/all-pricing every few months, and treat
#: the BigQuery export as the truth the moment it exists.
#:
#: Spot prices float. `PREEMPTIBLE_NVIDIA_A100_80GB_GPUS` quota is 0 in this
#: region, so `a2-ultragpu-*` has no spot entry at all.
#: ===========================================================================
typeset -gA gcp_gpu_price_ondemand=(
    g2-standard-4    0.45
    g2-standard-8    0.65
    g2-standard-12   0.85
    g2-standard-16   1.05
    g2-standard-32   1.75
    a2-highgpu-1g    3.20
    a2-ultragpu-1g   4.30
    a3-highgpu-8g   29.00
)
typeset -gA gcp_gpu_price_spot=(
    g2-standard-4    0.14
    g2-standard-8    0.20
    g2-standard-12   0.26
    g2-standard-16   0.32
    g2-standard-32   0.54
    a2-highgpu-1g    1.05
    a3-highgpu-8g    9.00
)
#: EUR per GB-month.
typeset -gA gcp_gpu_price_disk=(
    pd-balanced          0.11
    pd-ssd               0.19
    pd-standard          0.045
    hyperdisk-balanced   0.13
)
##
function h-gcp-gpu-gcloud {
    command gcloud --project="${gcp_gpu_project}" "$@"
}

function h-gcp-gpu-labels {
    ec "owner=${gcp_gpu_owner},budget=personal,purpose=gpu-research"
}

function h-gcp-gpu-label-filter {
    ec "labels.owner=${gcp_gpu_owner}"
}

function h-gcp-gpu-deps {
    ensure-cmd gcloud jq
}

function h-gcp-gpu-dry-run-p {
    bool "${gcp_gpu_dry_run}"
}

function h-gcp-gpu-reval {
    #: Every mutating call goes through here, so `--dry-run` is total rather
    #: than sprinkled per-callsite.
    if h-gcp-gpu-dry-run-p ; then
        ecgray "DRY-RUN: $(gquote-simple "$@")"
        return 0
    fi

    reval-ec "$@"
}
##
function h-gcp-gpu-price {
    #: `h-gcp-gpu-price MACHINE [PROVISIONING_MODEL]` -> EUR/hour, or 0 if unknown.
    local machine="${1:?}" model="${2:-STANDARD}" price

    if [[ "${model:u}" == SPOT ]] ; then
        price="${gcp_gpu_price_spot[$machine]}"
    fi
    if test -z "$price" ; then
        price="${gcp_gpu_price_ondemand[$machine]}"
    fi

    if test -z "$price" ; then
        #: Silence here would under-report burn and month-to-date, which is
        #: exactly the direction the soft cap must never fail in. Add the
        #: machine to the price table above.
        ecerr "$0: no price for '${machine}' -- counting it as 0. Budget figures are now UNDER-reported."
        price=0
    fi

    ec "$price"
}

function h-gcp-gpu-disk-price {
    #: `h-gcp-gpu-disk-price TYPE SIZE_GB` -> EUR/month.
    local type="${1:?}" gb="${2:?}" rate
    rate="${gcp_gpu_price_disk[$type]:-0.11}"

    printf '%.2f\n' $(( rate * gb ))
}
##
function h-gcp-gpu-month-start {
    local y m
    y="$(strftime '%Y' $EPOCHSECONDS)"
    m="$(strftime '%m' $EPOCHSECONDS)"

    local out
    strftime -r -s out '%Y-%m-%d %H:%M:%S' "${y}-${m}-01 00:00:00" || return $?
    ec "$out"
}

function h-gcp-gpu-month-end {
    local y m
    y="$(strftime '%Y' $EPOCHSECONDS)"
    #: `10#` so that `08` and `09` are not read as bad octal.
    m="$(strftime '%m' $EPOCHSECONDS)"

    integer ny=$(( 10#$y )) nm=$(( 10#$m + 1 ))
    if (( nm > 12 )) ; then
        nm=1
        ny=$(( ny + 1 ))
    fi

    local out
    strftime -r -s out '%Y-%m-%d %H:%M:%S' "$(printf '%04d-%02d-01 00:00:00' $ny $nm)" || return $?
    ec "$out"
}

function h-gcp-gpu-day-start {
    local out
    strftime -r -s out '%Y-%m-%d %H:%M:%S' "$(strftime '%Y-%m-%d 00:00:00' $EPOCHSECONDS)" || return $?
    ec "$out"
}

function h-gcp-gpu-dur-human {
    #: seconds -> `3d 04h 12m`
    integer s="${1:-0}"
    integer d=$(( s / 86400 )) h=$(( (s % 86400) / 3600 )) m=$(( (s % 3600) / 60 ))

    if (( d > 0 )) ; then
        printf '%dd %02dh %02dm\n' $d $h $m
    else
        printf '%02dh %02dm\n' $h $m
    fi
}
##
function h-gcp-gpu-instance-json {
    #: Prints the instance resource, or fails silently when it does not exist.
    h-gcp-gpu-gcloud compute instances describe "${gcp_gpu_instance}" \
        --zone="${gcp_gpu_zone}" --format=json 2>/dev/null
}

function h-gcp-gpu-instance-exists-p {
    h-gcp-gpu-instance-json >/dev/null 2>&1
}

function h-gcp-gpu-disk-exists-p {
    h-gcp-gpu-gcloud compute disks describe "${gcp_gpu_data_disk}" \
        --zone="${gcp_gpu_zone}" &>/dev/null
}

function h-gcp-gpu-sa-exists-p {
    h-gcp-gpu-gcloud iam service-accounts describe "${gcp_gpu_sa}" &>/dev/null
}

function h-gcp-gpu-bucket-exists-p {
    command gcloud storage buckets describe "${gcp_gpu_bucket}" \
        --project="${gcp_gpu_project}" &>/dev/null
}
##
function h-gcp-gpu-billing-table {
    #: Resolves the billing-export table in `cloud_billing` and caches the
    #: answer for the shell session. Returns 1 when the export does not exist,
    #: which is the current state of this project: the dataset was created
    #: 2026-01-02 and no export was ever enabled into it. Callers fall back to
    #: `h-gcp-gpu-spend-audit`.
    ##
    if test -n "${gcp_gpu_billing_table_cache}" ; then
        if [[ "${gcp_gpu_billing_table_cache}" == NONE ]] ; then
            return 1
        fi
        ec "${gcp_gpu_billing_table_cache}"
        return 0
    fi

    local table
    table="$(command bq --project_id="${gcp_gpu_project}" ls --format=json "${gcp_gpu_project}:cloud_billing" 2>/dev/null \
        | command jq -r '[.[]?.tableReference.tableId | select(test("^gcp_billing_export"))] | first // empty')"

    if test -z "$table" ; then
        typeset -g gcp_gpu_billing_table_cache=NONE
        return 1
    fi

    typeset -g gcp_gpu_billing_table_cache="${gcp_gpu_project}.cloud_billing.${table}"
    ec "${gcp_gpu_billing_table_cache}"
}

function h-gcp-gpu-spend-bq {
    #: `h-gcp-gpu-spend-bq FROM_EPOCH TO_EPOCH` -> TSV of `service<TAB>eur`.
    local from="${1:?}" to="${2:?}" table
    table="$(h-gcp-gpu-billing-table)" || return 1

    local query
    query="SELECT service.description AS service, ROUND(SUM(cost), 4) AS eur
FROM \`${table}\`
WHERE usage_start_time >= TIMESTAMP_SECONDS(${from})
  AND usage_start_time <  TIMESTAMP_SECONDS(${to})
  AND EXISTS (SELECT 1 FROM UNNEST(labels) l
              WHERE l.key = 'owner' AND l.value = '${gcp_gpu_owner}')
GROUP BY service
HAVING eur > 0
ORDER BY eur DESC"

    command bq --project_id="${gcp_gpu_project}" query \
        --use_legacy_sql=false --format=json --quiet "$query" 2>/dev/null \
        | command jq -r '.[]? | [.service, .eur] | @tsv'
}

function h-gcp-gpu-audit-events {
    #: `h-gcp-gpu-audit-events FROM_EPOCH [INSTANCE]`
    #:
    #: Admin Activity audit logs are always on, free, and readable without any
    #: extra role -- unlike Data Access logs, which are off in this project.
    #: That is what makes the fallback estimate possible at all.
    ##
    local from="${1:?}" name="${2:-${gcp_gpu_instance}}"

    #: The `timestamp` bound is not an optimisation, it is the difference
    #: between 6 seconds and minutes: unbounded, the backend walks the whole
    #: 400-day retention window, and `gcp-gpu-status` becomes unusable.
    #: `--max-run-duration` caps a single run at ${gcp_gpu_max_run}, so looking
    #: back a few days before the window provably catches a run that was
    #: already going when the window opened.
    #:
    #: `from` is always a month or day boundary, so `since` is stable and the
    #: memoi key actually hits instead of missing once a second.
    local since
    local -x TZ=UTC
    strftime -s since '%Y-%m-%dT%H:%M:%SZ' \
        $(( from - gcp_gpu_audit_lookback_days * 86400 )) || return $?

    memoi_expire="${gcp_gpu_audit_cache_ttl}" memoi_skiperr=y memoi-eval \
        h-gcp-gpu-gcloud logging read \
        "logName:\"cloudaudit.googleapis.com%2Factivity\" AND resource.type=\"gce_instance\" AND protoPayload.resourceName:\"/instances/${name}\" AND timestamp>=\"${since}\"" \
        --limit=1000 --format=json 2>/dev/null
}

function h-gcp-gpu-spend-audit {
    #: `h-gcp-gpu-spend-audit FROM_EPOCH TO_EPOCH` -> TSV of `service<TAB>eur`.
    #:
    #: Reconstructs running intervals from the audit log and multiplies by the
    #: price table. An estimate, not billed euros: it models the VM and my
    #: labeled disks, and nothing else (no egress, no IP, no snapshots).
    ##
    local from="${1:?}" to="${2:?}"

    local machine="${gcp_gpu_machine}" model=STANDARD json
    json="$(h-gcp-gpu-instance-json)"
    if test -n "$json" ; then
        machine="$(ec "$json" | command jq -r '.machineType | split("/") | last')"
        model="$(ec "$json" | command jq -r '.scheduling.provisioningModel // "STANDARD"')"
    fi

    local rate
    rate="$(h-gcp-gpu-price "$machine" "$model")"

    local seconds
    seconds="$(h-gcp-gpu-audit-events "$from" \
        | command jq --argjson from "$from" --argjson to "$to" --argjson now "$EPOCHSECONDS" '
            [ .[]? | { t: (.timestamp | sub("\\.[0-9]+"; "") | fromdateiso8601),
                       m: (.protoPayload.methodName | split(".") | last) } ]
            | sort_by(.t)
            #: A fold, not a pairwise zip: the log repeats `start` several times
            #: for a single boot, and a naive pairing double-counts badly.
            | reduce .[] as $e ({run: null, out: []};
                if ($e.m == "insert" or $e.m == "start") then
                    (if .run == null then .run = $e.t else . end)
                elif ($e.m | test("^(stop|delete|preempted|guestTerminate)$")) then
                    (if .run != null then .out += [[.run, $e.t]] | .run = null else . end)
                else . end)
            | (if .run != null then .out + [[.run, $now]] else .out end)
            | map((([.[1], $to] | min) - ([.[0], $from] | max)) | if . > 0 then . else 0 end)
            | add // 0')"
    : "${seconds:=0}"

    local compute
    compute="$(printf '%.4f' $(( rate * seconds / 3600.0 )))"
    if (( compute > 0 )) ; then
        printf 'Compute Engine (estimated)\t%s\n' "$compute"
    fi

    #: Disks bill whether or not anything is running, so a month-to-date figure
    #: that omits them understates the standing floor -- the exact thing the cap
    #: exists to catch.
    local disk_month
    disk_month="$(h-gcp-gpu-gcloud compute disks list --filter="$(h-gcp-gpu-label-filter)" --format=json 2>/dev/null \
        | command jq -r --argjson p "$(h-gcp-gpu-price-disk-json)" '
            [ .[]? | ($p[.type | split("/") | last] // 0.11) * (.sizeGb | tonumber) ] | add // 0')"
    : "${disk_month:=0}"

    local disk
    disk="$(printf '%.4f' $(( disk_month * (to - from) / (86400.0 * 30) )))"
    if (( disk > 0 )) ; then
        printf 'Persistent Disk (estimated)\t%s\n' "$disk"
    fi
}

function h-gcp-gpu-price-disk-json {
    local k out=()
    for k in "${(@k)gcp_gpu_price_disk}" ; do
        out+=( "$(command jq -nc --arg k "$k" --argjson v "${gcp_gpu_price_disk[$k]}" '{($k): $v}')" )
    done

    command jq -nc --argjson a "[${(j:,:)out}]" '$a | add // {}'
}

function h-gcp-gpu-spend-total {
    #: `h-gcp-gpu-spend-total FROM_EPOCH TO_EPOCH` -> a bare EUR number.
    #: Sets `$gcp_gpu_spend_source` to `bigquery` or `audit-estimate`.
    local from="${1:?}" to="${2:?}" rows

    if rows="$(h-gcp-gpu-spend-bq "$from" "$to")" && test -n "$rows" ; then
        typeset -g gcp_gpu_spend_source=bigquery
    else
        if h-gcp-gpu-billing-table >/dev/null 2>&1 ; then
            #: The table exists but returned nothing, which for a month-to-date
            #: window means genuinely zero rather than a missing source.
            typeset -g gcp_gpu_spend_source=bigquery
            ec 0
            return 0
        fi
        rows="$(h-gcp-gpu-spend-audit "$from" "$to")"
        typeset -g gcp_gpu_spend_source=audit-estimate
    fi

    ec "$rows" | command awk -F'\t' '{ s += $2 } END { printf "%.4f\n", s + 0 }'
}

function h-gcp-gpu-spend-source-note {
    if [[ "${gcp_gpu_spend_source}" == bigquery ]] ; then
        ecgray "source: BigQuery billing export. Lags a few hours -- a zero here may not mean zero."
    else
        ecgray "source: ESTIMATE from Admin Activity logs + the local price table."
        ecgray "        The billing export into ${gcp_gpu_project}:cloud_billing was never enabled;"
        ecgray "        no egress, IP or snapshot cost is modelled. See usage.org, Escalation."
    fi
}
##
function h-gcp-gpu-budget-cap {
    #: The cap is a plain number in a file so that it is trivially greppable
    #: and trivially editable. Never read the override from here: an override
    #: that can live in a config file is not an override, it is a new default.
    if test -r "${gcp_gpu_budget_config}" ; then
        local cap
        IFS= read -r cap < "${gcp_gpu_budget_config}" || true
        cap="${cap%%[[:space:]]#}"
        if [[ "$cap" == <->(.<->)# ]] ; then
            ec "$cap"
            return 0
        fi
    fi

    ec "${gcp_gpu_budget_default}"
}

function gcp-gpu-budget-set {
    local cap="${1:?usage: gcp-gpu-budget-set EUR}"
    assert-args cap @RET

    ensure-dir "${gcp_gpu_budget_config}" @RET
    ec "$cap" > "${gcp_gpu_budget_config}" @RET

    ec "budget cap set to EUR ${cap}/month  (${gcp_gpu_budget_config})"
}

function h-gcp-gpu-budget-ok-p {
    #: Predicate. Prints nothing, returns status only.
    if bool "${GCP_GPU_BUDGET_OVERRIDE}" ; then
        return 0
    fi

    local cap spent
    cap="$(h-gcp-gpu-budget-cap)" || return 0
    spent="$(h-gcp-gpu-spend-total "$(h-gcp-gpu-month-start)" "$EPOCHSECONDS")" || return 0

    (( spent < cap ))
}

function gcp-gpu-override {
    export GCP_GPU_BUDGET_OVERRIDE=1

    ecerr "GCP_GPU_BUDGET_OVERRIDE=1 exported for THIS SHELL ONLY."
    ecerr "The soft cap will not stop 'gcp-gpu-up' until you close this shell or unset it."
    ecerr "Leave a note to yourself about why you escalated; future-you will ask."
}

function gcp-gpu-budget {
    h-gcp-gpu-deps @RET

    local cap month_start month_end spent
    cap="$(h-gcp-gpu-budget-cap)"
    month_start="$(h-gcp-gpu-month-start)"
    month_end="$(h-gcp-gpu-month-end)"
    spent="$(h-gcp-gpu-spend-total "$month_start" "$EPOCHSECONDS")"

    local remaining
    remaining="$(printf '%.2f' $(( cap - spent )))"

    local -F elapsed_days days_left
    elapsed_days=$(( (EPOCHSECONDS - month_start) / 86400.0 ))
    days_left=$(( (month_end - EPOCHSECONDS) / 86400.0 ))

    local projected burn
    burn="$(gcp-gpu-burn --bare)"
    if (( elapsed_days > 0 )) ; then
        #: Run-rate so far, carried forward, plus whatever is burning right now.
        projected="$(printf '%.2f' $(( spent / elapsed_days * (elapsed_days + days_left) + burn * 24 * days_left )))"
    else
        projected="$(printf '%.2f' $(( spent )))"
    fi

    print -r -- "cap            EUR ${cap} / month  (${gcp_gpu_budget_config})"
    printf   'month-to-date  EUR %.2f\n' "$spent"
    print -r -- "remaining      EUR ${remaining}"
    printf   'days left      %.1f\n' "$days_left"
    print -r -- "projected     ~EUR ${projected}  (run-rate + current burn)"

    if (( spent >= cap )) ; then
        ecerr "OVER CAP. 'gcp-gpu-up' will refuse until the month rolls over."
        ecerr "Deliberate escalation: GCP_GPU_BUDGET_OVERRIDE=1 gcp-gpu-up"
    fi
    h-gcp-gpu-spend-source-note
}

function gcp-gpu-spend {
    h-gcp-gpu-deps @RET

    local from to label="month"
    case "${1}" in
        --today)
            from="$(h-gcp-gpu-day-start)" ; label=today ;;
        --month|'')
            from="$(h-gcp-gpu-month-start)" ; label=month ;;
        *)
            ecerr "$0: usage: $0 [--today|--month]"
            return 1 ;;
    esac
    to="$EPOCHSECONDS"

    local rows
    if rows="$(h-gcp-gpu-spend-bq "$from" "$to")" && test -n "$rows" ; then
        typeset -g gcp_gpu_spend_source=bigquery
    else
        if h-gcp-gpu-billing-table >/dev/null 2>&1 ; then
            typeset -g gcp_gpu_spend_source=bigquery
            rows=''
        else
            rows="$(h-gcp-gpu-spend-audit "$from" "$to")"
            typeset -g gcp_gpu_spend_source=audit-estimate
        fi
    fi

    ec "spend this ${label}, owner=${gcp_gpu_owner}:"
    if test -z "$rows" ; then
        ec "  (nothing)"
    else
        ec "$rows" | command awk -F'\t' '{ printf "  %-34s EUR %8.2f\n", $1, $2 ; s += $2 }
                                          END { printf "  %-34s EUR %8.2f\n", "TOTAL", s + 0 }'
    fi
    h-gcp-gpu-spend-source-note
}
##
function h-gcp-gpu-stockout-p {
    #: Spot VMs do not queue. Creation fails immediately, and "waiting for a
    #: spot VM" means retrying in a loop rather than an API-side queue.
    [[ "${1}" == *(ZONE_RESOURCE_POOL_EXHAUSTED|does not have enough resources|resource pool exhausted|QUOTA_EXCEEDED_STOCKOUT)* ]]
}

function h-gcp-gpu-retry {
    #: Retries `"$@"` only on a stockout, in a single zone -- the data disk is
    #: zonal and cannot follow the VM elsewhere, so a cross-zone retry loop
    #: would silently produce an instance with no data.
    integer attempt=1
    local out

    while true ; do
        if out="$(reval "$@" 2>&1)" ; then
            test -n "$out" && ec "$out"
            return 0
        fi

        if ! h-gcp-gpu-stockout-p "$out" ; then
            ecerr "$out"
            return 1
        fi

        if (( attempt >= gcp_gpu_retry_max )) ; then
            ecerr "$0: still no capacity in ${gcp_gpu_zone} after ${attempt} attempts. Giving up."
            ecerr "Try --on-demand, or a different machine type."
            return 1
        fi

        ecgray "[${attempt}/${gcp_gpu_retry_max}] no spot capacity in ${gcp_gpu_zone}; retrying in ${gcp_gpu_retry_sleep}s ..."
        sleep "${gcp_gpu_retry_sleep}"
        attempt=$(( attempt + 1 ))
    done
}
##
function h-gcp-gpu-max-run-min {
    #: `--max-run-duration` as minutes. Accepts `8h`, `90m`, `3600s` or a bare
    #: number of seconds, which is what gcloud itself takes.
    local v="${gcp_gpu_max_run}"

    case "$v" in
        *h) ec $(( ${v%h} * 60 )) ;;
        *m) ec "${v%m}" ;;
        *s) ec $(( ${v%s} / 60 )) ;;
        *)  ec $(( v / 60 )) ;;
    esac
}

function h-gcp-gpu-deadline-min {
    #: The on-VM absolute deadline sits one grace period beyond GCE's own
    #: ceiling, so it only ever fires when --max-run-duration did not.
    local m
    m="$(h-gcp-gpu-max-run-min)" || return $?

    ec $(( m + gcp_gpu_reap_grace_min ))
}
##
function h-gcp-gpu-cpu-util-max {
    #: `h-gcp-gpu-cpu-util-max INSTANCE_ID MINUTES` -> peak mean CPU percent.
    #:
    #: Cloud Monitoring, not ssh and not the Ops Agent: CPU utilization is a
    #: built-in Compute metric that needs no agent installed, and it keeps
    #: working when the box is wedged -- exactly the case an outside-in reaper
    #: exists for.
    #:
    #: Peak-of-means rather than mean-of-means: one busy sample anywhere in the
    #: window must be enough to call the box "not idle".
    local id="${1:?}" minutes="${2:?}"

    local token
    token="$(command gcloud auth print-access-token 2>/dev/null)" || return 1
    test -n "$token" || return 1

    local start end
    local -x TZ=UTC
    strftime -s end   '%Y-%m-%dT%H:%M:%SZ' $EPOCHSECONDS || return $?
    strftime -s start '%Y-%m-%dT%H:%M:%SZ' $(( EPOCHSECONDS - minutes * 60 )) || return $?

    local url="https://monitoring.googleapis.com/v3/projects/${gcp_gpu_project}/timeSeries"
    command curl -s --max-time 30 -H "Authorization: Bearer ${token}" -G "$url" \
        --data-urlencode "filter=metric.type=\"compute.googleapis.com/instance/cpu/utilization\" AND resource.labels.instance_id=\"${id}\"" \
        --data-urlencode "interval.startTime=${start}" \
        --data-urlencode "interval.endTime=${end}" \
        --data-urlencode "aggregation.alignmentPeriod=300s" \
        --data-urlencode "aggregation.perSeriesAligner=ALIGN_MEAN" \
        2>/dev/null \
        | command jq -r '[.timeSeries[]?.points[]?.value.doubleValue] | if length == 0 then "" else (max * 100) end'
}

function h-gcp-gpu-heartbeat {
    #: `h-gcp-gpu-heartbeat [NAME] [ZONE]` -> the guest attribute the on-VM idle
    #: checker publishes each minute, or empty if absent/unreachable.
    local name="${1:-${gcp_gpu_instance}}" zone="${2:-${gcp_gpu_zone}}"

    h-gcp-gpu-gcloud compute instances get-guest-attributes "$name" \
        --zone="$zone" --query-path=gcp-gpu/status \
        --format='value(value)' 2>/dev/null
}

function h-gcp-gpu-heartbeat-age {
    #: Seconds since the last heartbeat, or empty when there is none.
    local hb ts
    hb="$(h-gcp-gpu-heartbeat "$@")" || return 0
    test -n "$hb" || return 0

    ts="${${(M)${(z)hb}:#ts=*}#ts=}"
    test -n "$ts" || return 0

    ec $(( EPOCHSECONDS - ts ))
}
##
function h-gcp-gpu-startup-script {
    #: Runs as root on every boot. Idempotent by construction: each stage
    #: checks for its own completion, because a preempted spot VM re-runs this
    #: on resume.
    cat <<EOF
#!/bin/bash
GCP_GPU_DATA_DEVICE="/dev/disk/by-id/google-${gcp_gpu_data_disk}"
GCP_GPU_BUCKET="${gcp_gpu_bucket}"
GCP_GPU_IDLE_MIN="${gcp_gpu_idle_min}"
GCP_GPU_DEADLINE_MIN="$(h-gcp-gpu-deadline-min)"
EOF

    cat <<'GCP_GPU_STARTUP_EOF'
set -uo pipefail
exec > >(tee -a /var/log/gcp-gpu-startup.log) 2>&1
echo "=== gcp-gpu startup $(date -Is) ==="

## data disk -------------------------------------------------------------
mkdir -p /mnt/data
if [ -b "$GCP_GPU_DATA_DEVICE" ]; then
    if ! blkid "$GCP_GPU_DATA_DEVICE" >/dev/null 2>&1 ; then
        echo "formatting $GCP_GPU_DATA_DEVICE (first boot only)"
        mkfs.ext4 -m 0 -E lazy_itable_init=0,lazy_journal_init=0,discard "$GCP_GPU_DATA_DEVICE"
    fi
    if ! mountpoint -q /mnt/data ; then
        mount -o discard,defaults "$GCP_GPU_DATA_DEVICE" /mnt/data
    fi
    if ! grep -q '^/dev/disk/by-id/google-' /etc/fstab ; then
        echo "$GCP_GPU_DATA_DEVICE /mnt/data ext4 discard,defaults,nofail 0 2" >> /etc/fstab
    fi
    mkdir -p /mnt/data/runs /mnt/data/venvs
    #: The login user owns it; nothing here should need root.
    for u in $(ls /home 2>/dev/null) ; do chown -R "$u:$u" /mnt/data || true ; done
else
    echo "WARN: $GCP_GPU_DATA_DEVICE absent; /mnt/data is on the BOOT disk and will not survive deletion"
fi

## nvidia driver ---------------------------------------------------------
#: G2 machine types cannot use the Deep Learning VM images, so the driver is
#: ours to install. This is the one stage that can take ~10 minutes.
if ! command -v nvidia-smi >/dev/null 2>&1 ; then
    echo "installing NVIDIA driver"
    export DEBIAN_FRONTEND=noninteractive
    apt-get update -y
    apt-get install -y python3 pciutils gcc make dkms "linux-headers-$(uname -r)" curl
    curl -fsSL -o /opt/install_gpu_driver.py \
        https://raw.githubusercontent.com/GoogleCloudPlatform/compute-gpu-installation/main/linux/install_gpu_driver.py
    python3 /opt/install_gpu_driver.py --force || echo "WARN: driver install returned non-zero"
fi

## tooling ---------------------------------------------------------------
if ! command -v tmux >/dev/null 2>&1 ; then
    DEBIAN_FRONTEND=noninteractive apt-get install -y tmux git rsync
fi

## idle auto-shutdown ----------------------------------------------------
cat > /usr/local/bin/gcp-gpu-idle-check <<'IDLE_EOF'
#!/bin/bash
#: Shuts the box down after $IDLE_THRESHOLD_MIN consecutive minutes with GPU
#: utilization <= $GPU_UTIL_MAX *and* no attached tmux client. Both thresholds
#: are set in the systemd unit; override them there.
set -uo pipefail
: "${IDLE_THRESHOLD_MIN:=30}"
: "${GPU_UTIL_MAX:=0}"
: "${IDLE_STATE_FILE:=/var/lib/gcp-gpu-idle.count}"

util=0
if command -v nvidia-smi >/dev/null 2>&1 ; then
    util=$(nvidia-smi --query-gpu=utilization.gpu --format=csv,noheader,nounits 2>/dev/null \
           | sort -rn | head -1)
    util=${util:-0}
fi

#: tmux runs as the login user, so root cannot see it via `tmux list-clients`
#: alone; walk the sockets instead.
attached=0
for sock in /tmp/tmux-*/default ; do
    [ -S "$sock" ] || continue
    n=$(tmux -S "$sock" list-clients 2>/dev/null | wc -l)
    attached=$(( attached + n ))
done

count=0
[ -r "$IDLE_STATE_FILE" ] && count=$(cat "$IDLE_STATE_FILE" 2>/dev/null || echo 0)

if [ "$util" -le "$GPU_UTIL_MAX" ] && [ "$attached" -eq 0 ] ; then
    count=$(( count + 1 ))
else
    count=0
fi
echo "$count" > "$IDLE_STATE_FILE"

#: Publish a heartbeat the outside world can read without ssh. This is what
#: lets `gcp-gpu-reap` tell "the timer is alive and deliberately not shutting
#: down, because someone is attached" from "the timer is dead". Without it, a
#: reaper cannot distinguish a human sitting at a quiet prompt from a wedged
#: box, and would kill the human.
curl -s -X PUT --max-time 5 \
    --data "ts=$(date +%s) idle=${count} gpu=${util} clients=${attached}" \
    -H "Metadata-Flavor: Google" \
    "http://metadata.google.internal/computeMetadata/v1/instance/guest-attributes/gcp-gpu/status" \
    >/dev/null 2>&1 || true

if [ "$count" -ge "$IDLE_THRESHOLD_MIN" ] ; then
    logger -t gcp-gpu-idle "idle ${count}m (gpu=${util}% clients=${attached}); shutting down"
    /sbin/shutdown -h now
fi
IDLE_EOF
chmod +x /usr/local/bin/gcp-gpu-idle-check

cat > /etc/systemd/system/gcp-gpu-idle.service <<IDLESVC_EOF
[Unit]
Description=Shut down when the GPU has been idle and no tmux client is attached

[Service]
Type=oneshot
Environment=IDLE_THRESHOLD_MIN=${GCP_GPU_IDLE_MIN}
Environment=GPU_UTIL_MAX=0
ExecStart=/usr/local/bin/gcp-gpu-idle-check
IDLESVC_EOF

cat > /etc/systemd/system/gcp-gpu-idle.timer <<'IDLETMR_EOF'
[Unit]
Description=Run the idle check every minute

[Timer]
OnBootSec=10min
OnUnitActiveSec=1min
AccuracySec=10s

[Install]
WantedBy=timers.target
IDLETMR_EOF

systemctl daemon-reload
systemctl enable --now gcp-gpu-idle.timer

## absolute deadline backstop ------------------------------------------
#: Independent of both the idle timer above and GCE's --max-run-duration.
#: Those are the two things that normally stop this box; this fires only if
#: BOTH failed, and it is deliberately the dumbest mechanism available -- a
#: single `shutdown` scheduled at boot, with no dependency on nvidia-smi, the
#: metadata server, or a working network.
if [ -n "${GCP_GPU_DEADLINE_MIN:-}" ] && [ "${GCP_GPU_DEADLINE_MIN}" -gt 0 ] 2>/dev/null ; then
    #: `shutdown -c` first: a resumed spot VM re-runs this script, and without
    #: cancelling we would stack schedules from the previous boot.
    shutdown -c >/dev/null 2>&1 || true
    shutdown -h "+${GCP_GPU_DEADLINE_MIN}" \
        "gcp-gpu: absolute deadline reached (${GCP_GPU_DEADLINE_MIN}m since boot)" >/dev/null 2>&1 || true
    echo "absolute deadline armed: shutdown in ${GCP_GPU_DEADLINE_MIN} minutes"
fi

echo "=== gcp-gpu startup done $(date -Is) ==="
GCP_GPU_STARTUP_EOF
}
##
function h-gcp-gpu-ensure-sa {
    if h-gcp-gpu-sa-exists-p ; then
        return 0
    fi

    ec "creating service account ${gcp_gpu_sa}"
    h-gcp-gpu-reval h-gcp-gpu-gcloud iam service-accounts create gpu-runner \
        --display-name="GPU workstation runner (${gcp_gpu_owner})" @RET
}

function h-gcp-gpu-ensure-bucket {
    if h-gcp-gpu-bucket-exists-p ; then
        return 0
    fi

    ec "creating bucket ${gcp_gpu_bucket} in ${gcp_gpu_region}"
    h-gcp-gpu-reval command gcloud storage buckets create "${gcp_gpu_bucket}" \
        --project="${gcp_gpu_project}" \
        --location="${gcp_gpu_region}" \
        --uniform-bucket-level-access @RET

    #: On the bucket only. A project-wide grant would hand every VM in a shared
    #: lab project write access to my results.
    h-gcp-gpu-reval command gcloud storage buckets add-iam-policy-binding "${gcp_gpu_bucket}" \
        --project="${gcp_gpu_project}" \
        --member="serviceAccount:${gcp_gpu_sa}" \
        --role=roles/storage.objectAdmin @RET
}

function h-gcp-gpu-ensure-disk {
    if h-gcp-gpu-disk-exists-p ; then
        return 0
    fi

    local monthly
    monthly="$(h-gcp-gpu-disk-price "${gcp_gpu_data_type}" "${gcp_gpu_data_gb}")"
    ec "creating data disk ${gcp_gpu_data_disk} (${gcp_gpu_data_gb}GB ${gcp_gpu_data_type}, ~EUR ${monthly}/month, billed whether or not the VM runs)"

    h-gcp-gpu-reval h-gcp-gpu-gcloud compute disks create "${gcp_gpu_data_disk}" \
        --zone="${gcp_gpu_zone}" \
        --size="${gcp_gpu_data_gb}GB" \
        --type="${gcp_gpu_data_type}" \
        --labels="$(h-gcp-gpu-labels)" @RET
}
##
function gcp-gpu-up {
    : "usage: gcp-gpu-up [--on-demand] [--machine TYPE] [--dry-run]"
    h-gcp-gpu-deps @RET

    local model=SPOT machine="${gcp_gpu_machine}" gcp_gpu_dry_run="${gcp_gpu_dry_run}"
    #: Kept because the arg loop shifts `$@` away, and the override hint below
    #: has to echo back what was actually typed to be copy-pasteable.
    local -a orig_args=( "$@" )
    while (( $# )) ; do
        case "$1" in
            --on-demand) model=STANDARD ; shift ;;
            --spot)      model=SPOT ; shift ;;
            --machine)   machine="${2:?--machine needs a type}" ; shift 2 ;;
            --dry-run)   gcp_gpu_dry_run=y ; shift ;;
            *) ecerr "$0: unknown argument: $1" ; return 1 ;;
        esac
    done

    ## preflight ---------------------------------------------------------
    if ! h-gcp-gpu-budget-ok-p ; then
        local cap spent
        cap="$(h-gcp-gpu-budget-cap)"
        spent="$(h-gcp-gpu-spend-total "$(h-gcp-gpu-month-start)" "$EPOCHSECONDS")"

        ecerr "$0: REFUSING -- over the monthly cap."
        ecerr "  month-to-date  EUR $(printf '%.2f' "$spent")"
        ecerr "  cap            EUR ${cap}   (${gcp_gpu_budget_config})"
        ecerr ""
        ecerr "  Raise the cap:  gcp-gpu-budget-set 100"
        ecerr "  Or escalate once, for this shell only:"
        ecerr "      GCP_GPU_BUDGET_OVERRIDE=1 $0 ${orig_args[*]}"
        h-gcp-gpu-spend-source-note
        return 1
    fi
    if bool "${GCP_GPU_BUDGET_OVERRIDE}" ; then
        ecerr "NOTE: budget override is active; the soft cap was not consulted."
    fi

    ## idempotence -------------------------------------------------------
    local json status
    json="$(h-gcp-gpu-instance-json)"
    if test -n "$json" ; then
        status="$(ec "$json" | command jq -r '.status')"
        case "$status" in
            RUNNING)
                ec "${gcp_gpu_instance} is already RUNNING in ${gcp_gpu_zone}. Nothing to do."
                gcp-gpu-status
                return 0 ;;
            TERMINATED|SUSPENDED)
                ec "${gcp_gpu_instance} exists but is ${status}; starting it."
                #: Resuming can hit a stockout exactly like creation can.
                h-gcp-gpu-retry h-gcp-gpu-reval h-gcp-gpu-gcloud compute instances start \
                    "${gcp_gpu_instance}" --zone="${gcp_gpu_zone}" @RET
                ec "started ${gcp_gpu_instance}."
                return 0 ;;
            *)
                ec "${gcp_gpu_instance} is ${status}; leaving it alone."
                return 0 ;;
        esac
    fi

    ## prerequisites -----------------------------------------------------
    h-gcp-gpu-ensure-sa @RET
    h-gcp-gpu-ensure-bucket @RET
    h-gcp-gpu-ensure-disk @RET

    local startup
    startup="$(mktemp)" @RET
    h-gcp-gpu-startup-script > "$startup" @RET

    local rate
    rate="$(h-gcp-gpu-price "$machine" "$model")"
    ec "creating ${gcp_gpu_instance}: ${machine}, ${model}, ${gcp_gpu_zone}, ~EUR ${rate}/hr, max ${gcp_gpu_max_run}"

    local -a opts
    opts=(
        compute instances create "${gcp_gpu_instance}"
        --zone="${gcp_gpu_zone}"
        --machine-type="${machine}"
        --maintenance-policy=TERMINATE
        --provisioning-model="${model}"
        #: The single most reliable protection here, and it needs nobody's
        #: approval: a hard wall-clock ceiling enforced by GCE itself.
        --max-run-duration="${gcp_gpu_max_run}"
        --instance-termination-action=STOP
        --image-family="${gcp_gpu_image_family}"
        --image-project="${gcp_gpu_image_project}"
        --boot-disk-size="${gcp_gpu_boot_gb}GB"
        --boot-disk-type="${gcp_gpu_boot_type}"
        --disk="name=${gcp_gpu_data_disk},device-name=${gcp_gpu_data_disk},mode=rw,auto-delete=no"
        --service-account="${gcp_gpu_sa}"
        --scopes=https://www.googleapis.com/auth/devstorage.read_write,https://www.googleapis.com/auth/logging.write,https://www.googleapis.com/auth/monitoring.write
        --labels="$(h-gcp-gpu-labels)"
        #: Lets the idle checker publish the heartbeat that `gcp-gpu-reap`
        #: reads to tell a live-but-deliberately-quiet box from a dead timer.
        --metadata=enable-guest-attributes=TRUE
        --metadata-from-file="startup-script=${startup}"
    )

    h-gcp-gpu-retry h-gcp-gpu-reval h-gcp-gpu-gcloud "${opts[@]}"
    local ret=$?

    command rm -f -- "$startup"
    if (( ret != 0 )) ; then
        return $ret
    fi

    ec ""
    ec "created. The first boot installs the NVIDIA driver and takes ~10 minutes."
    ec "  watch it:   gcp-gpu-ssh 'sudo tail -f /var/log/gcp-gpu-startup.log'"
    ec "  then:       gcp-gpu-attach"
}

function gcp-gpu-down {
    h-gcp-gpu-deps @RET

    if ! h-gcp-gpu-instance-exists-p ; then
        ec "${gcp_gpu_instance} does not exist in ${gcp_gpu_zone}. Nothing to stop."
        return 0
    fi

    h-gcp-gpu-reval h-gcp-gpu-gcloud compute instances stop \
        "${gcp_gpu_instance}" --zone="${gcp_gpu_zone}" @RET

    ec "stopped ${gcp_gpu_instance}. Boot disk, /mnt/data, drivers and packages all survive."
    ec "Disks keep billing while stopped: gcp-gpu-disks"
}

function gcp-gpu-destroy {
    h-gcp-gpu-deps @RET

    if ! h-gcp-gpu-instance-exists-p ; then
        ec "${gcp_gpu_instance} does not exist in ${gcp_gpu_zone}. Nothing to delete."
        return 0
    fi

    ec "About to DELETE instance ${gcp_gpu_instance} (${gcp_gpu_zone})."
    ec "  --keep-disks=all : ${gcp_gpu_data_disk} and the boot disk both survive and keep billing."
    ask "Delete ${gcp_gpu_instance}?" n || {
        ec "aborted."
        return 1
    }

    h-gcp-gpu-reval h-gcp-gpu-gcloud compute instances delete \
        "${gcp_gpu_instance}" --zone="${gcp_gpu_zone}" --keep-disks=all --quiet @RET

    ec "deleted ${gcp_gpu_instance}. Disks kept -- 'gcp-gpu-disks' to see what is still billing."
}
##
function gcp-gpu-ssh {
    h-gcp-gpu-deps @RET

    if (( $# )) ; then
        h-gcp-gpu-gcloud compute ssh "${gcp_gpu_instance}" --zone="${gcp_gpu_zone}" -- "$@"
    else
        h-gcp-gpu-gcloud compute ssh "${gcp_gpu_instance}" --zone="${gcp_gpu_zone}"
    fi
}

function gcp-gpu-attach {
    h-gcp-gpu-deps @RET

    #: `-A` attaches if it exists and creates it otherwise, so this is the only
    #: command needed either way.
    h-gcp-gpu-gcloud compute ssh "${gcp_gpu_instance}" --zone="${gcp_gpu_zone}" -- \
        -t "tmux new-session -A -s ${gcp_gpu_tmux_session}"
}

function gcp-gpu-sync {
    h-gcp-gpu-deps @RET

    ec "syncing /mnt/data/runs/ -> ${gcp_gpu_bucket}/runs/"
    gcp-gpu-ssh "gcloud storage rsync -r /mnt/data/runs/ ${gcp_gpu_bucket}/runs/" @RET

    ec "synced."
}
##
function h-gcp-gpu-last-stop-reason {
    #: Distinguishes "I stopped it" from "it was preempted". The operations log
    #: is authoritative; guest attributes do not survive a stop.
    local ops
    ops="$(h-gcp-gpu-gcloud compute operations list \
        --filter="targetLink~${gcp_gpu_instance}$" \
        --format=json 2>/dev/null)"

    if test -z "$ops" ; then
        ec unknown
        return 0
    fi

    ec "$ops" | command jq -r '
        [ .[]? | select(.operationType | test("preempt|stop|terminate|delete")) ]
        | sort_by(.insertTime) | last
        | if . == null then "unknown"
          elif (.operationType | test("preempt")) then "PREEMPTED"
          elif (.operationType | test("stop")) then "stopped (by you or by max-run-duration/idle timer)"
          else .operationType end'
}

function h-gcp-gpu-idle-timer-state {
    #: Best-effort: needs ssh, so it degrades to `unknown` rather than failing.
    local out
    out="$(gcp-gpu-ssh 'systemctl is-active gcp-gpu-idle.timer' 2>/dev/null | command tr -d '[:space:]')"

    ec "${out:-unknown}"
}

function gcp-gpu-status {
    h-gcp-gpu-deps @RET

    local json
    json="$(h-gcp-gpu-instance-json)"

    ec "instance       ${gcp_gpu_instance}  (${gcp_gpu_zone}, ${gcp_gpu_project})"

    if test -z "$json" ; then
        ec "state          ABSENT -- no instance by that name in this zone"
        ec "burn           EUR 0.00/hr"
    else
        local status machine model gpu started
        status="$(ec "$json"  | command jq -r '.status')"
        machine="$(ec "$json" | command jq -r '.machineType | split("/") | last')"
        model="$(ec "$json"   | command jq -r '.scheduling.provisioningModel // "STANDARD"')"
        gpu="$(ec "$json"     | command jq -r '[.guestAccelerators[]? | "\(.acceleratorCount)x \(.acceleratorType | split("/") | last)"] | join(", ") // ""')"
        started="$(ec "$json" | command jq -r '.lastStartTimestamp // empty')"

        ec "state          ${status}"
        ec "machine        ${machine}   ${model}"
        ec "gpu            ${gpu:-(none reported)}"

        if [[ "$status" == RUNNING ]] && test -n "$started" ; then
            local epoch
            strftime -r -s epoch '%Y-%m-%dT%H:%M:%S' "${started%.*}" 2>/dev/null \
                && ec "uptime         $(h-gcp-gpu-dur-human $(( EPOCHSECONDS - epoch )))"
        fi

        if [[ "$status" == TERMINATED ]] ; then
            ec "why            $(h-gcp-gpu-last-stop-reason)"
        fi

        if [[ "$status" == RUNNING ]] ; then
            ec "idle timer     $(h-gcp-gpu-idle-timer-state)  (threshold ${gcp_gpu_idle_min}m)"
            ec "burn           EUR $(h-gcp-gpu-price "$machine" "$model")/hr"
        else
            ec "burn           EUR 0.00/hr  (disks still bill: gcp-gpu-disks)"
        fi
    fi

    ec ""
    local cap spent
    cap="$(h-gcp-gpu-budget-cap)"
    spent="$(h-gcp-gpu-spend-total "$(h-gcp-gpu-month-start)" "$EPOCHSECONDS")"
    printf 'month-to-date  EUR %.2f  of EUR %s  (EUR %.2f left)\n' \
        "$spent" "$cap" $(( cap - spent ))
    h-gcp-gpu-spend-source-note
}
##
function gcp-gpu-burn {
    #: `--bare` prints only the number, for arithmetic in other functions.
    h-gcp-gpu-deps @RET
    local bare=''
    [[ "${1}" == --bare ]] && bare=y

    local rows
    rows="$(h-gcp-gpu-gcloud compute instances list \
        --filter="$(h-gcp-gpu-label-filter) AND status=RUNNING" --format=json 2>/dev/null \
        | command jq -r '.[]? | [.name, (.machineType | split("/") | last), (.scheduling.provisioningModel // "STANDARD")] | @tsv')"

    local -F total=0
    local name machine model rate
    if test -n "$rows" ; then
        while IFS=$'\t' read -r name machine model ; do
            test -z "$name" && continue
            rate="$(h-gcp-gpu-price "$machine" "$model")"
            total=$(( total + rate ))
            bool "$bare" || printf '  %-20s %-16s %-9s EUR %5.2f/hr\n' "$name" "$machine" "$model" "$rate"
        done <<< "$rows"
    fi

    if bool "$bare" ; then
        printf '%.4f\n' "$total"
        return 0
    fi

    if (( total == 0 )) ; then
        ec "nothing running under owner=${gcp_gpu_owner}."
    fi
    printf 'TOTAL %.2f EUR/hr  (%.2f EUR/day if left running)\n' "$total" $(( total * 24 ))
    ecgray "compute only; disks bill separately -- gcp-gpu-disks"
}

function gcp-gpu-ps {
    h-gcp-gpu-deps @RET

    local rows
    rows="$(h-gcp-gpu-gcloud compute instances list \
        --filter="$(h-gcp-gpu-label-filter)" --format=json 2>/dev/null \
        | command jq -r '.[]? | [.name, (.zone | split("/") | last), .status,
                                 (.machineType | split("/") | last),
                                 (.scheduling.provisioningModel // "STANDARD"),
                                 (.lastStartTimestamp // "")] | @tsv')"

    if test -z "$rows" ; then
        ec "no instances labeled owner=${gcp_gpu_owner} anywhere in ${gcp_gpu_project}."
        return 0
    fi

    printf '%-18s %-18s %-12s %-16s %-9s %s\n' NAME ZONE STATE MACHINE MODEL UPTIME
    local name zone status machine model started epoch up
    while IFS=$'\t' read -r name zone status machine model started ; do
        test -z "$name" && continue
        up='-'
        if [[ "$status" == RUNNING ]] && test -n "$started" ; then
            strftime -r -s epoch '%Y-%m-%dT%H:%M:%S' "${started%.*}" 2>/dev/null \
                && up="$(h-gcp-gpu-dur-human $(( EPOCHSECONDS - epoch )))"
        fi
        printf '%-18s %-18s %-12s %-16s %-9s %s\n' "$name" "$zone" "$status" "$machine" "$model" "$up"
    done <<< "$rows"
}

function gcp-gpu-disks {
    #: The standing cost floor, and the thing I will forget about.
    h-gcp-gpu-deps @RET

    local rows
    rows="$(h-gcp-gpu-gcloud compute disks list \
        --filter="$(h-gcp-gpu-label-filter)" --format=json 2>/dev/null \
        | command jq -r '.[]? | [.name, (.zone | split("/") | last), .sizeGb,
                                 (.type | split("/") | last),
                                 (if (.users | length) > 0 then (.users[0] | split("/") | last) else "-" end)] | @tsv')"

    if test -z "$rows" ; then
        ec "no disks labeled owner=${gcp_gpu_owner}."
        return 0
    fi

    printf '%-18s %-18s %6s %-20s %-18s %s\n' NAME ZONE GB TYPE ATTACHED-TO EUR/MONTH
    local -F total=0
    local name zone gb type users cost
    while IFS=$'\t' read -r name zone gb type users ; do
        test -z "$name" && continue
        cost="$(h-gcp-gpu-disk-price "$type" "$gb")"
        total=$(( total + cost ))
        printf '%-18s %-18s %6s %-20s %-18s %s\n' "$name" "$zone" "$gb" "$type" "$users" "$cost"
    done <<< "$rows"

    printf 'TOTAL %.2f EUR/month -- billed whether or not anything is running.\n' "$total"
}

function gcp-gpu-quota {
    h-gcp-gpu-deps @RET
    local region="${1:-${gcp_gpu_region}}"

    ec "GPU quota, ${region}  (usage/limit)"
    printf '%-24s %14s %14s\n' ACCELERATOR ON-DEMAND SPOT

    h-gcp-gpu-gcloud compute regions describe "$region" --format=json \
        | command jq -r '
            [ .quotas[] | select(.metric | test("^(PREEMPTIBLE_)?NVIDIA_.*_GPUS$")) ]
            | map({ name: (.metric | sub("^PREEMPTIBLE_"; "") | sub("^NVIDIA_"; "") | sub("_GPUS$"; "")),
                    spot: (.metric | startswith("PREEMPTIBLE_")),
                    v: "\(.usage | floor)/\(.limit | floor)" })
            | group_by(.name)
            | map({ name: .[0].name,
                    od:   ([ .[] | select(.spot | not) | .v ] | first // "-"),
                    sp:   ([ .[] | select(.spot)       | .v ] | first // "-") })
            #: Sort by limit descending so the usable ones surface first.
            | sort_by( -( .od | split("/") | last | tonumber ) )
            | .[] | [.name, .od, .sp] | @tsv' \
        | command awk -F'\t' '{ printf "%-24s %14s %14s\n", $1, $2, $3 }'

    ecgray "note: A100_80GB has spot limit 0 in europe-west4 -- there is no spot path for it at all."
}

function gcp-gpu-avail {
    h-gcp-gpu-deps @RET

    local -a zones
    zones=( "$@" )
    if (( ${#zones} == 0 )) ; then
        zones=( europe-west4-a europe-west4-b europe-west4-c europe-west3-b )
    fi

    local z types
    for z in "${zones[@]}" ; do
        #: nvidia- only: the raw list also carries TPU types (`ct5lp`, `ct6e`),
        #: which are noise here. `-vws` are the virtual-workstation variants of
        #: cards already listed.
        types="$(h-gcp-gpu-gcloud compute accelerator-types list \
            --filter="zone:(${z})" --format='value(name)' 2>/dev/null \
            | command grep '^nvidia-' | command grep -v -- '-vws$' \
            | command sort | command paste -sd' ' -)"
        printf '%-18s %s\n' "$z" "${types:-(none / not queryable)}"
    done
}

function gcp-gpu-queue {
    h-gcp-gpu-deps @RET

    local pending mig
    pending="$(h-gcp-gpu-gcloud compute instances list \
        --filter="status=PROVISIONING OR status=STAGING" \
        --format='value(name,zone,status)' 2>/dev/null)"

    mig="$(h-gcp-gpu-gcloud compute instance-groups managed resize-requests list \
        --region="${gcp_gpu_region}" --format='value(name,state)' 2>/dev/null)"

    if test -z "$pending" && test -z "$mig" ; then
        ec "nothing queued."
        ecgray "Expected: spot VMs do not queue. Creation fails immediately with"
        ecgray "ZONE_RESOURCE_POOL_EXHAUSTED, and 'gcp-gpu-up' retries in a loop client-side."
        ecgray "A real queue needs a MIG resize request, which nothing here creates."
        return 0
    fi

    test -n "$pending" && { ec "instances coming up:" ; ec "$pending" }
    test -n "$mig"     && { ec "MIG resize requests:" ; ec "$mig" }
}
##
function h-gcp-gpu-verdict {
    #: `h-gcp-gpu-verdict NAME ZONE ID UPTIME_S` -> `CODE<TAB>explanation`.
    #:
    #: The whole design in one function. Two mechanisms already stop this box:
    #: GCE's --max-run-duration and the on-VM idle timer. This never second-
    #: guesses either -- it reports ANOMALY only when one of them is
    #: demonstrably broken, so it cannot misfire on work that is merely quiet.
    local name="${1:?}" zone="${2:?}" id="${3:?}"
    integer uptime_s="${4:?}"

    integer deadline_s=$(( ($(h-gcp-gpu-max-run-min) + gcp_gpu_reap_grace_min) * 60 ))
    if (( uptime_s > deadline_s )) ; then
        printf 'ANOMALY\t--max-run-duration=%s did not fire (up %s, %dm past the ceiling)\n' \
            "${gcp_gpu_max_run}" "$(h-gcp-gpu-dur-human $uptime_s)" \
            $(( (uptime_s - deadline_s) / 60 ))
        return 0
    fi

    #: A fresh heartbeat means the idle timer is alive. If it is alive and has
    #: not shut the box down, it has a reason -- an attached tmux client, or a
    #: busy GPU -- and that reason is better informed than anything visible
    #: from out here. Trust it and stop.
    local hb_age
    hb_age="$(h-gcp-gpu-heartbeat-age "$name" "$zone")"
    if test -n "$hb_age" && (( hb_age < gcp_gpu_reap_heartbeat_max_min * 60 )) ; then
        printf 'OK\tidle timer alive (heartbeat %ds ago); deferring to it\n' "$hb_age"
        return 0
    fi

    #: No usable heartbeat. Fall back to CPU: if the timer were working, a box
    #: this quiet would already be gone.
    local cpu
    cpu="$(h-gcp-gpu-cpu-util-max "$id" "${gcp_gpu_reap_idle_min}")"

    if test -z "$cpu" ; then
        printf 'UNKNOWN\tno heartbeat and no CPU samples yet; too new to judge\n'
        return 0
    fi

    if (( uptime_s < gcp_gpu_reap_idle_min * 60 )) ; then
        printf 'OK\tup only %s; less than the %dm idle window\n' \
            "$(h-gcp-gpu-dur-human $uptime_s)" "${gcp_gpu_reap_idle_min}"
        return 0
    fi

    if (( cpu < gcp_gpu_reap_cpu_pct )) ; then
        printf 'ANOMALY\tidle timer silent%s and CPU peaked at %.1f%% over %dm\n' \
            "${hb_age:+ (heartbeat ${hb_age}s old)}" "$cpu" "${gcp_gpu_reap_idle_min}"
        return 0
    fi

    printf 'OK\tno heartbeat, but CPU peaked at %.1f%% over %dm; something is running\n' \
        "$cpu" "${gcp_gpu_reap_idle_min}"
}

function gcp-gpu-idle {
    #: Read-only. Never stops anything.
    h-gcp-gpu-deps @RET

    local rows
    rows="$(h-gcp-gpu-gcloud compute instances list \
        --filter="$(h-gcp-gpu-label-filter) AND status=RUNNING" --format=json 2>/dev/null \
        | command jq -r '.[]? | [.name, (.zone | split("/") | last), .id,
                                 (.machineType | split("/") | last),
                                 (.lastStartTimestamp // "")] | @tsv')"

    if test -z "$rows" ; then
        ec "nothing running under owner=${gcp_gpu_owner}."
        return 0
    fi

    local name zone id machine started epoch verdict code why hb cpu
    integer uptime_s
    while IFS=$'\t' read -r name zone id machine started ; do
        test -z "$name" && continue

        uptime_s=0
        if test -n "$started" ; then
            strftime -r -s epoch '%Y-%m-%dT%H:%M:%S' "${started%.*}" 2>/dev/null \
                && uptime_s=$(( EPOCHSECONDS - epoch ))
        fi

        verdict="$(h-gcp-gpu-verdict "$name" "$zone" "$id" "$uptime_s")"
        code="${verdict%%$'\t'*}"
        why="${verdict#*$'\t'}"

        hb="$(h-gcp-gpu-heartbeat-age "$name" "$zone")"
        cpu="$(h-gcp-gpu-cpu-util-max "$id" 30)"

        ec "${name}  (${zone}, ${machine})"
        ec "  uptime      $(h-gcp-gpu-dur-human $uptime_s)  of ${gcp_gpu_max_run} ceiling"
        ec "  cpu peak    ${cpu:-n/a}% over the last 30m"
        ec "  heartbeat   ${hb:+${hb}s ago}${hb:-none -- on-VM idle timer is not reporting}"
        ec "  verdict     ${code}: ${why}"
    done <<< "$rows"

    ecgray "read-only. 'gcp-gpu-reap' acts on ANOMALY; 'gcp-gpu-reap --kill' stops them."
}

function gcp-gpu-reap {
    #: `gcp-gpu-reap [--kill]`. Default reports and changes nothing.
    #:
    #: Fires only when a safety net demonstrably failed, never on an idle
    #: threshold of its own -- the on-VM timer already owns that decision and
    #: is better informed. See `h-gcp-gpu-verdict`.
    h-gcp-gpu-deps @RET

    local kill_p=''
    case "${1}" in
        --kill) kill_p=y ;;
        '') ;;
        *) ecerr "$0: usage: $0 [--kill]" ; return 1 ;;
    esac

    local rows
    rows="$(h-gcp-gpu-gcloud compute instances list \
        --filter="$(h-gcp-gpu-label-filter) AND status=RUNNING" --format=json 2>/dev/null \
        | command jq -r '.[]? | [.name, (.zone | split("/") | last), .id,
                                 (.lastStartTimestamp // "")] | @tsv')"

    if test -z "$rows" ; then
        ec "nothing running under owner=${gcp_gpu_owner}. Nothing to reap."
        return 0
    fi

    local name zone id started epoch verdict code why
    integer uptime_s reaped=0
    while IFS=$'\t' read -r name zone id started ; do
        test -z "$name" && continue

        uptime_s=0
        if test -n "$started" ; then
            strftime -r -s epoch '%Y-%m-%dT%H:%M:%S' "${started%.*}" 2>/dev/null \
                && uptime_s=$(( EPOCHSECONDS - epoch ))
        fi

        verdict="$(h-gcp-gpu-verdict "$name" "$zone" "$id" "$uptime_s")"
        code="${verdict%%$'\t'*}"
        why="${verdict#*$'\t'}"

        if [[ "$code" != ANOMALY ]] ; then
            ec "${name}: ${code} -- ${why}"
            continue
        fi

        reaped=$(( reaped + 1 ))
        ecerr "${name}: ANOMALY -- ${why}"

        if bool "$kill_p" ; then
            h-gcp-gpu-reval h-gcp-gpu-gcloud compute instances stop \
                "$name" --zone="$zone" --quiet
            ec "${name}: stopped."
        else
            ec "${name}: would stop it. Re-run with --kill, or: gcp-gpu-down"
        fi
    done <<< "$rows"

    if (( reaped == 0 )) ; then
        ec "no anomalies. Every running instance is accounted for."
    fi
}

function gcp-gpu-project-cost {
    #: Whole-project spend, not just mine -- read out of the budget-alert
    #: function's own logs, which print `costAmount` every ~30 minutes.
    #:
    #: This is a side effect of someone else's Cloud Function, not an interface
    #: anyone promised to keep. It is here because it is the only real spend
    #: figure available while the BigQuery export does not exist. If it stops
    #: returning anything, that function was changed or removed.
    h-gcp-gpu-deps @RET
    local days="${1:-30}"

    ec "project-wide cost (all seven editors), last ${days}d:"
    memoi_expire=1800 memoi_skiperr=y memoi-eval \
        h-gcp-gpu-gcloud logging read \
        'resource.type="cloud_run_revision" AND resource.labels.service_name="budget-notification-logger" AND textPayload:"Current cost"' \
        --freshness="${days}d" --limit=500 --format='value(timestamp,textPayload)' 2>/dev/null \
        | command awk '$1 ~ /^[0-9]{4}-[0-9]{2}-[0-9]{2}/ {
                         d = substr($1, 1, 10) ; c = $NF ; gsub(/[()]/, "", c)
                         if (!(d in seen)) { seen[d] = c ; print "  " d "  " c } }' \
        | command sort

    ecgray "budget-period totals, so the series resets when the period rolls over."
    ecgray "Your own share is 'gcp-gpu-spend'; this is the shared project."
}
##
typeset -g gcp_gpu_reaper_src="${gcp_gpu_reaper_src:-${HOME}/scripts/python/gcp/gpu_reaper}"
typeset -g gcp_gpu_reaper_fn="${gcp_gpu_reaper_fn:-gpu-reaper-${gcp_gpu_owner}}"
typeset -g gcp_gpu_reaper_role="${gcp_gpu_reaper_role:-gcpGpuReaper}"
typeset -g gcp_gpu_reaper_schedule="${gcp_gpu_reaper_schedule:-*/15 * * * *}"

function gcp-gpu-reaper-iam-cmds {
    #: Prints the setup this project needs but that nothing here will perform.
    #:
    #: Section 8 of the brief is absolute: never modify IAM policy. Enabling an
    #: API and minting a role on a SHARED lab project are exactly the changes
    #: six other editors would not expect, so they stay a deliberate act by a
    #: human who has read them.
    ##
    ec "# 1. Cloud Scheduler is not enabled on this project yet."
    ec "gcloud services enable cloudscheduler.googleapis.com --project=${gcp_gpu_project}"
    ec ""
    ec "# 2. A custom role with exactly the four permissions the reaper needs."
    ec "#    Deliberately NOT roles/compute.instanceAdmin.v1: that would let the"
    ec "#    function stop any instance in a project shared with six other people."
    ec "gcloud iam roles create ${gcp_gpu_reaper_role} --project=${gcp_gpu_project} \\"
    ec "  --title='GCP GPU reaper' \\"
    ec "  --permissions=compute.instances.list,compute.instances.stop,compute.instances.getGuestAttributes,monitoring.timeSeries.list"
    ec ""
    ec "# 3. Bind it to the runner SA, which today holds only objectAdmin on one bucket."
    ec "#    NOTE: this needs resourcemanager.projects.setIamPolicy, which is"
    ec "#    Owner-only. An editor cannot run it -- ask the project owner."
    ec "gcloud projects add-iam-policy-binding ${gcp_gpu_project} \\"
    ec "  --member=serviceAccount:${gcp_gpu_sa} \\"
    ec "  --role=projects/${gcp_gpu_project}/roles/${gcp_gpu_reaper_role}"
    ec ""
    ec "# 4. AFTER deploying: Cloud Scheduler calls the function with an OIDC token"
    ec "#    against --no-allow-unauthenticated, so the SA needs run.invoker too."
    ec "#    Scoped to the single service, not the project."
    ec "gcloud run services add-iam-policy-binding ${gcp_gpu_reaper_fn} \\"
    ec "  --region=${gcp_gpu_region} --project=${gcp_gpu_project} \\"
    ec "  --member=serviceAccount:${gcp_gpu_sa} --role=roles/run.invoker"
    ec ""
    ec "# Prerequisite: ${gcp_gpu_sa} is created by the first \`gcp-gpu-up\`."
    ec "# Every other API this needs (cloudfunctions, run, cloudbuild,"
    ec "# artifactregistry, monitoring) is already enabled on this project."
    ec ""
    ec "# Note: the role is still project-scoped, so the function COULD stop"
    ec "# anyone's instance. The code never does -- every query filters on"
    ec "# labels.owner=${gcp_gpu_owner} -- but the permission is broader than the"
    ec "# behaviour. GCE has no per-instance grant that survives recreation."
}

function h-gcp-gpu-reaper-ready-p {
    command gcloud services list --enabled --project="${gcp_gpu_project}" \
        --format='value(config.name)' 2>/dev/null \
        | command grep -qx 'cloudscheduler.googleapis.com'
}

function gcp-gpu-reaper-deploy {
    #: Deploys the server-side reaper. Refuses until the IAM work above is done,
    #: rather than half-deploying something that will fail at 03:00 with a
    #: permission error nobody is awake to read.
    h-gcp-gpu-deps @RET

    if ! test -f "${gcp_gpu_reaper_src}/main.py" ; then
        ecerr "$0: no function source at ${gcp_gpu_reaper_src}/main.py"
        return 1
    fi

    if ! h-gcp-gpu-reaper-ready-p ; then
        ecerr "$0: Cloud Scheduler is not enabled, and the reaper SA has no role yet."
        ecerr "These change a SHARED project, so run them yourself and re-run this:"
        ecerr ""
        gcp-gpu-reaper-iam-cmds >&2
        return 1
    fi

    local max_run_min
    max_run_min="$(h-gcp-gpu-max-run-min)" @RET

    ec "deploying ${gcp_gpu_reaper_fn} to ${gcp_gpu_region} (schedule: ${gcp_gpu_reaper_schedule})"
    h-gcp-gpu-reval h-gcp-gpu-gcloud functions deploy "${gcp_gpu_reaper_fn}" \
        --gen2 --region="${gcp_gpu_region}" \
        --runtime=python312 --entry-point=reap \
        --source="${gcp_gpu_reaper_src}" \
        --trigger-http --no-allow-unauthenticated \
        --service-account="${gcp_gpu_sa}" \
        --set-env-vars="GCP_GPU_PROJECT=${gcp_gpu_project},GCP_GPU_OWNER=${gcp_gpu_owner},GCP_GPU_MAX_RUN_MIN=${max_run_min},GCP_GPU_GRACE_MIN=${gcp_gpu_reap_grace_min},GCP_GPU_IDLE_MIN=${gcp_gpu_reap_idle_min},GCP_GPU_CPU_PCT=${gcp_gpu_reap_cpu_pct},GCP_GPU_HEARTBEAT_MAX_MIN=${gcp_gpu_reap_heartbeat_max_min},GCP_GPU_ENABLE_KILL=1" @RET

    local uri
    uri="$(h-gcp-gpu-gcloud functions describe "${gcp_gpu_reaper_fn}" \
        --gen2 --region="${gcp_gpu_region}" --format='value(serviceConfig.uri)' 2>/dev/null)"

    ec "scheduling ${gcp_gpu_reaper_schedule}"
    h-gcp-gpu-reval h-gcp-gpu-gcloud scheduler jobs create http "${gcp_gpu_reaper_fn}" \
        --location="${gcp_gpu_region}" \
        --schedule="${gcp_gpu_reaper_schedule}" \
        --uri="${uri}" --http-method=GET \
        --oidc-service-account-email="${gcp_gpu_sa}" \
        --oidc-token-audience="${uri}" @RET

    ec ""
    ec "deployed. It reports to Cloud Logging on every run:"
    ec "  gcp-gpu-reaper-logs"
    ec "Dry it out first with:  GCP_GPU_ENABLE_KILL=0 (redeploy to change)"
}

function gcp-gpu-reaper-logs {
    h-gcp-gpu-deps @RET

    h-gcp-gpu-gcloud logging read \
        "resource.type=\"cloud_run_revision\" AND resource.labels.service_name=\"${gcp_gpu_reaper_fn}\"" \
        --freshness="${1:-7d}" --limit=50 --format='value(timestamp,textPayload)'
}

function gcp-gpu-reaper-destroy {
    h-gcp-gpu-deps @RET

    ask "Delete the scheduler job and reaper function?" n || return 1

    h-gcp-gpu-reval h-gcp-gpu-gcloud scheduler jobs delete "${gcp_gpu_reaper_fn}" \
        --location="${gcp_gpu_region}" --quiet
    h-gcp-gpu-reval h-gcp-gpu-gcloud functions delete "${gcp_gpu_reaper_fn}" \
        --gen2 --region="${gcp_gpu_region}" --quiet

    ec "removed. The custom role and its binding are IAM, so they are yours to revoke:"
    ec "  gcloud projects remove-iam-policy-binding ${gcp_gpu_project} \\"
    ec "    --member=serviceAccount:${gcp_gpu_sa} \\"
    ec "    --role=projects/${gcp_gpu_project}/roles/${gcp_gpu_reaper_role}"
}
##
function gcp-gpu-panic {
    #: No confirmation on purpose: this exists for the moment I see a number I
    #: do not like. It only ever touches instances labeled as mine.
    h-gcp-gpu-deps @RET

    local names
    names="$(h-gcp-gpu-gcloud compute instances list \
        --filter="$(h-gcp-gpu-label-filter) AND status=RUNNING" \
        --format='value(name,zone)' 2>/dev/null)"

    if test -z "$names" ; then
        ec "nothing running under owner=${gcp_gpu_owner}. Already quiet."
        return 0
    fi

    local name zone
    while IFS=$'\t' read -r name zone ; do
        test -z "$name" && continue
        ec "stopping ${name} (${zone})"
        h-gcp-gpu-gcloud compute instances stop "$name" --zone="$zone" --quiet &
    done <<< "$names"

    wait
    ec "panic stop issued for every instance labeled owner=${gcp_gpu_owner}."
    ec "Disks keep billing. gcp-gpu-disks / gcp-gpu-destroy if you want them gone."
}

function gcp-gpu-audit {
    #: Read-only, in both sections. Section (b) exists so that I can see the
    #: shared lab context without ever acting on it.
    h-gcp-gpu-deps @RET

    ec "== (a) mine -- owner=${gcp_gpu_owner}, currently costing money =="
    gcp-gpu-burn
    ec ""
    gcp-gpu-disks

    ec ""
    ec "== (b) not mine -- READ ONLY, do not touch =="

    local others
    others="$(h-gcp-gpu-gcloud compute instances list --format=json 2>/dev/null \
        | command jq -r --arg me "${gcp_gpu_owner}" '
            .[]? | select((.labels.owner // "") != $me)
            | [ .name, (.zone | split("/") | last), .status,
                (.machineType | split("/") | last),
                ((.labels.owner // "unlabeled")) ] | @tsv')"

    if test -z "$others" ; then
        ec "  no other instances in ${gcp_gpu_project}."
    else
        printf '  %-30s %-18s %-12s %-18s %s\n' NAME ZONE STATE MACHINE OWNER-LABEL
        ec "$others" | command awk -F'\t' '{ printf "  %-30s %-18s %-12s %-18s %s\n", $1, $2, $3, $4, $5 }'
    fi

    ec ""
    local orphans
    orphans="$(h-gcp-gpu-gcloud compute disks list --format=json 2>/dev/null \
        | command jq -r --arg me "${gcp_gpu_owner}" '
            .[]? | select((.labels.owner // "") != $me)
            | [ .name, (.zone | split("/") | last), .sizeGb,
                (.type | split("/") | last),
                (if (.users | length) > 0 then "attached" else "UNATTACHED" end),
                ((.labels.owner // "unlabeled")) ] | @tsv')"

    if test -z "$orphans" ; then
        ec "  no other disks in ${gcp_gpu_project}."
    else
        printf '  %-30s %-18s %6s %-20s %-12s %s\n' NAME ZONE GB TYPE ATTACHED OWNER-LABEL
        local -F total=0
        local name zone gb type att owner cost
        while IFS=$'\t' read -r name zone gb type att owner ; do
            test -z "$name" && continue
            cost="$(h-gcp-gpu-disk-price "$type" "$gb")"
            total=$(( total + cost ))
            printf '  %-30s %-18s %6s %-20s %-12s %s\n' "$name" "$zone" "$gb" "$type" "$att" "$owner"
        done <<< "$orphans"
        printf '  ~EUR %.2f/month of disk in this project is not mine. Informational only.\n' "$total"
    fi
}
##
function gcp-gpu-babysit {
    #: Polls a preempted instance back up, with a bounded retry count so that it
    #: cannot loop forever against the budget.
    integer max="${1:-10}"
    h-gcp-gpu-deps @RET

    integer n=0
    while (( n < max )) ; do
        local status
        status="$(h-gcp-gpu-instance-json | command jq -r '.status // "ABSENT"')"

        case "$status" in
            RUNNING)
                sleep 60 ;;
            TERMINATED)
                if ! h-gcp-gpu-budget-ok-p ; then
                    ecerr "$0: over the monthly cap; not restarting. Stopping babysit."
                    return 1
                fi
                n=$(( n + 1 ))
                ec "[${n}/${max}] ${gcp_gpu_instance} is TERMINATED ($(h-gcp-gpu-last-stop-reason)); restarting."
                h-gcp-gpu-retry h-gcp-gpu-reval h-gcp-gpu-gcloud compute instances start \
                    "${gcp_gpu_instance}" --zone="${gcp_gpu_zone}" || return $?
                sleep 60 ;;
            ABSENT)
                ecerr "$0: ${gcp_gpu_instance} does not exist. Stopping babysit."
                return 1 ;;
            *)
                sleep 30 ;;
        esac
    done

    ec "$0: hit the ${max}-restart ceiling. Stopping so this cannot run away."
}
##
#: `ggu` (`git pull --rebase`) and `gga` (`git gui citool --amend`) already
#: exist in git/git.zsh, which loads before this file. Defining them here would
#: silently clobber them, so `gcp-gpu-up` and `gcp-gpu-attach` get no alias.
alias ggs='gcp-gpu-status'
alias ggd='gcp-gpu-down'
##
