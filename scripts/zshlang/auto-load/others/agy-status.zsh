##
#: Antigravity's remaining quota, and picking a conversation back up when it
#: resets.
#:
#: Three implementations of the same report, chosen by
#: [agfi:agy_status_method]:
#:
#: - =statusline= ([agfi:h-agy-status-statusline]) reads a cache that agy
#:   itself fills. agy pipes a JSON payload to a configured statusline command
#:   on every render, and that payload carries all four quota buckets; our hook
#:   =zshlang/wrappers/agy_statusline.dash= merges them into
#:   [agfi:agy_status_cache_file] as it goes. The report is then a file read,
#:   with no `agy' start-up and no HTTP at all. [agfi:agy-statusline-install]
#:   is what puts the hook in agy's settings.
#: - =direct= ([agfi:h-agy-status-direct]) asks the backend endpoint behind
#:   the numbers itself, with the OAuth token `agy' keeps in the login
#:   Keychain. One or two HTTP round trips, no CLI start-up. The work is in
#:   =python/agy_status.py=.
#: - =slow= ([agfi:h-agy-status-slow]) runs =agy -p /usage= (alias =/quota=),
#:   which prints one tab separated row per model group. Per the vendor's
#:   changelog it spends no quota and leaves no conversation behind, which is
#:   what makes it safe to call from a report you run all day. It is still a
#:   full CLI start-up plus five sequential backend round trips, so it runs
#:   under a timeout and in a throwaway directory -- Antigravity reads the
#:   cwd's project context, and a report must not attach itself to whatever
#:   repository you happen to be standing in.
#:
#: The =direct= path never falls back to the slow one on its own: a fallback
#: that fires silently is a breakage nobody notices for months. It says what
#: failed and names =agy-status-slow=, which is one word to type.
#:
#: =statusline= does fall back, and the difference is the difference between
#: the two failures. A dead direct path is a breakage being hidden; an empty
#: statusline cache only means the hook has not run, which is a fact about
#: whether agy has been open. It is not silent either way -- the reason is
#: always printed. See =docs/agy_status.md=.
#:
#: Everything after "when does it reset" -- arming the job, waiting the clock
#: out, deciding whether resuming is safe, delivering the resume text -- is
#: agent-neutral and lives in =agent-usage.zsh=. See =docs/agy_status.md= and
#: =docs/agent-usage-armed.md=.
##
#: Which implementation [agfi:agy-status] runs. An enum rather than a
#: =_p= switch, because there is nothing boolean about "which of these three".
typeset -g agy_status_method="${agy_status_method:-statusline}"
#: The cache the statusline hook writes and the =statusline= path reads. Two
#: writers share it, so both merge per bucket; see
#: [agfi:h-agy-status-cache-merge].
#:
#: =zshlang/wrappers/agy_statusline.dash= reads this same name out of its
#: *environment*, so overriding it for a test has to be an assignment the hook
#: can see, not merely a shell-local one.
typeset -g agy_status_cache_file="${agy_status_cache_file:-${HOME}/tmp/.agy-status/quota-cache.json}"
#: How old a cached bucket may be and still be reported. A NEGATIVE value means
#: "never drop on age": serve the cache however old it is. An absent cache is a
#: different question -- there is nothing to serve at any age -- so it falls
#: back to the slow path whatever this says.
typeset -g agy_status_statusline_max_age_s="${agy_status_statusline_max_age_s:-3600}"
#: Where agy keeps the settings [agfi:agy-statusline-install] writes into. A
#: knob mostly so the installer can be exercised against a copy.
typeset -g agy_statusline_settings_file="${agy_statusline_settings_file:-${HOME}/.gemini/antigravity-cli/settings.json}"
#: Whether to keep agy's own statusline and stack ours under it, rather than
#: replacing it. agy's own key, =stack_with_default=.
typeset -g agy_statusline_stack_with_default_p="${agy_statusline_stack_with_default_p:-y}"
#: How long =agy -p= gets before we give up on it. The slow path's alone: it
#: is sized for a cold CLI start-up, which is far too generous for one HTTP
#: request.
typeset -g agy_status_timeout_s="${agy_status_timeout_s:-60}"
#: The direct path's per-request timeout.
typeset -g agy_status_direct_timeout_s="${agy_status_direct_timeout_s:-20}"
#: Whether the direct path delegates to the brish garden; see
#: [agfi:h-agy-status-garden-p] for what =auto= decides and why.
typeset -g agy_status_garden_p="${agy_status_garden_p:-auto}"
#: Emit the report as a JSON array instead of prose, for a caller that parses
#: it ([agfi:h-agy-status-arm-deadline] is one).
typeset -g agy_status_json_p="${agy_status_json_p:-n}"
#: Also show the pay-as-you-go credit balance. There is no endpoint of ours
#: for it, so it costs an `agy' start-up on either path -- hence off by
#: default.
typeset -g agy_status_credits_p="${agy_status_credits_p:-n}"
#: Utilization at or above which a model group counts as blocking us. The only
#: knob of the armed job that is Antigravity's own.
typeset -g agy_status_arm_full_pct="${agy_status_arm_full_pct:-100}"
#: Colour of the prose report: =auto= is "colour iff our stdout is a terminal",
#: resolved by [agfi:h-color-mode-p]. An enum rather than a switch,
#: because a caller that has already decided has to be able to say so:
#: [agfi:agent-status] runs us inside the garden, where stdout is a pipe no
#: matter what the user is looking at. The JSON never gets colour.
typeset -g agy_status_color="${agy_status_color:-auto}"
##
function h-agy-status-run {
    #: Runs one of Antigravity's print-mode slash commands and prints its
    #: output verbatim. The slow path's engine, and the only way to the
    #: credits, which have no endpoint of their own here.
    #: Usage: h-agy-status-run </usage|/credits>
    ##
    local timeout_s="${agy_status_timeout_s:-60}"

    local slash_cmd="${1}"
    assert-args slash_cmd @RET

    ensure-cmd agy @RET

    #: Delegated for the same reason the direct path is: refreshing an expired
    #: token is a keychain write, so a detached session would sit on
    #: Antigravity's login prompt instead of reporting. Delegating the whole
    #: command keeps the credential inside the GUI-attached process.
    if h-agy-status-garden-p && brishz-alive-p ; then
        brishz_out_file_p=y brishzq.zsh agy-status-run-garden "${slash_cmd}"
        return $?
    fi

    #: GNU coreutils first, since that is what the rest of zshlang assumes;
    #: plain =timeout= where the system ships it unprefixed. Running without
    #: one is still better than refusing to report at all.
    local -a runner=()
    if isdefined-cmd gtimeout ; then
        runner=(command gtimeout "${timeout_s}")
    elif isdefined-cmd timeout ; then
        runner=(command timeout "${timeout_s}")
    fi

    local tmp_dir
    tmp_dir="$(gmktemp -d)" @TRET

    #: `builtin cd' in the subshell only: the caller's directory is never
    #: touched, and no wrapper of `cd' gets a say. There are no chpwd hooks in
    #: zshlang, so nothing prints into the output we are capturing.
    local out='' ret=0
    out="$(builtin cd -- "${tmp_dir}" && "${runner[@]}" command agy -p "${slash_cmd}")" || ret=$?

    silent command rm -rf -- "${tmp_dir}" || true

    if (( ret != 0 )) ; then
        if (( ret == 124 )) ; then
            ecerr "$0: \`agy -p ${slash_cmd}' timed out after ${timeout_s}s"
        else
            ecerr "$0: \`agy -p ${slash_cmd}' failed (${ret})"
        fi

        return "${ret}"
    fi

    ec "${out}"
}

function h-agy-status-bucket-id {
    #: The cache bucket id for one of [agfi:h-agy-status-rows]' rows.
    #: Usage: h-agy-status-bucket-id <group> <label>
    #:
    #: agy's own ids -- =gemini-5h=, =gemini-weekly=, =3p-5h=, =3p-weekly= --
    #: are what the statusline payload carries, so the slow path's rows are
    #: mapped onto them rather than the other way round: the hook is the writer
    #: that knows all four, and its naming wins.
    ##
    local group="${1}" label="${2}"
    assert-args group label @RET

    #: Case-insensitively throughout: these are upstream's display strings, and
    #: "Claude and GPT models" has already changed case once.
    local group_l="${group:l}" label_l="${label:l}"

    local family='' window=''
    if [[ "${group_l}" == *gemini* ]] ; then
        family=gemini
    elif [[ "${group_l}" == (*claude*|*gpt*|*openai*|*anthropic*) ]] ; then
        family=3p
    fi

    if [[ "${label_l}" == *weekly* ]] ; then
        window=weekly
    elif [[ "${label_l}" == (*5h*|*hour*) ]] ; then
        window=5h
    fi

    if test -n "${family}" && test -n "${window}" ; then
        ec "${family}-${window}"
        return 0
    fi

    #: A row we do not recognize is neither dropped nor guessed at: it gets a
    #: stable id derived from its own text, so a group upstream invents
    #: survives the merge and shows up in the report instead of silently
    #: vanishing between two writers.
    local derived="${group_l}-${label_l}"
    derived="${derived//[^a-z0-9]/-}"
    #: `extendedglob' is set repo-wide, so `(#c2,)' is "two or more".
    derived="${derived//-(#c2,)/-}"
    ec "${${derived#-}%-}"
}

function h-agy-status-bucket-names {
    #: The friendly =<group>\t<label>= for a cache bucket id: the inverse of
    #: [agfi:h-agy-status-bucket-id], so that the cache-backed report says the
    #: same words the other two implementations do.
    #: Usage: h-agy-status-bucket-names <bucket-id>
    ##
    local bucket_id="${1}"
    assert-args bucket_id @RET

    local group label
    case "${bucket_id}" in
        gemini-*) group='Gemini Models' ;;
        3p-*) group='Claude and GPT models' ;;
        #: An id we did not coin; showing it verbatim is more honest than
        #: inventing a display name for it.
        *) group="${bucket_id}" ;;
    esac

    case "${bucket_id}" in
        *-5h) label='5h Limit Remaining' ;;
        *-weekly) label='Weekly Limit Remaining' ;;
        *) label='Limit Remaining' ;;
    esac

    printf '%s\t%s\n' "${group}" "${label}"
}

function h-agy-status-rows {
    #: One normalized row per model group:
    #:   group \t label \t remaining_percent \t reset_epoch \t reset_iso \t bucket_id
    #:
    #: The upstream percent is what is LEFT, not what is spent -- the opposite
    #: of Claude Code's =utilization_percent= -- and the name here says so, so
    #: that nobody reads 98% as "nearly exhausted".
    ##
    ensure-cmd gdate @RET

    local raw
    raw="$(h-agy-status-run /usage)" @TRET

    local line pct iso epoch
    local -a f
    for line in "${(@f)raw}" ; do
        test -n "${line}" || continue

        #: `(ps:\t:)' rather than `read': tab is IFS whitespace, so `read'
        #: would collapse an empty field rather than keep the columns lined up.
        f=( "${(@ps:\t:)line}" )
        if (( ${#f} < 4 )) ; then
            #: A banner, a warning, an upsell line: anything that is not a
            #: quota row. Skipped rather than fatal, so one new line of chrome
            #: upstream does not take the report down with it.
            ecgray "$0: skipping unrecognized row: ${line}"
            continue
        fi

        pct="${f[3]%\%}"
        if [[ "${pct}" != <->(|.<->) ]] ; then
            ecgray "$0: skipping row with a non-numeric percent: ${line}"
            continue
        fi

        iso="${f[4]}"
        #: `gdate' because BSD date cannot read an ISO-8601 string without
        #: being told its format first.
        epoch="$(gdate -d "${iso}" +%s)" @TRET

        #: The bucket id is carried here rather than recomputed by each
        #: consumer, because both the cache merge and the JSON want it and they
        #: must not be able to disagree.
        local bucket_id
        bucket_id="$(h-agy-status-bucket-id "${f[1]}" "${f[2]}")" @TRET

        printf '%s\t%s\t%s\t%s\t%s\t%s\n' \
            "${f[1]}" "${f[2]}" "${pct}" "${epoch}" "${iso}" "${bucket_id}"
    done
}

function h-agy-status-json {
    #: Turns [agfi:h-agy-status-rows] on stdin into the JSON array
    #: =agy_status_json_p= promises.
    ##
    ensure-cmd jq @RET

    local line obj
    local -a f objects=()
    while IFS= read -r line ; do
        test -n "${line}" || continue
        f=( "${(@ps:\t:)line}" )

        #: `--arg'/`--argjson' throughout: a group name is upstream's text and
        #: must not be able to reach the jq program as syntax.
        obj="$(jq -n \
            --arg group "${f[1]}" \
            --arg label "${f[2]}" \
            --argjson remaining "${f[3]}" \
            --argjson resets "${f[4]}" \
            --arg iso "${f[5]}" \
            --arg bucket_id "${f[6]:-}" \
            '{group: $group, label: $label, remaining_percent: $remaining, resets_at: $resets, resets_at_iso: $iso}
             + (if $bucket_id == "" then {} else {bucket_id: $bucket_id} end)')" @TRET
        objects+=("${obj}")
    done

    #: `-s' rather than building the array in zsh, so the result is jq's own
    #: idea of well-formed rather than ours.
    ec "${(F)objects}" | jq -s .
}

function h-agy-status-credits {
    #: The pay-as-you-go balance, as `Label: value' lines. Its rows are
    #: two-column (`Remaining credits', `Upgrade'), so they do not go through
    #: [agfi:h-agy-status-rows].
    ##
    local raw
    raw="$(h-agy-status-run /credits)" @TRET

    local line
    local -a f
    for line in "${(@f)raw}" ; do
        test -n "${line}" || continue

        f=( "${(@ps:\t:)line}" )
        if (( ${#f} < 2 )) ; then
            ec "${line}"
            continue
        fi

        ec "${f[1]}: ${f[2]}"
    done
}

function h-agy-status-write-atomic {
    #: Writes $2 into $1 through a temp file in the same directory.
    #: Usage: h-agy-status-write-atomic <file> <content>
    #:
    #: Shared by the cache merge and the settings installer. Both files have a
    #: reader that may show up at any instant -- the statusline hook renders
    #: constantly, and agy re-reads its settings -- and a half-written file
    #: would be read as a broken one rather than as an old one.
    ##
    local dest="${1}" content="${2}"
    assert-args dest content @RET

    ensure-cmd gmktemp gmv @RET

    assert mkdir -p -- "${dest:h}" @RET

    local tmp
    tmp="$(gmktemp --tmpdir="${dest:h}" "${dest:t}.XXXXXX")" @TRET

    #: The temp file is cleaned up on either failure: it sits next to the real
    #: file, where a leftover would be noise in a directory somebody reads.
    ec "${content}" > "${tmp}" || {
        local retcode=$?
        silent command rm -f -- "${tmp}" || true
        return "${retcode}"
    }

    assert gmv -f -- "${tmp}" "${dest}" || {
        local retcode=$?
        silent command rm -f -- "${tmp}" || true
        return "${retcode}"
    }
}

function h-agy-status-cache-merge {
    #: Merges rows into the shared quota cache, one bucket at a time.
    #: Usage: h-agy-status-cache-merge <source-name> <rows>
    #:
    #: Per bucket, never wholesale. The two writers know different things: the
    #: statusline hook sees all four buckets, while =agy -p /usage= prints only
    #: the two weekly ones, so a slow run that replaced the whole file would
    #: throw away five-hour windows the hook had recorded minutes earlier.
    #: Read-modify-write, and the write is atomic.
    ##
    local cache_file="${agy_status_cache_file:-${HOME}/tmp/.agy-status/quota-cache.json}"

    local source_name="${1}" rows="${2}"
    assert-args source_name rows @RET

    ensure-cmd jq @RET

    zmodload zsh/datetime 2>/dev/null

    local old=''
    if test -r "${cache_file}" ; then
        old="$(command cat -- "${cache_file}")" || old=''
    fi

    #: `fromjson?' rather than reading the file as JSON: a truncated cache must
    #: cost us the old buckets, not the write we are in the middle of.
    local merged
    merged="$(jq -n \
        --arg src "${source_name}" \
        --arg rows "${rows}" \
        --arg old "${old}" \
        --argjson now "${EPOCHSECONDS}" \
        '
        ( ($old | fromjson?) // {} ) as $prevraw
        | ( if ($prevraw | type) == "object" then $prevraw else {} end ) as $prev
        | ( $rows
            | split("\n")
            | map(select(length > 0) | split("\t"))
            | map(select((.[5] // "") != ""))
            | map({ key: .[5],
                    value: { remaining_percent: (.[2] | tonumber),
                             resets_at: (.[3] | tonumber),
                             captured_at: $now,
                             source: $src } })
            | from_entries ) as $fresh
        | { buckets: (($prev.buckets // {}) + $fresh) }')" @TRET

    h-agy-status-write-atomic "${cache_file}" "${merged}" @RET
}

function h-agy-status-render {
    #: The prose report, from [agfi:h-agy-status-rows]-shaped rows in $1.
    #:
    #: Shared by every implementation that renders locally, so that the answer
    #: reads identically whether it came from =agy=, from the backend, or from
    #: the statusline cache.
    #: Usage: h-agy-status-render <rows>
    ##
    local rows="${1}"
    assert-args rows @RET

    zmodload zsh/datetime 2>/dev/null

    #: Decided once, here, rather than per line: `auto' has to test the stdout
    #: of the report as a whole. Note the call is *not* in a command
    #: substitution -- see [agfi:h-color-mode-p] for why that matters.
    local color=never
    h-color-mode-p "${agy_status_color:-auto}" && color=always

    #: Raw SGR strings from zsh's own `colors', not [agfi:colorfg]: that one
    #: re-decides for itself from the environment ([agfi:isColor],
    #: [agfi:true-color-p]), which is the decision the knob above exists to
    #: take away from it.
    local c_group='' c_when='' c_off='' c_pct=''
    local -A pct_colors=()
    if [[ "${color}" == always ]] ; then
        c_group="${fg_bold[white]}"
        c_when="${fg[blue]}"
        c_off="${reset_color}"
        pct_colors=(
            low "${fg_bold[red]}"
            mid "${fg[yellow]}"
            high "${fg[green]}"
        )
    fi

    local line when
    local -a f
    integer remaining_s
    for line in "${(@f)rows}" ; do
        test -n "${line}" || continue
        f=( "${(@ps:\t:)line}" )

        remaining_s=$(( f[4] - EPOCHSECONDS ))
        if (( remaining_s > 0 )) ; then
            when="resets $(date-unix-to-3339 "${f[4]}") (in $(seconds-fmt-short ${remaining_s}))"
        else
            #: `agy' reports a window's end even after it has passed, until
            #: the next call refreshes it.
            when="reset $(seconds-fmt-short $(( -remaining_s ))) ago"
        fi

        #: Presentation only, so deliberately not knobs: nothing branches on
        #: these but the escape codes, and [agfi:agy_status_arm_full_pct] is
        #: the one threshold that actually decides anything.
        if (( f[3] <= 10 )) ; then
            c_pct="${pct_colors[low]}"
        elif (( f[3] <= 33 )) ; then
            c_pct="${pct_colors[mid]}"
        else
            c_pct="${pct_colors[high]}"
        fi

        ec "${c_group}${f[1]}${c_off} (${f[2]}): ${c_pct}${f[3]}% remaining${c_off}, ${c_when}${when}${c_off}"
    done
}

function h-agy-status-slow {
    #: What is left of Antigravity's quota, per model group, and when it comes
    #: back, by asking =agy= itself. Reachable as =agy-status-slow=.
    #: See =docs/agy_status.md=.
    ##
    local json_p="${agy_status_json_p:-n}"
    local credits_p="${agy_status_credits_p:-n}"

    if (( $# > 0 )) ; then
        #: This path is driven entirely by knobs; only the direct one forwards
        #: arguments, to =agy_status.py=. Said out loud rather than dropped, so
        #: a =--host= aimed at the wrong implementation is visible.
        ecgray "$0: ignoring arguments: $*"
    fi

    local rows
    rows="$(h-agy-status-rows)" @TRET
    if test -z "${rows}" ; then
        ecerr "$0: no quota rows in \`agy -p /usage'"
        return 1
    fi

    #: The slow path is the authority on the two weekly buckets, so what it
    #: learns is worth keeping for the fast path to serve. Non-fatal: a cache
    #: we could not update must never cost us a report we already have in hand.
    h-agy-status-cache-merge slow "${rows}" @STRUE

    if bool "${json_p}" ; then
        #: Credits are deliberately not in the JSON: they are a different
        #: currency from the quota windows, and nothing parses them.
        ec "${rows}" | h-agy-status-json
        return $?
    fi

    h-agy-status-render "${rows}" @RET

    if bool "${credits_p}" ; then
        h-agy-status-credits
    fi
}
##
#: The statusline path
##
function h-agy-status-statusline-raw {
    #: One line per cache bucket fresh enough to report:
    #:   bucket_id \t remaining_percent \t resets_at \t captured_at \t source
    #:
    #: Fails (jq's own "no output" status) when nothing survives the age
    #: filter, which is what tells [agfi:h-agy-status-statusline] to fall back.
    ##
    local cache_file="${agy_status_cache_file:-${HOME}/tmp/.agy-status/quota-cache.json}"
    local max_age_s="${agy_status_statusline_max_age_s:-3600}"

    ensure-cmd jq @RET

    if [[ "${max_age_s}" != (-|)<-> ]] ; then
        ecerr "$0: agy_status_statusline_max_age_s is not an integer: ${max_age_s}"
        return 1
    fi

    test -r "${cache_file}" || return 1

    zmodload zsh/datetime 2>/dev/null

    #: A negative max age is "never drop on age", so the whole `captured_at'
    #: test is skipped rather than compared against a negative number -- a
    #: bucket with no `captured_at' at all has to survive that setting too.
    #: Such a bucket comes out with =-1= for its capture time rather than 0,
    #: because "unknown" and "captured at the epoch" are different things and
    #: the second one renders as an age of half a million hours.
    #:
    #: jq's own stderr is dropped: a corrupt cache is answered by the caller's
    #: fallback, with a sentence that says so, not by a parse error from a
    #: program the user did not run.
    jq -er \
        --argjson now "${EPOCHSECONDS}" \
        --argjson max_age "${max_age_s}" \
        '
        ( .buckets // {} )
        | to_entries
        | map(select((.value | type) == "object"))
        | map(select((.value.remaining_percent | type) == "number"))
        | map(select($max_age < 0
                     or (((.value.captured_at // null) | type) == "number"
                         and ($now - .value.captured_at) <= $max_age)))
        | .[]
        | [ .key,
            (.value.remaining_percent | tostring),
            ((.value.resets_at // 0) | tostring),
            ((if (.value.captured_at | type) == "number" then .value.captured_at else -1 end) | tostring),
            (.value.source // "unknown") ]
        | @tsv' "${cache_file}" 2>/dev/null
}

function h-agy-status-statusline {
    #: The same report as [agfi:h-agy-status-slow], read from the cache agy's
    #: own statusline hook keeps up to date. Reachable as
    #: =agy-status-statusline=. See =docs/agy_status.md=.
    ##
    local json_p="${agy_status_json_p:-n}"
    local credits_p="${agy_status_credits_p:-n}"
    local cache_file="${agy_status_cache_file:-${HOME}/tmp/.agy-status/quota-cache.json}"
    local max_age_s="${agy_status_statusline_max_age_s:-3600}"

    #: Before the `deus' branch, so that an argument aimed at the wrong
    #: implementation is visible whichever way we go from here.
    if (( $# > 0 )) ; then
        ecgray "$0: ignoring arguments: $*"
    fi

    if isDeus ; then
        #: The established "force, bypass the memo" convention: asking under
        #: `deus' is asking to distrust what we have lying around, so the cache
        #: is not even opened.
        ecgray "$0: deus: bypassing the cache and asking agy itself"
        h-agy-status-slow
        return $?
    fi

    if ! test -r "${cache_file}" ; then
        #: An absent cache is not a staleness question: there is nothing to
        #: serve however generous [agfi:agy_status_statusline_max_age_s] is, so
        #: this fallback ignores the knob entirely. Unlike the direct path's
        #: refusal to fall back, this one is not a breakage being hidden --
        #: nothing is broken, the hook has simply never run.
        ecgray "$0: no statusline cache at ${cache_file}; falling back to the slow path (see \`agy-statusline-install')"
        h-agy-status-slow
        return $?
    fi

    local raw=''
    raw="$(h-agy-status-statusline-raw)" || raw=''

    if test -z "${raw}" ; then
        #: The validity check is here, in the failure branch, rather than in
        #: front of every successful read: it exists only to pick the right
        #: sentence, and the happy path should not pay a second jq for it.
        if ! silent jq -e . "${cache_file}" ; then
            ecgray "$0: the cache is not valid JSON (${cache_file}); falling back to the slow path"
        else
            ecgray "$0: nothing in the cache is newer than ${max_age_s}s; falling back to the slow path"
        fi

        h-agy-status-slow
        return $?
    fi

    zmodload zsh/datetime 2>/dev/null

    local line names iso
    local -a f rows=() sources=()
    integer age oldest_age=0 unknown_age_seen=0
    for line in "${(@f)raw}" ; do
        test -n "${line}" || continue
        f=( "${(@ps:\t:)line}" )

        names="$(h-agy-status-bucket-names "${f[1]}")" @TRET
        iso="$(date-unix-to-3339 "${f[3]}")" @TRET

        #: The very row contract the other two implementations emit, bucket id
        #: included, so [agfi:h-agy-status-arm-deadline] and
        #: [agfi:agent-status] never learn that a third path exists.
        rows+=( "${names%%$'\t'*}"$'\t'"${names#*$'\t'}"$'\t'"${f[2]}"$'\t'"${f[3]}"$'\t'"${iso}"$'\t'"${f[1]}" )

        if (( f[4] < 0 )) ; then
            #: See [agfi:h-agy-status-statusline-raw]: -1 is "no capture time
            #: in the cache at all", which is a different claim from "captured
            #: a long time ago" and must not be averaged in as one.
            unknown_age_seen=1
        else
            age=$(( EPOCHSECONDS - f[4] ))
            (( age < 0 )) && age=0
            (( age > oldest_age )) && oldest_age=${age}
        fi

        sources+=( "${f[5]}" )
    done

    #: The worst age rather than the best, and the writers named: a number that
    #: came out of the cache has to say so, or the report reads as live when it
    #: is not.
    local note
    note="$0: from the cache, up to $(seconds-fmt-short ${oldest_age}) old, written by ${(j:, :)${(@u)sources}}" @TRET
    if (( unknown_age_seen )) ; then
        note+=' (and some rows carry no capture time)'
    fi

    if bool "${json_p}" ; then
        #: The note goes to stderr, because stdout is a JSON document somebody
        #: is about to parse.
        ecgray "${note}"

        ec "${(F)rows}" | h-agy-status-json
        return $?
    fi

    ecgray "${note}"
    h-agy-status-render "${(F)rows}" @RET

    if bool "${credits_p}" ; then
        #: Credits have no endpoint and no cache, so asking for them is still
        #: asking for an `agy' start-up, exactly as on the other two paths.
        h-agy-status-credits
    fi
}
##
#: Installing the hook into agy's settings
##
function h-agy-statusline-command {
    #: The absolute path agy is told to run. One place, so the installer, the
    #: uninstaller and the predicate cannot disagree about what "ours" means.
    ##
    ec "${NIGHTDIR:-${HOME}/scripts}/zshlang/wrappers/agy_statusline.dash"
}

function h-agy-statusline-desired {
    #: The =statusLine= object we want in agy's settings, as JSON.
    #:
    #: The key names are agy's own: =type=, =command=, =enabled= and
    #: =stack_with_default=. Confirmed against Antigravity's CLI documentation
    #: and against the changelog agy itself prints (`agy changelog'), which is
    #: where =stack_with_default= was announced. =padding= is a fifth key and is
    #: deliberately left unset, so agy's own default stands.
    ##
    local stack_p="${agy_statusline_stack_with_default_p:-y}"

    ensure-cmd jq @RET

    local stack=false
    bool "${stack_p}" && stack=true

    local cmd
    cmd="$(h-agy-statusline-command)" @TRET

    jq -n --arg command "${cmd}" --argjson stack "${stack}" \
        '{type: "command", command: $command, enabled: true, stack_with_default: $stack}'
}

function h-agy-statusline-current-command {
    #: The command agy is currently configured to run for its statusline, or
    #: nothing at all. The FIRST word only: an installed command may carry
    #: arguments (upstream's own renderer documents a =--classic= flag that
    #: way), and it is still the same command.
    ##
    local settings_file="${agy_statusline_settings_file:-${HOME}/.gemini/antigravity-cli/settings.json}"

    ensure-cmd jq @RET

    test -r "${settings_file}" || return 0

    #: The trailing `// ""' is not redundant: jq's `"" | split(" ")' is the
    #: EMPTY array, so indexing it yields null, and `jq -r' prints null as the
    #: four characters "null" -- which would then read as a foreign statusline
    #: to every caller here.
    jq -r '((.statusLine.command // "") | split(" ")[0]) // ""' "${settings_file}"
}

function agy-statusline-installed-p {
    #: Whether agy's statusline is pointed at OUR hook. Quiet: the exit status
    #: is the whole answer.
    #: See =docs/agy_status.md=.
    ##
    local current ours
    current="$(h-agy-statusline-current-command)" @TRET
    ours="$(h-agy-statusline-command)" @TRET

    test -n "${current}" && [[ "${current}" == "${ours}" ]]
}

function agy-statusline-install {
    #: Points agy's =statusLine= at our hook, idempotently.
    #: See =docs/agy_status.md=.
    ##
    local settings_file="${agy_statusline_settings_file:-${HOME}/.gemini/antigravity-cli/settings.json}"

    ensure-cmd jq @RET

    local ours desired
    ours="$(h-agy-statusline-command)" @TRET
    desired="$(h-agy-statusline-desired)" @TRET

    if ! test -x "${ours}" ; then
        #: Caught here rather than by agy, which would only disable the
        #: statusline after a few silent failures.
        ecerr "$0: the hook is missing or not executable: ${ours}"
        return 1
    fi

    local existing='{}'
    if test -e "${settings_file}" ; then
        #: Refused rather than overwritten: the file is agy's, and everything
        #: in it but our one key is the user's.
        silent jq -e . "${settings_file}" || {
            ecerr "$0: not valid JSON, refusing to touch it: ${settings_file}"
            return 1
        }

        existing="$(command cat -- "${settings_file}")" @TRET
    fi

    local current
    current="$(h-agy-statusline-current-command)" @TRET

    if test -n "${current}" && [[ "${current}" != "${ours}" ]] ; then
        if ! isDeus ; then
            ecerr "$0: a statusLine is already configured, and it is not ours:"
            ecerr "    ${current}"
            ecerr "  Run \`deus ${0}' to replace it, or remove it from agy with \`/statusline delete'."
            return 1
        fi

        ecgray "$0: deus: replacing a foreign statusLine: ${current}"
    fi

    if agy-statusline-installed-p ; then
        local installed
        installed="$(jq -Sc '.statusLine // {}' "${settings_file}")" @TRET

        local wanted
        wanted="$(ec "${desired}" | jq -Sc .)" @TRET

        if [[ "${installed}" == "${wanted}" ]] ; then
            #: Said out loud, so that running this twice is visibly a no-op
            #: rather than an invisible rewrite of a file we back up only once.
            ecgray "$0: already installed: ${ours}"
            return 0
        fi
    fi

    #: Once, and only before the first modification. A backup taken on every
    #: run would eventually be a backup of our own output, which is no backup.
    local backup="${settings_file}.agy-statusline.bak"
    if test -e "${settings_file}" && ! test -e "${backup}" ; then
        assert mkdir -p -- "${settings_file:h}" @RET
        assert cp -- "${settings_file}" "${backup}" @RET
        ecgray "$0: backed the original up to ${backup}"
    fi

    #: Read-modify-write of the whole document: every other key survives, and
    #: jq is what decides what well-formed means, not us.
    local updated
    updated="$(ec "${existing}" | jq --argjson statusline "${desired}" \
        '. + {statusLine: $statusline}')" @TRET

    h-agy-status-write-atomic "${settings_file}" "${updated}" @RET

    ecgray "$0: installed: ${ours}"
    ecgray "$0: agy picks it up on its next start; \`agy-status' then reads ${agy_status_cache_file:-${HOME}/tmp/.agy-status/quota-cache.json}"
}

function agy-statusline-uninstall {
    #: Removes OUR statusLine entry from agy's settings, and only ours.
    #: See =docs/agy_status.md=.
    ##
    local settings_file="${agy_statusline_settings_file:-${HOME}/.gemini/antigravity-cli/settings.json}"

    ensure-cmd jq @RET

    if ! test -e "${settings_file}" ; then
        ecgray "$0: nothing to remove: ${settings_file} does not exist"
        return 0
    fi

    silent jq -e . "${settings_file}" || {
        ecerr "$0: not valid JSON, refusing to touch it: ${settings_file}"
        return 1
    }

    local current ours
    current="$(h-agy-statusline-current-command)" @TRET
    ours="$(h-agy-statusline-command)" @TRET

    if test -z "${current}" ; then
        ecgray "$0: not installed; nothing to remove"
        return 0
    fi

    if [[ "${current}" != "${ours}" ]] ; then
        #: Never `deus'-overridable, unlike the install: removing somebody
        #: else's statusline is not a thing we should be able to be talked
        #: into, and agy's own `/statusline delete' is right there.
        ecgray "$0: leaving a statusLine that is not ours alone: ${current}"
        return 0
    fi

    local updated
    updated="$(jq 'del(.statusLine)' "${settings_file}")" @TRET

    h-agy-status-write-atomic "${settings_file}" "${updated}" @RET

    ecgray "$0: uninstalled"
}
##
#: The direct path
##
function h-agy-status-garden-p {
    #: Whether to run the report inside the brish garden rather than here.
    #: It guards both paths: the direct one reads the login keychain itself,
    #: and `agy' reads it too whenever the access token it holds has expired
    #: and must be refreshed -- which is why the slow path needs this as much
    #: as the direct one. Without it a detached session stops on Antigravity's
    #: interactive login prompt and waits out its own timeout.
    #:
    #: The garden's worker shells are attached to the GUI session, so they can
    #: read the login keychain, which is where the OAuth token lives. A
    #: GUI-detached session cannot: its keychain search list collapses to the
    #: System keychain alone, so the lookup finds nothing and =security=
    #: reports the credential as simply absent. See =docs/agy_status.md=.
    #:
    #: Proactive rather than a retry, because over ssh the local read *cannot*
    #: succeed -- attempting it first would only buy a round trip and a
    #: misleading error. The same shape as
    #: [agfi:h-claude-code-usage-garden-p], for the same reason.
    ##
    local mode="${agy_status_garden_p:-auto}"

    case "${mode}" in
        y) return 0 ;;
        n) return 1 ;;
        auto)
            #: `managername' answers `Aqua' only in a session attached to the
            #: GUI login, and that attachment is exactly what the keychain
            #: search list depends on -- so it names the real condition, where
            #: [agfi:isSSH] only guesses at it. An agent-spawned shell on this
            #: very machine is `Background' and cannot read the keychain
            #: either. Kept as a fallback for a platform without `launchctl'.
            ##
            local manager=''
            if isdefined-cmd launchctl ; then
                manager="$(command launchctl managername 2>/dev/null)" || manager=''
            fi

            if test -n "${manager}" ; then
                [[ "${manager}" != Aqua ]]
            else
                isSSH
            fi
            ;;
        *)
            ecerr "$0: unknown agy_status_garden_p: ${mode} (auto, y, n)"
            return 1
            ;;
    esac
}

function agy-status-run-garden {
    #: What the garden runs on our behalf; see [agfi:h-agy-status-run]. It
    #: forces the guard off, so a garden worker can never bounce the command
    #: back to another worker -- the worker is already where the keychain is
    #: readable.
    ##
    local slash_cmd="${1}"
    assert-args slash_cmd @RET

    agy_status_garden_p=n h-agy-status-run "${slash_cmd}"
}

function h-agy-status-run-direct {
    #: The one place =agy_status.py= is invoked, so the garden delegation
    #: cannot be forgotten by a caller.
    #:
    #: The whole invocation is delegated, not just the keychain read, so the
    #: token never leaves the GUI-attached process: only the rendered report or
    #: the JSON array comes back. See =docs/agy_status.md=.
    ##
    if h-agy-status-garden-p && brishz-alive-p ; then
        local -a color_opts=()
        #: The garden's stdout is a pipe, so =--color auto= would resolve to no
        #: colour for the command run most often. Placed first, so the
        #: =--color= our caller resolved still wins (argparse is last-wins).
        [[ -t 1 ]] && color_opts=(--color always)

        #: Non-ASCII survives the trip this way; the inline transport mangles
        #: it.
        brishz_out_file_p=y brishzq.zsh agy_status.py "${color_opts[@]}" "$@"
        return $?
    fi

    $proxyenv revaldbg command agy_status.py "$@"
}

function h-agy-status-direct {
    #: The same report as [agfi:h-agy-status-slow], read from the backend
    #: rather than from =agy=. Reachable as =agy-status-direct=.
    #: See =docs/agy_status.md=.
    ##
    local json_p="${agy_status_json_p:-n}"
    local credits_p="${agy_status_credits_p:-n}"
    local timeout_s="${agy_status_direct_timeout_s:-20}"

    ensure-cmd agy_status.py @RET

    #: Decided here rather than left to the script's own =auto=: the knob is
    #: about *our* stdout, and the script may well be running in the garden,
    #: where stdout is a pipe whatever the user is looking at. Note the call is
    #: not in a command substitution -- see [agfi:h-color-mode-p] for why that
    #: matters.
    local color=never
    h-color-mode-p "${agy_status_color:-auto}" && color=always

    local -a script_args=(--timeout "${timeout_s}" --color "${color}")
    if bool "${json_p}" ; then
        script_args+=(--json)
    fi

    local retcode=0
    #: Our own arguments come last, so anything the caller passes -- =--host=,
    #: =--no-relogin= -- overrides what we decided.
    h-agy-status-run-direct "${script_args[@]}" "$@" || retcode=$?

    if (( retcode == 0 )) && bool "${credits_p}" && ! bool "${json_p}" ; then
        #: Credits are the one number with no endpoint of ours, so this
        #: addendum is still an =agy= start-up even on the fast path. It is off
        #: by default, and asking for it is asking for that cost.
        #:
        #: Deliberately not in the JSON, exactly as on the slow path: they are
        #: a different currency from the quota windows, and nothing parses them.
        h-agy-status-credits
    fi

    return "${retcode}"
}
##
function agy-status {
    #: What is left of Antigravity's quota, per bucket, and when it comes back.
    #: The gateway: [agfi:agy_status_method] picks the implementation.
    #: See =docs/agy_status.md=.
    ##
    local method="${agy_status_method:-statusline}"

    case "${method}" in
        statusline)
            h-agy-status-statusline "$@"
            ;;
        direct)
            h-agy-status-direct "$@"
            ;;
        slow)
            h-agy-status-slow "$@"
            ;;
        *)
            ecerr "$0: unknown agy_status_method: ${method} (statusline, direct, slow)"
            return 1
            ;;
    esac
}
aliasfn agys agy-status

#: Named implementations, so a caller can demand one without knowing the knob.
#: The slow one is what a failing direct report names, and what to reach for
#: when you need the number =agy= itself would print.
aliasfnq agy-status-statusline agy_status_method=statusline agy-status
aliasfnq agy-status-direct agy_status_method=direct agy-status
aliasfnq agy-status-slow agy_status_method=slow agy-status
##
#: Waiting the quota out
##
function h-agy-status-arm-deadline {
    #: Prints "<reset-epoch>\t<msg>" for the reset worth waiting on, or
    #: nothing at all when there is nothing to wait for.
    #:
    #: Split out from [agfi:h-agy-status-arm] because it is the only part
    #: with a decision in it, and the only part that can be exercised without
    #: arming a real job.
    ##
    local full_pct="${agy_status_arm_full_pct:-100}"

    ensure-cmd jq @RET

    local json
    json="$(agy_status_json_p=y agy-status)" @TRET

    #: The percent is what REMAINS, so a group is spent when at most
    #: `100 - full_pct' of it is left: the default 100 means "nothing left".
    integer max_remaining=$(( 100 - full_pct ))

    #: The EARLIEST reset among the spent groups, unlike Claude Code's latest:
    #: these are independent quotas rather than nested windows, so the first
    #: one back is genuinely usable. Hence also naming the group in the
    #: message -- which quota returned is the whole of what changed.
    #:
    #: `unique' on the groups, because a group now contributes two rows rather
    #: than one: every path but the slow one reports the five-hour window
    #: beside the weekly one, and both being spent must not spell the group's
    #: name twice in a notification.
    local out=''
    out="$(ec "${json}" | jq -er --argjson max "${max_remaining}" '
        [.[] | select(.remaining_percent <= $max)]
        | select(length > 0)
        | [(map(.resets_at) | min), (map(.group) | unique | join(", "))]
        | @tsv')" || out=''

    if test -n "${out}" ; then
        printf '%s\t%s\n' "${out%%$'\t'*}" \
            "Antigravity: ${out#*$'\t'} quota reset, usage available again"
        return 0
    fi

    if ! isDeus ; then
        ecgray "$0: usage already possible, not arming (use \`deus\` to arm anyway)"
        return 0
    fi

    #: deus: arm for whichever group rolls over first anyway, so the mechanism
    #: can be exercised without having to be rate-limited first.
    out="$(ec "${json}" | jq -er '
        min_by(.resets_at) | [.resets_at, .group] | @tsv')" @TRET

    printf '%s\t%s\n' "${out%%$'\t'*}" \
        "Antigravity: ${out#*$'\t'} quota window rolled over"
}

function h-agy-status-arm {
    #: Arms, or re-arms, a one-shot job for when Antigravity's quota comes
    #: back. $1 is the tmux session it lives in.
    ##
    local session="${1}"
    assert-args session @RET

    local out
    out="$(h-agy-status-arm-deadline)" @TRET
    #: Nothing to wait for; the reason was already said on stderr.
    test -n "${out}" || return 0

    #: Only Antigravity sessions are offered, unlike Claude Code's kitty
    #: picker: there is no cross-agent story here -- an Antigravity quota
    #: reset unblocks Antigravity conversations and nothing else. `local' is
    #: dynamically scoped in zsh, so the picker sees it without anything being
    #: exported.
    local agent_session_agents=agy

    h-agent-usage-arm "${session}" "${out%%$'\t'*}" "${out#*$'\t'}"
}

function h-agy-status-arm-schedule {
    #: Arms without printing a report, matching Claude Code's
    #: =h-...-arm-schedule= escape hatch. The tmux session it lives in is
    #: named once, here and in [agfi:agy-status-armed-sessions].
    ##
    h-agy-status-arm 'agy-status-armed'
}

function agy-status-notify {
    #: The quota report, then an armed job for when it comes back -- the
    #: intended way in, mirroring [agfi:claude-code-usage-fable-notify].
    ##
    local retcode=0
    agy-status "$@" || retcode=$?

    if (( retcode == 0 )) ; then
        #: =>&2= because our stdout may be a JSON document a caller is about
        #: to parse; non-fatal because a failed schedule must not make a
        #: working report look broken.
        h-agy-status-arm-schedule >&2 || true
    fi

    return "${retcode}"
}
##
#: Resuming rather than merely announcing, named after the delivery mechanism
#: exactly as Claude Code's are; see
#: [agfi:h-agent-usage-continue-targets].
##
aliasfnq agy-status-continue-kitty-fz \
    agent_usage_arm_action=continue agent_usage_continue_via=kitty \
    agy-status-notify

aliasfnq agy-status-continue-tmux-fz \
    agent_usage_arm_action=continue agent_usage_continue_via=tmux \
    agy-status-notify

aliasfnq agy-status-continue-frontmost \
    agent_usage_arm_action=continue agent_usage_continue_via=frontmost \
    agy-status-notify

aliasfn agyk agy-status-continue-kitty-fz
aliasfn agyt agy-status-continue-tmux-fz
aliasfn agyfront agy-status-continue-frontmost
##
function agy-status-armed-sessions {
    #: Every tmux session an Antigravity armed job can live in, one per line.
    #: One today; a function anyway, so the cancel/status wrappers below ask
    #: rather than each spelling the name out.
    #:
    #: [agfi:agent-usage-armed-sessions] calls this when it is defined and
    #: skips it otherwise, so the name lives here only and a renamed session
    #: needs changing in one place.
    ##
    ec 'agy-status-armed'
}

#: Cancelling and reporting are the same act whatever armed the job, so both
#: are [agfi:h-agent-usage-armed-cancel] and [agfi:h-agent-usage-armed-status]
#: over the sessions this family owns. Named arguments still narrow it to one
#: session.
function agy-status-armed-cancel {
    local sessions=("$@")
    if (( ${#sessions} == 0 )) ; then
        sessions=("${(@f)$(agy-status-armed-sessions)}")
    fi

    h-agent-usage-armed-cancel "${sessions[@]}"
}

function agy-status-armed-status {
    local sessions=("$@")
    if (( ${#sessions} == 0 )) ; then
        sessions=("${(@f)$(agy-status-armed-sessions)}")
    fi

    h-agent-usage-armed-status "${sessions[@]}"
}
##
