#!/bin/bash
# ════════════════════════════════════════════════════════════════════════════
# Claude Code — Status Line with real-time usage tracking
#
# Dependencies: bash, jq, curl
# License: MIT
#
# Tracked at ~/scripts/configFiles/claude-code/statusline.sh; ~/.claude/hooks/
# statusline.sh is a symlink to it. Both Claude Code seats run this one file.
#
# Default: 🏫 │ 🌿 main★ │ Snt 4.6 │ 🟢 Ctx ▓▓▓░░░ 42% │ ⏳ 🟡 ▓▓░░░░ 35% ↻ 2h30m │ $0.12 ⏱ 1h4m
# ════════════════════════════════════════════════════════════════════════════

# ── Configuration (override via environment variables) ────────────────────────
TIMEZONE="${TIMEZONE:-}"                            # e.g. "America/New_York", empty = system default
REFRESH_INTERVAL="${REFRESH_INTERVAL:-300}"           # seconds between API calls (0 = every render, risks rate limiting)
SHOW_WEEKLY="${SHOW_WEEKLY:-1}"                      # set to 1 to show weekly + sonnet quotas

# ── Which seat this session runs on ───────────────────────────────────────────
# Claude Code exports CLAUDE_CONFIG_DIR for any non-default profile (the work
# seat uses ~/.claude-work) and nothing for the default one. Every per-profile
# file below is read from the *effective* config dir, so a work session reports
# its own usage, quota and settings rather than the personal seat's. Before
# this, all three defaulted to ~/.claude for both seats.
PROFILE_DIR="${CLAUDE_CONFIG_DIR:-$HOME/.claude}"
PROFILE_DIR="${PROFILE_DIR%/}"

USAGE_FILE="${USAGE_FILE:-$PROFILE_DIR/usage-exact.json}"
CREDENTIALS_FILE="${CREDENTIALS_FILE:-$PROFILE_DIR/.credentials.json}"
SETTINGS_FILE="${SETTINGS_FILE:-$PROFILE_DIR/settings.json}"

# ── Profile badge ─────────────────────────────────────────────────────────────
# One emoji naming the seat, so a work session is recognisable at a glance even
# though both seats share this file and settings.json. The table is not written
# here: profiles.gen.bash sits beside this file, generated from profiles.yaml so
# that the zsh launcher, the Go session pickers and this status line cannot
# disagree about which seat is which. Sourced rather than queried with jq --
# this runs on every render, and an array lookup costs no fork.
#
# An unregistered config dir gets a question mark and its own basename, which is
# also what happens if the generated file is missing.
# CLAUDE_CODE_PROFILE_BADGE_P=n turns it off (claude_statusline_badge_p=n).
declare -A claude_code_profile_markers=()
PROFILES_BASH="${BASH_SOURCE[0]%/*}/profiles.gen.bash"
[ -r "$PROFILES_BASH" ] && . "$PROFILES_BASH"

PROFILE_BADGE=""
case "${CLAUDE_CODE_PROFILE_BADGE_P:-1}" in
    0|n|no|false|off) ;;
    *)
        PROFILE_HOME="${PROFILE_DIR##*/}"
        PROFILE_BADGE="${claude_code_profile_markers[$PROFILE_HOME]:-❓$PROFILE_HOME}"
        ;;
esac

# ── Helpers ───────────────────────────────────────────────────────────────────
tz_date() {
    local tz="$1"; shift
    if [ -n "$tz" ]; then TZ="$tz" date "$@"; else date "$@"; fi
}

format_remaining() {
    local secs="$1"
    [ "$secs" -le 0 ] 2>/dev/null && return
    local h=$(( secs / 3600 )) m=$(( (secs % 3600) / 60 ))
    if [ $h -gt 0 ]; then echo "${h}h${m}m"
    elif [ $m -gt 0 ]; then echo "${m}m"
    else echo "<1m"
    fi
}

# Cross-platform ISO 8601 → epoch (GNU date -d || BSD date -j)
iso_to_epoch() {
    local iso="$1"
    date -d "$iso" +%s 2>/dev/null && return
    # macOS/BSD fallback: strip the offset/Z and fractional seconds, then parse the
    # core as UTC (-u). The API always sends +00:00, so the stripped wall-clock IS
    # UTC; without -u, date -j would read it as local time and skew the countdown.
    local core="${iso%[+-][0-9][0-9]:*}"  # strip +HH:MM / -HH:MM suffix
    core="${core%Z}"                       # strip trailing Z
    core="${core%%.*}"                     # strip .fractional
    date -juf "%Y-%m-%dT%H:%M:%S" "$core" +%s 2>/dev/null
}

file_mtime() {
    if stat --version &>/dev/null; then
        stat -c %Y "$1" 2>/dev/null || echo 0
    else
        stat -f %m "$1" 2>/dev/null || echo 0
    fi
}

cache_age_sec() {
    [ ! -f "$USAGE_FILE" ] && echo 999999 && return
    local age=$(( $(date +%s) - $(file_mtime "$USAGE_FILE") ))
    [ "$age" -lt 0 ] && age=0
    echo "$age"
}

# Coerce to a bare non-negative integer. Drops the decimal part then strips any
# non-digit. Critical: percentages/resets flow into $(( )), where a value like
# "x[$(cmd)]" would execute cmd via arithmetic array-subscript evaluation.
num() {
    local v="${1%%.*}"
    v="${v//[^0-9]/}"
    echo "$(( 10#${v:-0} ))"   # 10# forces base 10 — a leading zero would be read as octal
}

# make_bar <percent> → sets BAR_COLOR and BAR_STR (6-block bar)
make_bar() {
    local pct; pct="$(num "$1")"
    [ "$pct" -gt 100 ] && pct=100
    local filled=$(( (pct + 16) / 17 )); [ $filled -gt 6 ] && filled=6   # 17 = ceil(100/6): round onto 6 blocks
    local empty=$(( 6 - filled ))
    BAR_STR=""
    local i
    for ((i=0; i<filled; i++)); do BAR_STR+="▓"; done
    for ((i=0; i<empty;  i++)); do BAR_STR+="░"; done
    if   [ "$pct" -lt 50 ]; then BAR_COLOR="🟢"
    elif [ "$pct" -lt 80 ]; then BAR_COLOR="🟡"
    else                         BAR_COLOR="🔴"
    fi
}

# render_quota <emoji> <percent> <reset_epoch> → "emoji color bar pct% [↻ remain]".
# A reset moment already in the past means the window rolled over → usage back to 0%.
# Needs NOW set by the caller.
render_quota() {
    local emoji="$1" reset="$3" remain="" pct
    pct="$(num "$2")"
    if [ -n "$reset" ] && [ "$reset" -gt "$NOW" ] 2>/dev/null; then
        remain=$(format_remaining $(( reset - NOW )))
    elif [ -n "$reset" ] && [ "$reset" -le "$NOW" ] 2>/dev/null; then
        pct=0
    fi
    make_bar "$pct"
    local out="${emoji} ${BAR_COLOR} ${BAR_STR} ${pct}%"
    [ -n "$remain" ] && out="${out} ↻ ${remain}"
    echo "$out"
}

# ── Read JSON input from stdin ────────────────────────────────────────────────
JSON=$(cat)

# ── Parse all stdin fields in a single jq call ───────────────────────────────
# Joined on US (0x1f), not "|": a "|" in a branch path or model name would shift
# every field. US is non-whitespace so read preserves empty fields (absent
# rate_limits). rate_limits.* is native since Claude Code 2.1.x (Pro/Max) —
# preferred over the API call when present.
IFS=$'\x1f' read -r J_MODEL_DISPLAY J_MODEL_RAW J_CTX_PCT J_CTX_SIZE J_COST J_DURATION J_CWD \
    J_RL_5H_PCT J_RL_5H_RESET J_RL_7D_PCT J_RL_7D_RESET J_EFFORT \
    < <(echo "$JSON" | jq -r '[
        (if .model | type == "object" then .model.display_name // "" else "" end),
        (if .model | type == "string" then .model else "" end),
        (.context_window.used_percentage // 0 | tostring | split(".")[0]),
        (.context_window.context_window_size // 0),
        (.cost.total_cost_usd // ""),
        (.cost.total_duration_ms // ""),
        (.workspace.current_dir // ""),
        (.rate_limits.five_hour.used_percentage // ""),
        (.rate_limits.five_hour.resets_at // ""),
        (.rate_limits.seven_day.used_percentage // ""),
        (.rate_limits.seven_day.resets_at // ""),
        (.effort.level // "")
    ] | join("\u001f")' 2>/dev/null)

# ── Model ─────────────────────────────────────────────────────────────────────
MODEL="$J_MODEL_DISPLAY"
MODEL=$(echo "$MODEL" | sed 's/Default (\(.*\))/\1/' | sed 's/Claude //' | sed 's/ (.*//')
[ -z "$MODEL" ] && MODEL="$J_MODEL_RAW"
case "$MODEL" in
  claude-sonnet-4-6*|Sonnet\ 4.6*) MODEL="Snt 4.6" ;;
  claude-sonnet-4-5*|Sonnet\ 4.5*) MODEL="Snt 4.5" ;;
  claude-opus-4-6*|Opus\ 4.6*)     MODEL="Opus 4.6" ;;
  claude-opus-4-5*|Opus\ 4.5*)     MODEL="Opus 4.5" ;;
  claude-haiku-4*|Haiku\ 4*)       MODEL="Haiku 4"  ;;
esac
# strip control bytes — model name comes from untrusted JSON (terminal OSC injection)
MODEL="${MODEL//[$'\x01'-$'\x1f'$'\x7f']/}"

# ── Effort level ──────────────────────────────────────────────────────────────
# stdin wins: it is this session's live level, which /effort changes without
# touching settings.json. The settings file is the fallback, and is now read
# from the session's own config dir rather than always the personal one.
EFFORT_LABEL=""
EFFORT_RAW="$J_EFFORT"
if [ -z "$EFFORT_RAW" ] && [ -f "$SETTINGS_FILE" ]; then
    EFFORT_RAW="$(jq -r '.effortLevel // empty' "$SETTINGS_FILE" 2>/dev/null)"
fi
case "$EFFORT_RAW" in
    low)    EFFORT_LABEL="lo" ;;
    medium) EFFORT_LABEL="md" ;;
    high)   EFFORT_LABEL="hi" ;;
    max)    EFFORT_LABEL="mx" ;;
esac

# ── Context window ────────────────────────────────────────────────────────────
CTX_PERCENT="$(num "${J_CTX_PCT:-0}")"
CTX_LABEL="Ctx"
[ "$J_CTX_SIZE" -ge 900000 ] 2>/dev/null && CTX_LABEL="1M"   # ≥900k → extended 1M context

make_bar "$CTX_PERCENT"
CTX_COLOR="$BAR_COLOR" CTX_BAR="$BAR_STR"

# ── Session cost + duration ───────────────────────────────────────────────────
COST_STR="" DURATION_STR=""
if [[ "$J_COST" =~ ^[0-9]+(\.[0-9]+)?([eE][+-]?[0-9]+)?$ ]] && [ "$J_COST" != "0" ]; then
    COST_STR=$(printf '$%.2f' "$J_COST" 2>/dev/null)
fi
if [ -n "$J_DURATION" ] && [ "$J_DURATION" != "0" ] && [ "$J_DURATION" != "null" ]; then
    DURATION_STR=$(format_remaining $(( $(num "$J_DURATION") / 1000 )))
fi

# ── Git branch ────────────────────────────────────────────────────────────────
CWD="$J_CWD"
BRANCH="" DIRTY=""
if [ -n "$CWD" ] && [ -d "$CWD" ]; then
    BRANCH=$(git -C "$CWD" --no-optional-locks symbolic-ref --short HEAD 2>/dev/null)
    if [ -n "$BRANCH" ] && git -C "$CWD" --no-optional-locks diff --quiet HEAD 2>/dev/null; then
        [ -n "$(git -C "$CWD" --no-optional-locks ls-files --others --exclude-standard 2>/dev/null)" ] && DIRTY="★"
    else
        [ -n "$BRANCH" ] && DIRTY="★"
    fi
fi
[ -z "$BRANCH" ] && BRANCH="(no git)"
[ "${#BRANCH}" -gt 30 ] && BRANCH="${BRANCH:0:27}..."

# ── Refresh usage via Anthropic OAuth API ────────────────────────────────────
refresh_usage_api() {
    [ ! -f "$CREDENTIALS_FILE" ] && return 1
    local token
    token=$(jq -r '.claudeAiOauth.accessToken // empty' "$CREDENTIALS_FILE" 2>/dev/null)
    [ -z "$token" ] && return 1
    local resp
    resp=$(curl -s --max-time 3 \
        "https://api.anthropic.com/api/oauth/usage" \
        -H "Authorization: Bearer $token" \
        -H "anthropic-beta: oauth-2025-04-20" \
        -H "Content-Type: application/json" 2>/dev/null)
    echo "$resp" | jq -e '.five_hour.utilization' >/dev/null 2>&1 || return 1
    local tmp
    tmp=$(mktemp "${USAGE_FILE}.XXXXXX") || return 1
    if echo "$resp" | jq '{
        timestamp: (now | todate),
        source: "api",
        metrics: {
            session: {
                percent_used: .five_hour.utilization,
                percent_remaining: (100 - .five_hour.utilization),
                resets_at: .five_hour.resets_at
            },
            week_all: {
                percent_used: .seven_day.utilization,
                percent_remaining: (100 - .seven_day.utilization),
                resets_at: .seven_day.resets_at
            },
            week_sonnet: (if .seven_day_sonnet then {
                percent_used: .seven_day_sonnet.utilization,
                percent_remaining: (100 - .seven_day_sonnet.utilization),
                resets_at: .seven_day_sonnet.resets_at
            } else null end)
        }
    }' > "$tmp"; then
        mv "$tmp" "$USAGE_FILE"
    else
        rm -f "$tmp"; return 1
    fi
}

# Native stdin rate_limits (Pro/Max, CC ≥2.1.x) cover session + weekly-all. The
# API is only needed for the Sonnet quota (SHOW_WEEKLY) or as a fallback when the
# stdin field is absent — so most renders make no network call at all.
HAVE_STDIN_SESSION=0
[ -n "$J_RL_5H_PCT" ] && [ "$J_RL_5H_PCT" != "null" ] && HAVE_STDIN_SESSION=1
NEED_API=1
[ "$HAVE_STDIN_SESSION" = 1 ] && [ "$SHOW_WEEKLY" != "1" ] && NEED_API=0

[[ "$REFRESH_INTERVAL" =~ ^[0-9]+$ ]] || REFRESH_INTERVAL=300

# Lock outside world-writable /tmp to avoid a symlink/clobber on shared hosts.
LOCK_FILE="${XDG_RUNTIME_DIR:-$HOME/.claude}/statusline-refresh.lock"
if [ "$NEED_API" = 1 ] && [ "$(cache_age_sec)" -gt "$REFRESH_INTERVAL" ]; then
    # 2>/dev/null: if the lock dir is missing, skip the refresh quietly (no stderr noise)
    ( flock -n 9 || exit 0; refresh_usage_api ) 9>"$LOCK_FILE" 2>/dev/null
fi

# ── Resolve usage metrics: native stdin (preferred) → API cache (fallback) ────
BLOCK_DISPLAY="" WEEK_SONNET_DISPLAY=""
NOW=$(date +%s)

SESS_PCT="" SESS_EPOCH="" SESS_FROM_CACHE=0
WEEK_PCT="" WEEK_EPOCH="" SONNET_PCT=""

# Leave the epoch empty when no reset is sent — num("") is "0", which render_quota
# would read as a past reset and wrongly zero the live percentage.
if [ "$HAVE_STDIN_SESSION" = 1 ]; then
    SESS_PCT="$J_RL_5H_PCT"
    [ -n "$J_RL_5H_RESET" ] && [ "$J_RL_5H_RESET" != "null" ] && SESS_EPOCH="$(num "$J_RL_5H_RESET")"
fi
if [ "$SHOW_WEEKLY" = "1" ] && [ -n "$J_RL_7D_PCT" ] && [ "$J_RL_7D_PCT" != "null" ]; then
    WEEK_PCT="$J_RL_7D_PCT"
    [ -n "$J_RL_7D_RESET" ] && [ "$J_RL_7D_RESET" != "null" ] && WEEK_EPOCH="$(num "$J_RL_7D_RESET")"
fi

# Cache only fills metrics stdin didn't provide. resets_at parsed as ISO 8601
# (API format); the Sonnet weekly quota is API-only (absent from stdin).
if [ -f "$USAGE_FILE" ]; then
    IFS=$'\x1f' read -r CACHE_SOURCE U_SESS_PCT U_SESS_RESETS U_WEEK_PCT U_WEEK_RESETS U_SONNET_PCT \
        < <(jq -r '[
            (.source // "legacy"),
            (.metrics.session.percent_used     // ""),
            (.metrics.session.resets_at        // ""),
            (.metrics.week_all.percent_used    // ""),
            (.metrics.week_all.resets_at       // ""),
            (.metrics.week_sonnet.percent_used // "")
        ] | join("\u001f")' "$USAGE_FILE" 2>/dev/null)

    if [ -z "$SESS_PCT" ] && [ -n "$U_SESS_PCT" ] && [ "$U_SESS_PCT" != "null" ]; then
        SESS_PCT="$U_SESS_PCT"; SESS_FROM_CACHE=1
        [ "$CACHE_SOURCE" = "api" ] && [ -n "$U_SESS_RESETS" ] && SESS_EPOCH="$(iso_to_epoch "$U_SESS_RESETS")"
    fi
    if [ "$SHOW_WEEKLY" = "1" ]; then
        if [ -z "$WEEK_PCT" ] && [ -n "$U_WEEK_PCT" ] && [ "$U_WEEK_PCT" != "null" ]; then
            WEEK_PCT="$U_WEEK_PCT"
            [ "$CACHE_SOURCE" = "api" ] && [ -n "$U_WEEK_RESETS" ] && WEEK_EPOCH="$(iso_to_epoch "$U_WEEK_RESETS")"
        fi
        [ -n "$U_SONNET_PCT" ] && [ "$U_SONNET_PCT" != "null" ] && SONNET_PCT="$U_SONNET_PCT"
    fi
fi

# ── Render ────────────────────────────────────────────────────────────────────
[ -n "$SESS_PCT" ] && [ "$SESS_PCT" != "null" ] && \
    BLOCK_DISPLAY="$(render_quota "⏳" "$SESS_PCT" "$SESS_EPOCH")"

if [ "$SHOW_WEEKLY" = "1" ]; then
    WEEK_INT="" WEEK_COLOR="" WEEK_RESET_LABEL="" SONNET_INT="" SONNET_COLOR=""
    if [ -n "$WEEK_PCT" ] && [ "$WEEK_PCT" != "null" ]; then
        WEEK_INT="$(num "$WEEK_PCT")"; make_bar "$WEEK_INT"; WEEK_COLOR="$BAR_COLOR"
        if [ -n "$WEEK_EPOCH" ]; then
            # GNU date -d @epoch || BSD date -r epoch
            WEEK_RESET_LABEL=$(tz_date "${TIMEZONE}" -d "@$WEEK_EPOCH" +"%a %Hh" 2>/dev/null \
                || tz_date "${TIMEZONE}" -r "$WEEK_EPOCH" +"%a %Hh" 2>/dev/null)
            WEEK_RESET_LABEL=$(echo "$WEEK_RESET_LABEL" | tr '[:upper:]' '[:lower:]')
        fi
    fi
    if [ -n "$SONNET_PCT" ] && [ "$SONNET_PCT" != "null" ]; then
        SONNET_INT="$(num "$SONNET_PCT")"; make_bar "$SONNET_INT"; SONNET_COLOR="$BAR_COLOR"
    fi
    if [ -n "$WEEK_INT" ] && [ -n "$SONNET_INT" ]; then
        WEEK_SONNET_DISPLAY="📅 ${WEEK_COLOR} ${WEEK_INT}% / Snt ${SONNET_COLOR} ${SONNET_INT}%"
        [ -n "$WEEK_RESET_LABEL" ] && WEEK_SONNET_DISPLAY+=" ↻ ${WEEK_RESET_LABEL}"
    elif [ -n "$WEEK_INT" ]; then
        WEEK_SONNET_DISPLAY="📅 ${WEEK_COLOR} ${WEEK_INT}%"
        [ -n "$WEEK_RESET_LABEL" ] && WEEK_SONNET_DISPLAY+=" ↻ ${WEEK_RESET_LABEL}"
    elif [ -n "$SONNET_INT" ]; then
        WEEK_SONNET_DISPLAY="Snt ${SONNET_COLOR} ${SONNET_INT}%"
    fi
fi

# ── Stale indicator — ⚠ in place of color dot. Only when session came from the
# cache: stdin rate_limits are always fresh, so cache age is irrelevant there.
IS_STALE=0
if [ "$SESS_FROM_CACHE" = 1 ] && [ -f "$USAGE_FILE" ] && [ "$REFRESH_INTERVAL" -gt 0 ] 2>/dev/null; then
    [ "$(cache_age_sec)" -gt $(( REFRESH_INTERVAL * 3 )) ] && IS_STALE=1   # 3 missed refresh windows
fi
if [ "$IS_STALE" = 1 ] && [ -n "$BLOCK_DISPLAY" ]; then
    # Exactly one color dot is present; the other two replacements are no-ops.
    BLOCK_DISPLAY="${BLOCK_DISPLAY/🟢/⚠}"
    BLOCK_DISPLAY="${BLOCK_DISPLAY/🟡/⚠}"
    BLOCK_DISPLAY="${BLOCK_DISPLAY/🔴/⚠}"
fi

# ── Assemble ──────────────────────────────────────────────────────────────────
PARTS=()
[ -n "$PROFILE_BADGE" ] && PARTS+=("$PROFILE_BADGE")
[ -n "$BRANCH" ] && PARTS+=("🌿 $BRANCH$DIRTY")
if [ -n "$MODEL" ] && [ -n "$EFFORT_LABEL" ]; then
    PARTS+=("$MODEL/$EFFORT_LABEL")
elif [ -n "$MODEL" ]; then
    PARTS+=("$MODEL")
fi
[ -n "$CTX_PERCENT" ]         && PARTS+=("$CTX_COLOR $CTX_LABEL $CTX_BAR ${CTX_PERCENT}%")
[ -n "$BLOCK_DISPLAY" ]       && PARTS+=("$BLOCK_DISPLAY")
[ -n "$WEEK_SONNET_DISPLAY" ] && PARTS+=("$WEEK_SONNET_DISPLAY")
# Cost + duration (only if non-zero)
if [ -n "$COST_STR" ] && [ -n "$DURATION_STR" ]; then
    PARTS+=("$COST_STR ⏱ $DURATION_STR")
elif [ -n "$COST_STR" ]; then
    PARTS+=("$COST_STR")
fi

RESULT=""
for part in "${PARTS[@]}"; do
    [ -z "$RESULT" ] && RESULT="$part" || RESULT="$RESULT │ $part"
done

echo "${RESULT}"
