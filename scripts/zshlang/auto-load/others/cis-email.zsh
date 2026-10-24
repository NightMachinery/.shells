## Generic entrypoints for the separately installed private cis-email skill.
## No identity, mailbox contents, agent IDs, or runtime state belong here.
## See =docs/cis-email.md= and the installed skill's SKILL.md.
##
function h-cis-email-helper {
    emulate -L zsh
    ensure-cmd python3 || return $?
    local skill_dir="${cis_email_skill_dir:-${HOME}/.agents/skills/cis-email}"
    local helper="${skill_dir}/scripts/cis_email_monitor.py"
    if [[ ! -f "${helper}" ]] ; then
        print -ru2 -- "$0: install the private cis-email skill, or set cis_email_skill_dir"
        return 1
    fi
    command python3 "${helper}" "$@"
}

function h-cis-email-monitor-name {
    emulate -L zsh
    local monitor_name="${1:-default}"
    if [[ ! "${monitor_name}" =~ '^[A-Za-z0-9][A-Za-z0-9_-]{0,47}$' ]] ; then
        print -ru2 -- "$0: invalid monitor name"
        return 64
    fi
    REPLY="${monitor_name}"
}

function cis-email-monitor-start {
    : "usage: cis-email-monitor-start [name] [--to PASEO_UUID] [--query QUERY] [--interval SECONDS] [--hours HOURS]"
    emulate -L zsh
    ensure-cmd python3 notmuch paseo tmux || return $?
    local monitor_name=default
    if (( $# )) && [[ "${1}" != --* ]] ; then
        monitor_name="${1}"
        shift
    fi
    h-cis-email-monitor-name "${monitor_name}" || return $?
    local job_name="cis-email-${monitor_name}"
    if tmux-job-running-p "${job_name}" ; then
        print -ru2 -- "$0: ${job_name} is already running; use cis-email-monitor-status"
        return 1
    fi
    local config_file
    config_file="$(h-cis-email-helper prepare --name "${monitor_name}" "$@")" || return $?
    local skill_dir="${cis_email_skill_dir:-${HOME}/.agents/skills/cis-email}"
    #: prepare persists scope and notification target privately before launch;
    #: the detached job does not depend on inherited agent/mail environment.
    tmux-job-start "${job_name}" "CIS_EMAIL_STATE_DIR=${config_file:h:h}" command python3 \
        "${skill_dir}/scripts/cis_email_monitor.py" run --name "${monitor_name}"
}

function cis-email-monitor-stop {
    : "usage: cis-email-monitor-stop [name]"
    emulate -L zsh
    h-cis-email-monitor-name "${1:-default}" || return $?
    tmux-job-stop "cis-email-${REPLY}"
}

function cis-email-monitor-status {
    : "usage: cis-email-monitor-status [name]"
    emulate -L zsh
    h-cis-email-monitor-name "${1:-default}" || return $?
    local monitor_name="${REPLY}"
    tmux-job-status "cis-email-${monitor_name}" || true
    h-cis-email-helper status --name "${monitor_name}"
}

function cis-email-monitor-check {
    : "usage: cis-email-monitor-check [name] [--no-notify]"
    emulate -L zsh
    local monitor_name="${1:-default}"
    (( $# )) && shift
    h-cis-email-monitor-name "${monitor_name}" || return $?
    h-cis-email-helper check --name "${monitor_name}" "$@"
}

function cis-email-sync {
    : "usage: cis-email-sync [wait-seconds]"
    emulate -L zsh
    ensure-cmd python3 || return $?
    local doom_dir="${DOOMDIR:-${HOME}/doom.d}"
    local helper="${doom_dir}/email/bin/night-mail-request"
    if [[ ! -f "${helper}" ]] ; then
        print -ru2 -- "$0: night-mail-request not found; check DOOMDIR"
        return 1
    fi
    command python3 "${helper}" --wait "${1:-60}"
}
