# Loaded by .shared.sh through zshlang/basic, and directly by the Git hook.
function h-night-path-scan {
    local root="${1}"
    local directory file
    # addToPATH reverses these arguments, just as for the old **/ glob.
    for directory in "${root}"/**/(N) ; do
        [[ "${directory}" == "${root}"/*.app/* ]] && continue
        for file in "${directory}"*(ND-.) ; do
            if [[ -x "${file}" ]] ; then
                kept+=("${directory}")
                continue 2
            fi
        done
        dropped+=("${directory}")
    done
}

function h-night-path-read {
    local root="${1}" cache="${2}"
    local -a lines
    local data line
    # NUL-delimited data, never sourced. The footer detects partial writes.
    [[ -f "${cache}" && -r "${cache}" ]] || return 1
    data="$(< "${cache}")" || return 1
    lines=( "${(@0)data}" )
    [[ -z "${lines[-1]}" ]] || return 1
    lines[-1]=()
    [[ "${lines[1]}" == "night-path-v1:${root}" && "${lines[-1]}" == "end:${#lines}" ]] || return 1
    kept=() dropped=()
    for line in "${lines[@]:1:${#lines}-2}" ; do
        case "${line}" in
            "+${root}/"|"+${root}/"*/) kept+=("${line:1}") ;;
            "-${root}/"|"-${root}/"*/) dropped+=("${line:1}") ;;
            *) return 1 ;;
        esac
    done
}

function h-night-path-fresh {
    local cache="${1}"
    local git_dir="${XDG_CONFIG_HOME:-${HOME}/.config}/vcsh/repo.d/night.sh.git"
    [[ ! "${git_dir}/index" -nt "${cache}" &&
       ! "${git_dir}/HEAD" -nt "${cache}" &&
       ! "${git_dir}/logs/HEAD" -nt "${cache}" ]] || return 1
    zmodload zsh/stat zsh/datetime || return 1
    local -a mtime
    zstat -A mtime +mtime -- "${cache}" 2>/dev/null || return 1
    (( EPOCHSECONDS - mtime[1] < 86400 ))
}

function h-night-path-cache-file {
    REPLY="${XDG_CACHE_HOME:-${HOME}/.cache}/night-path/${HOST}.v1"
}

function h-night-path-store {
    local root="${1}" cache="${2}"
    local -a kept dropped
    h-night-path-scan "${root}"
    local temporary
    temporary="$(command mktemp "${cache}.XXXXXXXX")" || return 1
    if {
        print -rN -- "night-path-v1:${root}" "${kept[@]/#/+}" "${dropped[@]/#/-}" \
            "end:$(( ${#kept} + ${#dropped} + 2 ))"
    } > "${temporary}" && command mv -f -- "${temporary}" "${cache}" ; then
        return 0
    fi
    command rm -f -- "${temporary}"
    return 1
}

function h-night-path-refresh {
    (
        local root="${1}" cache="${2}" mode="${3}"
        local lock_fd before
        local -a kept dropped
        umask 077
        command mkdir -p -- "${cache:h}" || return 1
        zmodload zsh/system || return 1
        # Keep the lock inode across refreshes. The kernel releases it on exit.
        : >> "${cache}.lock" || return 1
        if [[ "${mode}" == stale ]] ; then
            zsystem flock -t 0 -f lock_fd "${cache}.lock" || return 0
            h-night-path-fresh "${cache}" && return 0
        else
            zsystem flock -f lock_fd "${cache}.lock" || return 1
            if [[ "${mode}" == missing ]] && h-night-path-read "${root}" "${cache}" ; then
                return 0
            fi
        fi

        if [[ "${mode}" == manual ]] ; then
            h-night-path-read "${root}" "${cache}" || kept=()
            before="${#kept}"
        fi
        h-night-path-store "${root}" "${cache}" || return 1
        if [[ "${mode}" == manual ]] ; then
            h-night-path-read "${root}" "${cache}" || return 1
            print -r -- "Cached scripts PATH directories: ${before} -> ${#kept}"
        fi
    )
}

function h-night-path-apply {
    setopt LOCAL_OPTIONS NO_BG_NICE
    local root="${NIGHTDIR:-${HOME}/scripts}"
    root="${root%/}"
    local REPLY
    h-night-path-cache-file
    local cache="${REPLY}"
    local -a kept dropped
    if h-night-path-read "${root}" "${cache}" ; then
        if ! h-night-path-fresh "${cache}" ; then
            h-night-path-refresh "${root}" "${cache}" stale </dev/null >/dev/null 2>&1 &!
        fi
    elif ! h-night-path-refresh "${root}" "${cache}" missing 2>/dev/null ||
         ! h-night-path-read "${root}" "${cache}" ; then
        kept=() dropped=()
        h-night-path-scan "${root}"
    fi

    # Prune the same directories if inherited from an old shell, while
    # preserving PATH entries a caller added outside the recursive glob.
    local -A omitted
    local directory normalized
    local -a inherited
    for directory in "${dropped[@]}" ; do
        normalized="${directory//\/\//\/}"
        omitted[${normalized%/}]=1
    done
    for directory in "${path[@]}" ; do
        normalized="${directory//\/\//\/}"
        if [[ -z "${omitted[${normalized%/}]:-}" ]] ; then
            inherited+=("${directory}")
        fi
    done
    path=( "${inherited[@]}" )
    addToPATH "${kept[@]}"
}

function h-night-path-drop-missing {
    local directory
    local -a existing
    for directory in "${path[@]}" ; do
        [[ -d "${directory}" ]] && existing+=("${directory}")
    done
    path=( "${existing[@]}" )
}

function path-rebuild {
    : 'Rebuild the cached scripts PATH and show the change in directory count.'
    local root="${NIGHTDIR:-${HOME}/scripts}"
    local REPLY
    h-night-path-cache-file
    h-night-path-refresh "${root%/}" "${REPLY}" manual
}

function path-rebuild-install-hooks {
    : 'Install the PATH refresh hooks for the vcsh night.sh repository.'
    local root="${NIGHTDIR:-${HOME}/scripts}"
    local git_dir hook_path hook source_hook="${root}/setup/path-refresh-hook.zsh"
    local -i failed=0
    git_dir="$(command vcsh night.sh rev-parse --absolute-git-dir)" || return 1
    if command vcsh night.sh config --get core.hooksPath >/dev/null ; then
        print -ru2 -- 'night.sh uses core.hooksPath. See docs/path.md for integrating the refresh hook.'
        return 1
    fi
    [[ -x "${source_hook}" ]] || return 1
    command mkdir -p -- "${git_dir}/hooks" || return 1
    for hook in post-merge post-checkout post-commit post-rewrite ; do
        hook_path="${git_dir}/hooks/${hook}"
        if [[ -L "${hook_path}" && "${hook_path:A}" == "${source_hook:A}" ]] ; then
            continue
        fi
        if [[ -e "${hook_path}" || -L "${hook_path}" ]] ||
            ! command ln -s -- "${source_hook:A}" "${hook_path}" ; then
            print -ru2 -- "Add ${source_hook} to the existing ${hook} hook. See docs/path.md."
            failed=1
        fi
    done
    return "${failed}"
}
