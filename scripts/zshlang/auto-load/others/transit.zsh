##
#: Public-transit departure boards, from the shell.
#:
#: [agfi:transit-board] and the `departures-*` variants below are thin wrappers
#: over the TypeScript CLI in `javascript/transit`, run with `bun`. The shell
#: keeps the names, the flags and the paging policy; the package does the
#: fetching, the filtering and the rendering. One helper,
#: [agfi:h-transit-run], is the only place `bun` is invoked, so the variants
#: cannot drift apart in how the tool is called.
#:
#: The *places* -- stop ids, line numbers, direction letters, coordinates -- are
#: deliberately not in this file, and not in the package either. They say where
#: somebody actually lives and works, and this repository is public, so they
#: live in a private address configuration, `$transit_config_file`, which the
#: CLI reads through `ADDRESS_CONFIG`. The package ships no stops and no default
#: profile, and refuses to guess at one rather than pointing at somebody else's
#: neighbourhood. The zsh side is what knows where that file is;
#: [agfi:h-transit-conf-assert] is what explains its absence.
#:
#: The only word the public side knows is `home`, and even that is an
#: indirection: the configuration's `[defaults] home` key names one of its own
#: profiles. See [agfi:departures].
#:
#: See `docs/transit-board.md`, and `javascript/transit/readme.org`.
##
typeset -g transit_dir="${transit_dir:-${NIGHTDIR}/javascript/transit}"
#: Exported as `ADDRESS_CONFIG` by [agfi:h-transit-run] at call time rather than
#: here, so that overriding this global for a single call is actually obeyed.
typeset -g transit_config_file="${transit_config_file:-${HOME}/.address-config/address.toml}"

function h-transit-conf-assert {
    : "h-transit-conf-assert : refuse to run against no address configuration."
    #: This file is public and names no stop, so an unconfigured machine has
    #: nothing to read rather than a board for a place that is not yours. That
    #: is much the better failure of the two, but only if it explains itself,
    #: which is the whole job here.
    local conf_file="${transit_config_file}"
    assert-args conf_file @RET

    test -r "${conf_file}" && return 0

    ecerr "$0: no address configuration found: ${conf_file}"
    ecerr ""
    ecerr "  Stop ids, line numbers, direction letters and coordinates name the"
    ecerr "  places one particular person goes, so they are not in this public"
    ecerr "  repository, and the TypeScript side carries no default profile"
    ecerr "  either. They live in the private address configuration above."
    ecerr ""
    ecerr "  Point \`transit_config_file\` at your own copy, or set"
    ecerr "  \`ADDRESS_CONFIG\` in the environment for a single call."
    ecerr ""
    ecerr "  See \`docs/transit-board.md\`."
    return 1
}

function h-transit-run {
    : "h-transit-run <arg>... : run the transit CLI. The one place bun is invoked."
    #: Every user-facing name below comes through here, exactly as
    #: [agfi:h-claude-code-usage-run] is the single entry to its script. A
    #: second call site is how two commands quietly stop agreeing about colour,
    #: about the config path, or about which package they are running.
    #:
    #: The exit code is passed through unchanged rather than wrapped in
    #: `assert`: the CLI's codes are part of its contract (2 is a config
    #: validation failure, 3 is a subcommand that is not implemented yet), and
    #: `assert` would answer a perfectly informative exit 3 with a stack trace.
    local pkg_dir="${transit_dir}"
    local conf_file="${transit_config_file}"
    local color_p="${transit_color_p:-auto}"
    assert-args pkg_dir conf_file @RET

    ensure-cmd bun @RET

    if ! test -d "${pkg_dir}" ; then
        ecerr "$0: transit package directory not found: ${pkg_dir}"
        ecerr "  Set \`transit_dir\`, or check out this repository's \`javascript/transit\`."
        return 1
    fi

    local cli="${pkg_dir}/src/cli.ts"
    if ! test -e "${cli}" ; then
        ecerr "$0: transit CLI entry point not found: ${cli}"
        ecerr "  The package directory exists but is incomplete."
        return 1
    fi

    #: `--help` is the one thing that must work on a machine that has never been
    #: configured; being told to go and write a config file in order to read the
    #: usage line would be a poor joke.
    local arg help_p=n
    for arg in "$@" ; do
        if [[ "${arg}" == '--help' ]] ; then
            help_p=y
            break
        fi
    done
    bool "${help_p}" || h-transit-conf-assert @RET

    local -a color_opts=()
    case "${color_p}" in
        y) ;;
        n) color_opts=(--no-color) ;;
        auto)
            #: The CLI colours itself when its own stdout is a TTY, and its own
            #: stdout is a pipe whenever we page, so the decision has to be made
            #: out here where the terminal still is. `auto` is what a caller who
            #: is not paging wants; [agfi:transit-board] passes `y` when it is
            #: about to hand the output to a pager that understands colour.
            isOutTty || color_opts=(--no-color)
            ;;
        *)
            ecerr "$0: unknown transit_color_p: ${color_p} (auto, y, n)"
            return 1
            ;;
    esac

    #: Exported here rather than at file-load time, so that a caller who
    #: overrides `transit_config_file` for one call is obeyed.
    local -x ADDRESS_CONFIG="${conf_file}"

    #: `color_opts` first, so an explicit `--no-color` (or its absence) later in
    #: the caller's own arguments still wins under last-wins flag parsing.
    reval command bun "${cli}" "${color_opts[@]}" "$@"
}

function transit-board {
    : "transit-board <subcommand> [<arg>...] : departures, stop lookup and search.
Subcommands: board, discover, search, nearby, route, messages, config-export."
    #: The general entry point. Everything it does not recognise is the CLI's
    #: business and is passed straight through, so `--backend`, `--config` and
    #: the rest keep working without a wrapper knob each.
    #:
    #: Paging is ours to decide, not the tool's. A board that overflows the
    #: screen should scroll; a JSON document should not be handed to a pager at
    #: all, because its consumer is a program; and `--watch` draws its own
    #: screen, so paging it would be nonsense.
    local cache_p="${transit_board_cache_p:-n}"
    local horizon="${transit_board_horizon:-}"
    local backend="${transit_board_backend:-}"

    local -a opts=()
    if bool "${cache_p}" ; then
        opts+=(--cache)
    fi
    if test -n "${horizon}" ; then
        opts+=(--horizon "${horizon}")
    fi
    if test -n "${backend}" ; then
        opts+=(--backend "${backend}")
    fi

    local arg raw_p=n
    for arg in "$@" ; do
        case "${arg}" in
            --json|--watch)
                raw_p=y
                break
                ;;
        esac
    done

    #: `opts` before the caller's arguments, so an explicit flag typed on the
    #: command line overrides the keyword argument rather than the other way
    #: round.
    if bool "${raw_p}" || ! isOutTty ; then
        h-transit-run "${opts[@]}" "$@"
        return $?
    fi

    transit_color_p=y h-transit-run "${opts[@]}" "$@" | pager-if-overflow
    return "${pipestatus[1]}"
}

function departures {
    : "departures [<profile>] [<flag>...] : the departure board for one profile.
Defaults to \`home\`, which the private configuration's \`[defaults] home\` names."
    #: `home` is the one profile word this public file knows, and it is an
    #: alias rather than a profile: the configuration's `[defaults] home` key
    #: says which of its own profiles it means. So this function assumes only
    #: that the alias exists, never which place it points at.
    #:
    #: The variants below name profile keys directly. Those are conventional
    #: names from the private configuration, and nothing here depends on their
    #: contents; a profile added there becomes usable from the shell by adding
    #: one `aliasfn` line and nothing else. They are `aliasfn`s because they
    #: need no documentation of their own, and this one is a real function
    #: because it does: an `aliasfn` variant cannot carry a docstring.
    #:
    #: Only these two are defined here, and deliberately. A profile key is a
    #: word someone chose to name a place they go, so the more of them this
    #: public file lists, the more it says about a private file. Reach the rest
    #: as `departures <key>`, which needs no line here at all.
    local profile="${1:-home}"
    local -a rest=("${@[2,-1]}")

    transit-board "${rest[@]}" board "${profile}"
}
aliasfn departures-home departures home
aliasfn departures-work departures work

function departures-watch {
    : "departures-watch [<profile>] [<flag>...] : the live board, redrawn in place."
    #: The CLI's own `--watch` rather than a loop out here: it knows when its
    #: data actually changed, it can redraw in place instead of reprinting, and
    #: a zsh loop would re-pay process startup on every tick. Never paged; see
    #: [agfi:transit-board].
    local profile="${1:-home}"
    local -a rest=("${@[2,-1]}")

    transit-board --watch "${rest[@]}" board "${profile}"
}
