## @todo0 these should be in core.zsh
function isdefined() {
    local sym="$1"

    test -n "$sym" && (( $+commands[$sym] || $+functions[$sym] || $+aliases[$sym] ))
}
alias isDefined=isdefined

function ifdefined() {
    # @duplicateCode/2835fdc8e7eb4fedc98965e17db301b6
    ##
    local cmd_head="$1"

    if isdefined "$cmd_head" ; then
        reval "$@"
    else
        return 1270 # == 246
    fi
}

function isdefined-cmd {
     local sym="$1"

     test -n "$sym" && (( $+commands[$sym] ))
}
alias 'isDefined-cmd'=isdefined-cmd

function ifdefined-cmd() {
    # @duplicateCode/2835fdc8e7eb4fedc98965e17db301b6
    ##
    local cmd_head="$1"

    if isdefined-cmd "$cmd_head" ; then
        reval "$@"
    else
        return 1270 # == 246
    fi
}

function ensure-cmd {
    local name
    for name in "$@" ; do
        if ! isdefined-cmd "${name}" ; then
            ecerr "missing command: ${name}"
            return 1
        fi
    done
}

function cmd-sub() {
    local cmd="$1" sub="$2"

    if isdefined-cmd "$cmd" ; then
        print -nr -- "$cmd"
    else
        print -nr -- "$sub"
    fi
}

function ifdefined-cmd-or-cat {
    local cmd="$1" ; shift
    assert-args cmd @RET

    if isdefined-cmd "$cmd" ; then
        "$cmd" "$@"
    else
        ecerr "$(fn-name 3): $(gquote-sq $cmd) not found, falling back to 'cat'"

        cat
    fi

    ## @tests
    # `aliasfn t-cat ifdefined-cmd-or-cat nonexistent-918982`
    # `aliasfn t-char-count ifdefined-cmd-or-cat wc -c`
    ##
}
##
function ensure-dep1 {
    local dep="$1" install_cmd=("${@[2,-1]}")
    assert-args dep install_cmd || return $?

    if ! isdefined-cmd "${dep}" ; then
        assert reval "$install_cmd[@]"

        rehash
        if ! isdefined-cmd "${dep}" ; then
            ecerr "$0: could not install $(gquote-sq "$dep")"
            return 1
        fi
    fi
}

function go-local-dep {
    #: Ensures `name', a Go tool built from the local module at `dir', is on
    #: PATH and no older than its source: builds it when it is missing, and
    #: rebuilds it when any `*.go' (tests aside), `go.mod' or `go.sum' under
    #: `dir' is newer than the binary. [agfi:ensure-dep1] only asks "is it
    #: there", so an edited tool kept running its first build indefinitely.
    #:
    #: The guards that call this sit on hot paths (every render, hold and
    #: preview), so the fresh case forks nothing. The binary is found with
    #: `whence -p' and read back through `=name' expansion, both builtins that
    #: stop at the first hit; `$commands' is never read, since its first read
    #: fills the whole command hash (60ms here, with 355 PATH entries). A
    #: loop over `$path' in zsh measured 0.5ms, twice the builtins. Freshness
    #: is one glob with qualifiers plus `-nt'. `(#q...)' rather than bare
    #: qualifiers, which `no_bare_glob_qual' (set interactively) would read
    #: as a pattern. Measured on 2026-09-23: 0.7-1.0ms per call, against
    #: 0.2ms for the bare `whence -p' probe this replaced.
    #:
    #: A failed rebuild warns and keeps the old binary, so a half-written edit
    #: does not take the tool down with it; so does a stale binary on a host
    #: without Go. Build output goes to stderr, since callers often have
    #: stdout redirected into a document.
    #:
    #: `--optional': with neither binary nor Go, return 1 quietly, for a
    #: caller that has a slower fallback.
    #: Usage: go-local-dep [--optional] <name> <module-dir>
    ##
    setopt local_options extended_glob

    local optional=n
    if [[ "${1}" == --optional ]] ; then
        optional=y
        shift
    fi
    local name="${1}" dir="${2}"
    if [[ -z "${name}" || -z "${dir}" ]] ; then
        ecerr "$0: usage: $0 [--optional] <name> <module-dir>"
        return 1
    fi

    if whence -p "${name}" > /dev/null 2>&1 ; then
        local bin=( ="${name}" )
        local newest=( "${dir}"/**/(*.go~*_test.go|go.mod|go.sum)(#q.Nom[1]) )
        if (( ${#newest} == 0 )) || [[ ! "${newest[1]}" -nt "${bin[1]}" ]] ; then
            return 0
        fi
        if ! whence -p go > /dev/null 2>&1 ; then
            return 0
        fi

        ecerr "$0: ${name} is older than ${newest[1]#${dir}/}; rebuilding"
        if ! ( builtin cd -- "${dir}" && command go install ) >&2 ; then
            ecerr "$0: rebuilding ${name} failed; keeping the old binary"
            return 0
        fi
        if [[ "${newest[1]}" -nt "${bin[1]}" ]] ; then
            #: `go install' wrote somewhere else, and PATH still finds the old
            #: copy first; without this every call would rebuild.
            ecerr "$0: rebuilt ${name}, but ${bin[1]} is still the one on PATH; remove it or put GOBIN first"
        fi
        return 0
    fi

    if ! whence -p go > /dev/null 2>&1 ; then
        if [[ "${optional}" == y ]] ; then
            return 1
        fi
        ecerr "$0: missing command: go (needed to build ${name})"
        return 1
    fi

    ecerr "$0: building ${name} from ${dir}"
    ( builtin cd -- "${dir}" && command go install ) >&2 @RET
    rehash
    if ! whence -p "${name}" > /dev/null 2>&1 ; then
        ecerr "$0: could not install $(gquote-sq "${name}")"
        return 1
    fi
}
##
