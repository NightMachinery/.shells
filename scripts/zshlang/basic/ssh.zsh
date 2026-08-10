##
function mosh {
  bella_zsh_disable1

  terminfo-set-auto # with a bad terminfo, we can get the error: `terminals database is inaccessible`

  command mosh --server="TERM=$TERM TERM_PROGRAM=$TERM_PROGRAM KITTY_WINDOW_ID=$KITTY_WINDOW_ID ITERM_SESSION_ID=$ITERM_SESSION_ID BICON_MODE=$BICON_MODE mosh-server" "$@" # -- zsh
}
##
if [ -n "$SSH_CLIENT" ] || [ -n "$SSH_TTY" ] || [[ -n "$SSH_CONNECTION" ]] ; then
    amSSH=remote/ssh
else
    #: Forking is expensive.
    # case $(command ps -o comm= -p $PPID) in
    #     sshd|*/sshd) amSSH=remote/ssh;;
    # esac
fi

function isSSH() {
  test -n "$amSSH"
}
##

##
#: --- variables smuggled through ssh ---
#:
#: sshd forwards only what its AcceptEnv permits, and the widespread default
#: is `AcceptEnv LANG LC_*`; anything else is dropped silently. That is why a
#: client-side `SendEnv TERM_PROGRAM KITTY_WINDOW_ID` never reached the CIS
#: servers, and why KITTY_WINDOW_ID read as empty there.
#:
#: Convention: the sending side exports LC_<NAME>, the receiving side restores
#: <NAME>. The two functions below are the two halves of that.
##
typeset -ga env_smuggled_lc_vars
env_smuggled_lc_vars=( COLORFGBG TERM_PROGRAM KITTY_WINDOW_ID )
#: COLORFGBG is the load-bearing one: a terminal cannot be asked for its
#: background colour, so without it Emacs guesses -- and guesses dark, which
#: renders every face for the wrong polarity on a light terminal.

function env-save-smuggled-lc-vars {
    #: Client side. Mirror into LC_-prefixed copies so ssh will carry them.
    #: Static values can come from the terminal itself (kitty.conf `env`), but
    #: KITTY_WINDOW_ID differs per window, so it must be mirrored at runtime.
    local v
    for v in "${env_smuggled_lc_vars[@]}" ; do
        if [[ -n "${(P)v}" && -z "${(P)${:-LC_$v}}" ]] ; then
            export "LC_$v"="${(P)v}"
        fi
    done
}

function env-load-smuggled-lc-vars {
    #: Server side. Restore the plain names, never clobbering a value the
    #: local environment already set. Called from ~/.night-bootstrap.env.
    local v lc
    for v in "${env_smuggled_lc_vars[@]}" ; do
        lc="LC_$v"
        if [[ -n "${(P)lc}" && -z "${(P)v}" ]] ; then
            export "$v"="${(P)lc}"
        fi
    done
}

env-save-smuggled-lc-vars
