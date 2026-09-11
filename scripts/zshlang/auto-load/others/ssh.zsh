##
function ashL {
    local a=() i
    for i in "$@[2,-1]"
    do
        a+=(-L "${i}:localhost:${i}")
    done
    ash -NT "$1" "$a[@]"
}
##
function kitty-terminfo-install() {
    infocmp -x xterm-kitty | ssh "$@" tic -x -o \~/.terminfo/ /dev/stdin
}

function ssh {
  bella_zsh_disable1

  {
    if isDeus && fn-isTop && isKitty ; then
      # if fn-isTop && isKitty ; then
      # will install the xterm-kitty terminal definition on the remote in your home directory.
      # Only needs to run once per host
      reval-ec kitty +kitten ssh "$@"
    else
      reval-ec command ssh "$@"
    fi
  } always {
    reval-ecgray stty sane
  }
}
##
function firewall-allow-mosh-darwin() {
  #: Register mosh-server with the macOS Application Firewall (ALF).
  #:
  #: WHY: Homebrew's mosh-server is ad-hoc (linker) signed, so ALF's
  #: "automatically allow downloaded signed software" does not cover it. With
  #: the firewall on, and stealth mode especially, its inbound UDP is dropped
  #: SILENTLY -- mosh hangs with "Nothing received from server on UDP port
  #: 600xx" while ssh to the same host works fine, because sshd and Tailscale
  #: SSH are unaffected. Normally macOS would ask, but mosh-server is spawned
  #: over ssh with no GUI session to ask in, so it defaults to deny.
  #:
  #: The global off/on toggle is not superstition: ALF ignores --add and
  #: --unblockapp while it is running.
  #: https://github.com/mobile-shell/mosh/issues/898
  #:
  #: @warn RE-RUN AFTER EVERY `brew upgrade mosh`. We register the resolved
  #: Cellar path, which is version-stamped (.../Cellar/mosh/<ver>/bin/mosh-server),
  #: so an upgrade silently invalidates the registration and mosh starts hanging
  #: again. `--check` is the cheap way to confirm.
  #:
  #: This is only one of two layers. A peer reaching us over Tailscale also
  #: needs the pf anchor to pass 100.64.0.0/10; see
  #: [[NIGHTDIR:launchers/pf/install.org]].
  #:
  #: @usage firewall-allow-mosh-darwin          #: register (needs root)
  #: @usage firewall-allow-mosh-darwin --check  #: report only; no root, no changes
  ##
  local fw='/usr/libexec/ApplicationFirewall/socketfilterfw'
  if ! test -x "$fw" ; then
    ecerr "$0: no socketfilterfw at ${fw} (not macOS?)"
    return 1
  fi

  local mosh_sym="${commands[mosh-server]}"
  if test -z "$mosh_sym" ; then
    ecerr "$0: mosh-server is not in PATH"
    return 1
  fi
  local mosh_abs
  mosh_abs="$(greadlink -f "$mosh_sym")" @TRET

  #: Reads need no privileges, so --check stays usable from a non-interactive
  #: session and cannot prompt for anything.
  if [[ "$1" == '--check' ]] ; then
    reval-ec "$fw" --getglobalstate
    reval-ec "$fw" --getstealthmode

    local p
    for p in "$mosh_sym" "$mosh_abs" ; do
      reval-ec "$fw" --getappblocked "$p"
    done

    reval-ec "$fw" --listapps | grep -i -- mosh

    #: The RESOLVED path is the one that decides it; ALF canonicalises symlinks
    #: on --add and stores the real binary, so the SYMLINK line above keeps
    #: reporting "not part of the firewall" even after a successful run. That
    #: is expected, not a failure. socketfilterfw also exits 0 for an
    #: unregistered app, so the text is the only signal either way.
    if "$fw" --getappblocked "$mosh_abs" 2>/dev/null | grep -q -- 'is permitted' ; then
      ec "$0: OK -- ${mosh_abs} may accept incoming connections."
      return 0
    else
      ecerr "$0: NOT ALLOWED -- ${mosh_abs}"
      ecerr "$0: run \`${0}\` to register it (mosh will hang on UDP until you do)."
      return 1
    fi
  fi

  #: Plain sudo reads the password from /dev/tty, so with no controlling
  #: terminal it does not fail -- it HANGS until something times it out. Test
  #: for the tty itself rather than for an interactive shell: `zsh -ic' from an
  #: agent is interactive by every other measure and still has no tty. -A falls
  #: back to SUDO_ASKPASS there; -k so a warm timestamp cannot make a broken
  #: askpass setup look like it worked.
  local sudo_cmd=(sudo)
  if ! { : < /dev/tty } 2>/dev/null ; then
    sudo_cmd=(sudo -kA)
  fi

  reval-ec "$sudo_cmd[@]" "$fw" --setglobalstate off

  #: --remove first so a re-run after an upgrade cannot leave the stale entry
  #: behind; --add on an already-present path is otherwise a no-op.
  reval-ec "$sudo_cmd[@]" "$fw" --remove "$mosh_sym"
  reval-ec "$sudo_cmd[@]" "$fw" --remove "$mosh_abs"

  reval-ec "$sudo_cmd[@]" "$fw" --add "$mosh_sym"
  reval-ec "$sudo_cmd[@]" "$fw" --unblockapp "$mosh_sym"

  reval-ec "$sudo_cmd[@]" "$fw" --add "$mosh_abs"
  reval-ec "$sudo_cmd[@]" "$fw" --unblockapp "$mosh_abs"

  reval-ec "$sudo_cmd[@]" "$fw" --setglobalstate on

  #: Verify rather than trusting the exit codes above.
  firewall-allow-mosh-darwin --check
}
##
function ash {
    bella_zsh_disable1

    autossh -M 0 -o "ServerAliveInterval 30" -o "ServerAliveCountMax 3" "$@"
}
##
function ssh-run-in-shell {
  #: @examples
  #: `ssh-run-in-shell c0 exit 7` (returns 7)
  ##
  local fullhost="$1" ; shift
  assert-args fullhost @RET

  local cmd="$*"

  revaldbg command ssh -o ControlPath=none "${fullhost}" "${cmd}"
}
##
