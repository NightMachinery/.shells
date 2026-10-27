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
  #: No global off/on toggle around --add: the old advice that ALF ignores
  #: --add and --unblockapp while running (mosh issue 898, 2017) did not hold
  #: on Darwin 23 in 2026-10, where the plain sequence below fixed a live
  #: failure. A toggle would also leave the firewall OFF if we died between
  #: the two calls.
  #:
  #: @warn RE-RUN WHENEVER MOSH HANGS, and after every `brew upgrade mosh`.
  #: We register the resolved Cellar path, which is version-stamped
  #: (.../Cellar/mosh/<ver>/bin/mosh-server), so an upgrade invalidates the
  #: registration. But an entry has also gone stale with no upgrade at all,
  #: while ALF kept listing it as allowed.
  #:
  #: So `--check` is NOT proof: it reports only what ALF claims, and that
  #: claim said "permitted" while every packet was being dropped. `--probe`
  #: is the real test: it has a peer send UDP to a disposable mosh-server and
  #: counts the bytes that arrive.
  #:
  #: This is only one of two layers. A peer reaching us over Tailscale also
  #: needs the pf anchor to pass 100.64.0.0/10; see
  #: [[NIGHTDIR:launchers/pf/install.org]].
  #:
  #: @usage firewall-allow-mosh-darwin                      #: register (needs root)
  #: @usage firewall-allow-mosh-darwin --check              #: ALF's claim only; no root
  #: @usage firewall-allow-mosh-darwin --probe PEER [IP]    #: real UDP test; no root
  #:
  #: PEER is an ssh host with `nc` (ncat or BSD) that can reach IP, which
  #: defaults to our Tailscale IPv4. `--probe tealy`, for example.
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

  if [[ "$1" == '--probe' ]] ; then
    h-firewall-mosh-probe-darwin "$2" "$3"
    return $?
  fi

  if [[ "$1" == '--check' ]] ; then
    if h-firewall-allow-app-darwin --check "$0" "$mosh_sym" "$mosh_abs" ; then
      ec "$0: That is ALF's claim, not proof; \`--probe PEER\` tests real traffic."
      return 0
    else
      ecerr "$0: run \`${0}\` to register it (mosh will hang on UDP until you do)."
      return 1
    fi
  fi

  h-firewall-allow-app-darwin "$0" "$mosh_sym" "$mosh_abs" @RET

  firewall-allow-mosh-darwin --check
  ecgray "$0: for real proof, run \`$0 --probe PEER\` from here."
}

function h-firewall-allow-app-darwin() {
  #: Register an ad-hoc signed binary with the macOS Application Firewall (ALF),
  #: or with --check report what ALF claims about it. Shared by
  #: [agfi:firewall-allow-mosh-darwin] and [agfi:tailscale-firewall-allow-darwin];
  #: the former explains why such binaries need this.
  #:
  #: @usage h-firewall-allow-app-darwin [--check] CALLER SYMLINK_PATH RESOLVED_PATH
  ##
  local check_p=n
  if [[ "$1" == '--check' ]] ; then
    check_p=y
    shift
  fi
  local caller="$1" sym="$2" abs="$3"
  assert-args caller sym abs @RET

  local fw='/usr/libexec/ApplicationFirewall/socketfilterfw'
  if ! test -x "$fw" ; then
    ecerr "${caller}: no socketfilterfw at ${fw} (not macOS?)"
    return 1
  fi

  #: Reads need no privileges, so --check stays usable from a non-interactive
  #: session and cannot prompt for anything.
  if bool "${check_p}" ; then
    reval-ec "$fw" --getglobalstate
    reval-ec "$fw" --getstealthmode

    local p
    for p in "$sym" "$abs" ; do
      reval-ec "$fw" --getappblocked "$p"
    done

    reval-ec "$fw" --listapps | grep -F -- "${abs:t}"

    #: The RESOLVED path is the one that decides it; ALF canonicalises symlinks
    #: on --add and stores the real binary, so the SYMLINK line above keeps
    #: reporting "not part of the firewall" even after a successful run. That
    #: is expected, not a failure. socketfilterfw also exits 0 for an
    #: unregistered app, so the text is the only signal either way.
    if "$fw" --getappblocked "$abs" 2>/dev/null | grep -q -- 'is permitted' ; then
      ec "${caller}: ALF lists ${abs} as permitted."
      return 0
    else
      ecerr "${caller}: NOT ALLOWED -- ${abs}"
      return 1
    fi
  fi

  #: [agfi:h-sudo-cmd] explains why this is not just `sudo': a bare sudo with
  #: no controlling terminal hangs rather than failing.
  local sudo_cmd=( ${(@f)"$(h-sudo-cmd)"} )

  #: --remove first: a stale entry still lists as allowed, and --add on an
  #: already-present path is a no-op, so only remove-then-add refreshes it.
  #: Only the resolved path: ALF canonicalises symlinks on --add anyway.
  reval-ec "$sudo_cmd[@]" "$fw" --remove "$abs"
  reval-ec "$sudo_cmd[@]" "$fw" --add "$abs" @RET
  reval-ec "$sudo_cmd[@]" "$fw" --unblockapp "$abs" @RET
}

function h-firewall-mosh-probe-darwin() {
  #: Start a disposable mosh-server, have PEER send it UDP over ssh, and count
  #: the bytes that reach its socket. ALF drops before delivery, so a blocked
  #: server shows 0 bytes in nettop. Returns 0 if packets arrived, 1 if none
  #: did, 2 if the test itself could not run.
  ##
  local peer="$1" ip="$2"
  assert-args peer @RET
  if test -z "$ip" ; then
    ip="$(command tailscale ip -4 2>/dev/null | command head -n 1)"
  fi
  if test -z "$ip" ; then
    ecerr "$0: no IP given and no Tailscale IPv4 found"
    return 2
  fi

  local port=$(( 60900 + RANDOM % 100 ))
  #: mosh-server forks and its parent exits at once, so find the server by
  #: its socket, then confirm the command line is ours before trusting it.
  command mosh-server new -i "$ip" -p "$port" -- /bin/sleep 120 >/dev/null 2>&1
  local pid
  pid="$(command lsof -tnP -iUDP@"${ip}:${port}" 2>/dev/null | command head -n 1)"
  if test -z "$pid" || ! command ps -o command= -p "$pid" | command grep -q -- "-p ${port} -- /bin/sleep 120" ; then
    ecerr "$0: could not start a test mosh-server on ${ip}:${port}"
    return 2
  fi

  {
    #: ncat needs --send-only to exit after stdin ends; BSD nc rejects that
    #: flag, so fall back to its -w1.
    local remote="s() { printf probe | nc -u --send-only $ip $port 2>/dev/null || printf probe | nc -u -w1 $ip $port; }; s; s; s"
    if ! command timeout 30 command ssh "$peer" "$remote" ; then
      ecerr "$0: could not send from ${peer}"
      return 2
    fi
    command sleep 1

    local bytes
    bytes="$(command nettop -m udp -L1 -x -p "$pid" 2>/dev/null | command awk -F, -v n="mosh-server.${pid}" '$2 == n { print $5 }')"
    if (( ${bytes:-0} > 0 )) ; then
      ec "$0: OK -- mosh-server on ${ip}:${port} received ${bytes} bytes from ${peer}."
      return 0
    else
      ecerr "$0: BLOCKED -- mosh-server on ${ip}:${port} received nothing from ${peer}."
      ecerr "$0: if a plain UDP listener does receive, re-register with \`firewall-allow-mosh-darwin\`."
      return 1
    fi
  } always {
    command kill "$pid" 2>/dev/null
  }
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
