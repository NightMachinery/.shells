#!/usr/bin/env sh
# -*- mode: sh; sh-shell: sh; -*-
#: Stage 00: storage layout.
#: Creates the three tiers and writes the env contract that later stages,
#: and the interactive shell, both read.

ensure_dir "${NIGHT_BIN}"
ensure_dir "${NIGHT_LOCAL_CACHE}"
ensure_dir "${HOME}/code"
ensure_dir "${HOME}/tmp"

#: The big store may be on a filesystem we cannot write (misconfigured
#: profile, unmounted NFS). Fail loudly here rather than halfway through a
#: 2 GB conda install.
if ! mkdir -p "${NIGHT_BIG_STORE}" 2>/dev/null ; then
    die "cannot create NIGHT_BIG_STORE=${NIGHT_BIG_STORE} (override it in the environment)"
fi
if ! ( : > "${NIGHT_BIG_STORE}/.write-test" ) 2>/dev/null ; then
    die "NIGHT_BIG_STORE=${NIGHT_BIG_STORE} is not writable"
fi
rm -f "${NIGHT_BIG_STORE}/.write-test"

ensure_dir "${NIGHT_BIG_STORE}/envs"
ensure_dir "${NIGHT_BIG_STORE}/cache"

#: The cache tiers the env contract points at. Created here so the first
#: process to use one does not race, and so TMPDIR exists before any stage
#: (or login shell) tries to use it.
ensure_dir "${NIGHT_BIG_STORE}/hf"
ensure_dir "${NIGHT_BIG_STORE}/torch"
for d in tmp xdg triton torchinductor nv pip npm ; do
    ensure_dir "${NIGHT_LOCAL_CACHE}/${d}"
done

##
#: --- the default cache paths, symlinked onto the big store ---
#: The env contract below exports HF_HOME and TORCH_HOME, but exports only help
#: a process that inherited them. `ssh host cmd' runs a NON-INTERACTIVE shell,
#: which sources neither ~/.zshrc nor anything the login path pulls in, so a
#: remote one-liner silently gets the *default* ~/.cache paths and fills the
#: 48GB home quota. That is not hypothetical: a remote eval launched this way
#: re-downloaded 30GB of a model that was already on the big store and died
#: with `Errno 122 Disk quota exceeded' partway through.
#:
#: A symlink needs no environment at all, so it also covers tools that ignore
#: the env vars and hardcode the default. Both sides are shared NFS here, so
#: one link serves every host.
#:
#: Only ever replace a symlink or an empty directory -- never a populated one,
#: which would strand real data.
for _pair in "huggingface:hf" "torch:torch" ; do
    _link="${HOME}/.cache/${_pair%%:*}"
    _target="${NIGHT_BIG_STORE}/${_pair##*:}"
    ensure_dir "$(dirname "${_link}")"
    if [ -L "${_link}" ] ; then
        [ "$(readlink "${_link}")" = "${_target}" ] || run_soft ln -sfn "${_target}" "${_link}"
    elif [ ! -e "${_link}" ] ; then
        run_soft ln -sfn "${_target}" "${_link}"
    elif [ -d "${_link}" ] && [ -z "$(ls -A "${_link}" 2>/dev/null)" ] ; then
        rmdir "${_link}" 2>/dev/null && run_soft ln -sfn "${_target}" "${_link}"
    else
        warn "not linking ${_link}: exists and is not empty (move it to ${_target} by hand)"
    fi
done
unset _pair _link _target

##
#: --- privacy on a shared login node ---
#: Only when other people can log in here; on a single-user box these would be
#: needless friction (e.g. you could no longer hand a collaborator a path).
#: These directories default to 755, which would expose models, datasets,
#: fitted lenses and logs to every other user of the cluster. The parent
#: /mounts/work is 1777 and not ours to change, but that does not matter: a
#: child directory's own mode gates access to its contents, and the parent's
#: sticky bit (the `t') is what stops others deleting our directory despite
#: the parent being world-writable.
if [ "${NIGHT_MULTIUSER}" = y ] ; then
    for d in "${NIGHT_BIG_STORE}" "${NIGHT_LOCAL_CACHE}" "${HOME}/code" ; do
        [ -d "${d}" ] && run_soft chmod 700 "${d}"
    done
fi

#: Existing top-level entries in $HOME were created under umask 022 and are
#: world-readable; $HOME is only 711 (traversable), so anyone who guesses a
#: standard name -- .privateShell, .zcompdump, .emacs.d -- can read it.
#: We deliberately do NOT chmod $HOME itself; see the umask note in the env
#: contract below.
if [ -n "${NIGHT_BOOTSTRAP_HARDEN_HOME:-}" ] ; then
    find "${HOME}" -maxdepth 1 -mindepth 1 \! -type l -exec chmod go-rwx {} + 2>/dev/null
    ok "removed group/other access from top-level \$HOME entries"
fi

##
#: --- site marker ---
#: Records which profile this home belongs to, so later runs (and other
#: programs, e.g. night/cis-p in doom.d/config.el) can *read* the answer
#: instead of inferring it. Named generically, and holding the profile name,
#: because the site is a variable here -- CIS is one value, not the concept.
#: Where $HOME is shared, one file covers every host in the cluster.
if [ ! -e "${night_site_file}" ] && [ "${NIGHT_PROFILE}" != "default" ] ; then
    {
        printf '%s\n' "${NIGHT_PROFILE}"
        cat <<'MARK'

# Written by setup/bootstrap-sudoless/stages/00-dirs.sh.
# The first non-comment line is the profile name.
#
# An explicit declaration on purpose: every inferrable signal was wrong in some
# way. A mount point only proves the share is mounted; a $HOME path prefix is a
# naming convention, not an identity; the DNS search domain tracks network
# connectivity (the laptop matched it over VPN); hostnames need per-machine
# upkeep and collide.
MARK
    } > "${night_site_file}"
    chmod 600 "${night_site_file}"
    ok "wrote ${night_site_file} (${NIGHT_PROFILE})"
fi

##
#: The env contract. Sourced by ~/.privateShell (stage 10) so an interactive
#: shell agrees with the bootstrap about where things live.
env_file="${HOME}/.night-bootstrap.env"

#: Baked in at generation time rather than tested at every shell start: the
#: contract is sourced by every single shell, and the answer cannot change
#: without rerunning this stage anyway.
if [ "${NIGHT_MULTIUSER}" = y ] ; then
    umask_stanza='umask 077'
else
    umask_stanza="#: single-user host: keeping the system default umask."
fi

cat > "${env_file}" <<EOF
# -*- mode: sh; -*-
#: Generated by setup/bootstrap-sudoless/stages/00-dirs.sh
#: Storage contract for this host. Edit NIGHT_BIG_STORE here (or in
#: setup/bootstrap-sudoless/profiles/) to relocate the heavy trees.
#:
#: ############################################################################
#: @warn THIS FILE IS SHARED BY EVERY HOST.
#:
#: It lives in \$HOME, and on the CIS cluster \$HOME is one NFS mount shared by
#: ~12 machines. So this is *one file* that beta, rho*, zeta* and epsilon* all
#: source. Consequences:
#:
#:   - Never write a host-specific *value* here. Write an expression that
#:     *derives* the value per host, e.g. EMACS_SOCKET_NAME below uses \\\$HOST.
#:     A literal hostname would be silently wrong on the other eleven.
#:   - Regenerating it on one host regenerates it for all of them. Stages
#:     append to it, so a rerun of stage 00 discards those appends.
#:   - It is chmod 600: it carries REDISCLI_AUTH. Keep it that way.
#:   - Values that must differ per host belong under \$NIGHT_LOCAL_CACHE
#:     (per-host local disk), not in here.
#: ############################################################################

export NIGHT_PROFILE='${NIGHT_PROFILE}'
export NIGHT_BIN='${NIGHT_BIN}'
export NIGHT_BIG_STORE='${NIGHT_BIG_STORE}'
export NIGHT_LOCAL_CACHE='${NIGHT_LOCAL_CACHE}'

#: Package managers: keep cache and envs on the SAME filesystem, or conda /
#: pixi hardlink dedup breaks and every env stores real copies.
export MAMBA_ROOT_PREFIX="\${NIGHT_BIG_STORE}/mamba"
export CONDA_ENVS_DIRS="\${NIGHT_BIG_STORE}/mamba/envs"
export CONDA_PKGS_DIRS="\${NIGHT_BIG_STORE}/mamba/pkgs"
export PIXI_HOME="\${NIGHT_BIG_STORE}/pixi"
export UV_CACHE_DIR="\${NIGHT_LOCAL_CACHE}/uv"

#: Doom's whole .local tree (straight repos, builds, eln cache) is tens of
#: thousands of small files: keep it off the quota'd home.
export DOOMLOCALDIR="\${NIGHT_BIG_STORE}/doom-local/"

#: mise manages the static CLI binaries and language runtimes.
export MISE_DATA_DIR="\${NIGHT_BIG_STORE}/mise"
export MISE_CACHE_DIR="\${NIGHT_LOCAL_CACHE}/mise"

##
#: --- caches ---
#: The split below is the whole policy, and it follows from two measurements
#: on beta (see CIS/caches.org):
#:
#:   sequential read, O_DIRECT   local 3395 MB/s   NFS 610 MB/s   (5.6x)
#:   file creation               local 0.1 ms      NFS 0.6 ms     (6x)
#:
#: So: things that are LARGE, IMMUTABLE and WORTH SHARING go to the big
#: store, where they are downloaded once for every host and cost no quota.
#: Things that are SMALL, NUMEROUS and REGENERABLE go to local disk, where
#: the 6x metadata advantage actually matters and nothing is lost if the
#: machine wipes them.
#:
#: Nothing goes to \$HOME. It is the only quota'd filesystem here (48 GB, and
#: one 27B checkpoint is 54 GB), and on a shared home every host would be
#: writing the same cache over NFS at once.
export HF_HOME="\${NIGHT_BIG_STORE}/hf"
export HUGGINGFACE_HUB_CACHE="\${NIGHT_BIG_STORE}/hf/hub"
export TORCH_HOME="\${NIGHT_BIG_STORE}/torch"

#: Compiler caches: thousands of tiny files, rebuilt on demand, and tied to
#: this host's GPU and driver -- sharing them between hosts buys nothing.
export TRITON_CACHE_DIR="\${NIGHT_LOCAL_CACHE}/triton"
export TORCHINDUCTOR_CACHE_DIR="\${NIGHT_LOCAL_CACHE}/torchinductor"
export CUDA_CACHE_PATH="\${NIGHT_LOCAL_CACHE}/nv"
export PIP_CACHE_DIR="\${NIGHT_LOCAL_CACHE}/pip"
export NPM_CONFIG_CACHE="\${NIGHT_LOCAL_CACHE}/npm"

#: Everything that follows the XDG spec. On a shared home this also removes a
#: correctness hazard, not just a slow one: ~/.cache would otherwise be one
#: directory written concurrently by every host we are logged into.
export XDG_CACHE_HOME="\${NIGHT_LOCAL_CACHE}/xdg"

#: /tmp here is world-writable and carries a tmpfiles rule that ages it out
#: (D /tmp 1777 root root 30d); /var/tmp has no such rule. A private TMPDIR
#: keeps our scratch off a directory every other user can list.
export TMPDIR="\${NIGHT_LOCAL_CACHE}/tmp"

#: @warn Self-healing, and not optional. This file lives in a SHARED \$HOME
#: but points at a PER-HOST directory, so on any host where stage 00 has not
#: run those paths do not exist -- and a TMPDIR that does not exist breaks
#: real things. It broke the emacs daemon on a second host with
#:   (file-missing "Creating directory with prefix" ... "/var/tmp/USER/tmp/babel-")
#: which is a confusing way to be told that \$TMPDIR is a dangling path.
#:
#: The \`[ -d ]' test is a shell builtin, so the common case costs no fork;
#: mkdir runs once per host, ever. That matters because this file is sourced
#: by every single shell.
for _night_d in "\${TMPDIR}" "\${XDG_CACHE_HOME}" "\${TRITON_CACHE_DIR}" \\
                "\${TORCHINDUCTOR_CACHE_DIR}" "\${CUDA_CACHE_PATH}" \\
                "\${PIP_CACHE_DIR}" "\${NPM_CONFIG_CACHE}" ; do
    [ -d "\${_night_d}" ] || mkdir -p "\${_night_d}" 2>/dev/null || true
done
unset _night_d

case ":\${PATH}:" in
    *":\${NIGHT_BIN}:"*) : ;;
    *) export PATH="\${NIGHT_BIN}:\${PATH}" ;;
esac

#: --- variables smuggled through ssh ---
#: sshd forwards only what AcceptEnv permits (commonly, and here, "LANG LC_*"),
#: so the client sends LC_<NAME> and we restore <NAME>. The implementation is
#: [agfi:env-load-smuggled-lc-vars] in zshlang/basic/ssh.zsh -- it is ordinary
#: shell functionality, not bootstrap-specific, so it lives with the rest of
#: the shell library rather than being duplicated here.
#:
#: Guarded because this file is also sourced by POSIX sh (the bootstrap
#: stages), where zshlang is not loaded; those contexts do not need it.
if command -v env-load-smuggled-lc-vars >/dev/null 2>&1 ; then
    env-load-smuggled-lc-vars
fi

#: Where other people can log in, the default umask 0022 creates every file
#: world-readable. 077 makes new files 600 and new dirs 700.
#: @note \$HOME itself stays at the cluster's 711 convention (103 of 108 homes
#: use it) rather than 700: sshd reads ~/.ssh/authorized_keys, and on an NFS
#: home with root_squash a 700 home risks breaking key auth. 711 plus private
#: contents gives the same privacy without the lockout risk.
${umask_stanza}

#: redis listens on 127.0.0.1, which keeps out other *hosts* but NOT other
#: *users* of this machine. Where that matters, redis runs with requirepass
#: and this exports the secret. redis-cli reads REDISCLI_AUTH automatically, so
#: [agfi:redis-cli-wrapper] and friends need no change.
#: /proc/PID/environ is owner-readable only, so exporting it does not leak it.
if [ -r "\${HOME}/.redis-auth" ] ; then
    #: \`read' rather than \$(cat ...): a command substitution forks, and this
    #: file is sourced by every shell. Measured at ~150ms per startup on the
    #: NFS-mounted CIS home -- worth avoiding for a single line of a file.
    #: \`|| true' because read reports failure when the file has no trailing
    #: newline, having nonetheless read the value.
    IFS= read -r REDISCLI_AUTH < "\${HOME}/.redis-auth" || true
    export REDISCLI_AUTH
fi
EOF

chmod 600 "${env_file}"
ok "wrote ${env_file} (chmod 600; shared by every host)"
dim "big store:   ${NIGHT_BIG_STORE}"
dim "local cache: ${NIGHT_LOCAL_CACHE}"

##
#: --- make the contract reach NON-interactive shells ---
#: `ssh host cmd' runs the LOGIN shell non-interactively, so anything wired up
#: only through the interactive path is invisible to remote one-liners. That is
#: how a remote job wrote 30GB to the home quota instead of the big store:
#: `ssh beta "echo \$HF_HOME"' printed nothing while `zsh -ic' printed it
#: correctly.
#:
#: Two files, because two shells with different rules, and the login shell on
#: the CIS hosts is bash even though the interactive shell is zsh:
#:   ~/.zshenv  read by EVERY zsh, interactive or not.
#:   ~/.bashrc  read by non-interactive bash ONLY when sshd starts it -- which
#:              is exactly the case we need. Note this breaks if the file ever
#:              grows the usual `[ -z "$PS1" ] && return' guard at the top,
#:              since our line is appended at the bottom.
#:
#: The env file is pure exports and one guarded `read', so it is cheap enough
#: to source unconditionally. Appended idempotently: neither file is ours.
for _rc in "${HOME}/.zshenv" "${HOME}/.bashrc" ; do
    if [ -e "${_rc}" ] && grep -q 'night-bootstrap\.env' "${_rc}" 2>/dev/null ; then
        dim "${_rc} already sources the env contract"
        continue
    fi
    cat >> "${_rc}" <<'EOF'

#: Storage contract (HF_HOME, TORCH_HOME, TMPDIR, ...). Sourced here rather than
#: from an interactive-only rc on purpose: `ssh host cmd' is non-interactive and
#: would otherwise get the default ~/.cache paths and fill the home quota.
#: Added by setup/bootstrap-sudoless stage 00.
[ -r "${HOME}/.night-bootstrap.env" ] && . "${HOME}/.night-bootstrap.env"
EOF
    ok "wired ${env_file} into ${_rc} (reaches non-interactive shells)"
done
unset _rc
