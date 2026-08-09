#!/usr/bin/env sh
# -*- mode: sh; sh-shell: sh; -*-
#: Stage 25: the `g`-prefixed GNU tool names, on Linux.
#:
#: Why this is not optional. The dotfiles are written against macOS + Homebrew
#: `coreutils`, which installs GNU tools as gmv/gcp/gdate/grealpath/... On
#: Linux the same tools are GNU already but *unprefixed*, and only three
#: aliases (gxargs/gfind/gsed) existed in zshlang/basic/crossplatform.zsh.
#:
#: The damage is not cosmetic. zshlang/basic/auto-load/enhanced.zsh defines a
#: `mv` *function* that calls `command gmv`, so every mv in the shell -- and
#: compinit's own dumpfile rename -- fails with:
#:   command not found: gmv '-f' '~/.zcompdump.<host>.<pid>' '~/.zcompdump'
#: 81 call sites use grealpath, 34 gdate, 33 gsort, 27 ghead, 24 gmv.
#:
#: Symlinks, not aliases: aliases only exist in an interactive shell that
#: sourced the config, while the codebase also does `${commands[gdate]}` and
#: runs these from scripts and subshells.

# shellcheck disable=SC1091
. "${HOME}/.night-bootstrap.env"

case "$(uname -s)" in
    Linux) : ;;
    *) ok "not Linux; g-prefixed names come from Homebrew coreutils" ; return 0 2>/dev/null || exit 0 ;;
esac

ensure_dir "${NIGHT_BIN}"

##
#: Prefer the authoritative package listing; fall back to a fixed list.
gnu_srcs=''
if have dpkg-query ; then
    gnu_srcs="$(dpkg-query -L coreutils 2>/dev/null | grep -E '^/usr/bin/[a-z0-9.+-]+$')"
fi
if [ -z "${gnu_srcs}" ] ; then
    warn "dpkg-query unavailable; using a fixed coreutils list"
    for c in cat chgrp chmod chown cp cut date dd df dir du echo env expand expr \
             factor fmt fold head id join link ln ls md5sum mkdir mkfifo mknod \
             mktemp mv nice nl nohup nproc numfmt od paste pathchk pinky pr \
             printenv printf ptx pwd readlink realpath rm rmdir seq shred shuf \
             sleep sort split stat stdbuf stty sum sync tac tail tee test timeout \
             touch tr true false truncate tsort tty uname unexpand uniq unlink \
             users vdir wc who whoami yes basename dirname comm csplit ; do
        [ -x "/usr/bin/${c}" ] && gnu_srcs="${gnu_srcs}
/usr/bin/${c}"
    done
fi

#: Not coreutils, but the config expects the g-names for these too.
for c in sed grep egrep fgrep find xargs awk tar make diff patch time ; do
    if [ -x "/usr/bin/${c}" ] ; then
        gnu_srcs="${gnu_srcs}
/usr/bin/${c}"
    fi
done

##
made=0 ; skipped=0
for src in ${gnu_srcs} ; do
    [ -x "${src}" ] || continue
    base="$(basename "${src}")"
    dest="${NIGHT_BIN}/g${base}"

    #: Skip only on a REAL collision: prefixing must not shadow an existing
    #: different program. zip->gzip, cc->gcc, unzip->gunzip and it->git would
    #: all hijack a real binary. Keying on "base starts with g" instead would
    #: wrongly drop grep->ggrep, which the config uses 11 times.
    existing="$(command -v "g${base}" 2>/dev/null || true)"
    case "${existing}" in
        ''|"${NIGHT_BIN}/g${base}") : ;;
        *)
            dim "skip g${base}: would shadow ${existing}"
            skipped=$((skipped + 1))
            continue
            ;;
    esac

    #: Do not clobber something already on PATH that is not ours.
    if [ -e "${dest}" ] && [ ! -L "${dest}" ] ; then
        warn "not replacing non-symlink ${dest}"
        skipped=$((skipped + 1))
        continue
    fi

    ln -sfn "${src}" "${dest}"
    made=$((made + 1))
done

#: gawk is its own binary on Ubuntu (plain `awk` is often mawk, which the
#: config's gawk call sites would silently misbehave under).
if [ -x /usr/bin/gawk ] ; then
    ln -sfn /usr/bin/gawk "${NIGHT_BIN}/gawk"
    dim "gawk -> /usr/bin/gawk ($(gawk --version 2>&1 | head -1))"
else
    warn "gawk missing; ${NIGHT_BIN}/gawk points at awk, which may be mawk"
fi

ok "linked ${made} g-prefixed GNU tools into ${NIGHT_BIN} (${skipped} skipped)"

##
#: The ones the codebase leans on hardest -- fail loudly if any is absent.
missing=''
for c in gmv gcp grm gls gdate gsort ghead gtail gtr gcut gwc gstat gdu gdf \
         grealpath greadlink gtimeout gnice gnumfmt gshuf gtac gsplit gseq \
         gmkdir gtouch gbasename gdirname gsed ggrep gfind gxargs gawk ; do
    if ! [ -x "${NIGHT_BIN}/${c}" ] ; then
        missing="${missing} ${c}"
    fi
done

if [ -n "${missing}" ] ; then
    warn "still missing:${missing}"
else
    ok "all g-prefixed tools the dotfiles use are present"
fi
