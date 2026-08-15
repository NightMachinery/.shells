#!/usr/bin/env sh
# -*- mode: sh; sh-shell: sh; -*-
#: Profile: CIS LMU servers (beta, rho{1,2}, zeta{1,2}, epsilon{1..7}).
#:
#: Facts this profile encodes (probed [2026-08-09], see
#: nightNotesPrivate/research/Hinrich Schutze/servers/bootstrap/plan.org):
#:   - $HOME is ONE NFS mount shared by every host, quota ~48 GB.
#:   - /mounts/work and /nfs/gdata are shared NFS, writable, NO quota.
#:   - /var/tmp is local ext4, ~10x faster for small files, per host,
#:     and survives reboots (/tmp does NOT: tmpfiles wipes it at boot).
#:   - No sudo. No writable /opt or /usr/local. No environment-modules.
#:   - /opt/miniconda3 exists ONLY on beta -- never depend on it.

NIGHT_PROFILE_NAME='cis-lmu'

#: What the stages actually branch on. "cis-lmu" is just a name; these two
#: facts are what make this site different from a plain sudo-less box.
#: `:=' for consistency with the storage paths above: overridable from the
#: environment, but both are simply true here, so overriding either is
#: telling the bootstrap something false about the cluster.
: "${NIGHT_HOME_SHARED:=y}"   #: one NFS home across ~12 hosts
: "${NIGHT_MULTIUSER:=y}"     #: shared login nodes -- 8 other users were on beta

#: The big store. Single point of change; overridable from the environment:
#:   NIGHT_BIG_STORE=/nfs/gdata/feraidoon sh bootstrap.sh
#: Default is /mounts/work (7 TB free). /nfs/gdata (25 TB free) is the
#: alternative if the admins say work/ is swept.
: "${NIGHT_BIG_STORE:=/mounts/work/${USER}}"

#: Per-host local scratch. Regenerable content ONLY.
: "${NIGHT_LOCAL_CACHE:=/var/tmp/${USER}}"

#: Small and precious; inside the quota.
: "${NIGHT_BIN:=${HOME}/.local/bin}"

#: These hosts have direct international internet access.
: "${BOOTSTRAP_WITH_PROXY:=n}"
