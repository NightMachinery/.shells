##
#: Publishing the html-reports site: interactive HTML served to the phone over Tailscale,
#: and mirrored to eva behind a Cloudflare Access login for everyone else.
#:
#: The registry, the builder and the Access policies are documented in
#: [[zf:~\[nt\]/skills/html-reports/SKILL.md][the skill]]. The one rule worth repeating
#: here: Access matches on hostname *plus path*, so the section a page sits in decides who
#: may read it. `LMU/` is shared with the supervisor; `pv/` is not shared with anyone.
##
typeset -g html_pages_bin="${HOME}/notes/skills/html-reports/bin"
##
function pages-build {
    : "usage: pages-build
Rebuild the site from the registry. Does not touch the mirror."

    assert "${html_pages_bin}/build.py" "$@" @RET
}
##
function pages-push {
    : "usage: pages-push [<section> ...]
Rebuild, then push the whole site, or only the named sections, to the mirror host."

    #: Sections are path prefixes as they appear in the registry's "at", e.g. LMU/linroute.
    #: Pushing one also refreshes the index pages above it, which would otherwise keep
    #: advertising a stale page count and freshness.
    #:
    #: push.py stops if the destination is newer than what it is about to send, which is
    #: what a push from another machine, or an edit on the far end, looks like. It asks on
    #: the terminal; with no terminal it refuses rather than hanging, so `yes_p` is what
    #: you want from a script.
    local yes_p="${pages_push_yes_p:-}"
    local dry_p="${pages_push_dry_p:-}"
    local build_p="${pages_push_build_p:-y}"

    local opts=()
    bool "${yes_p}" && opts+=(--yes)
    bool "${dry_p}" && opts+=(--dry-run)
    bool "${build_p}" || opts+=(--no-build)

    assert "${html_pages_bin}/push.py" "${opts[@]}" "$@" @RET
}

aliasfn pages-push-lmu pages-push LMU
aliasfn pages-push-linroute pages-push LMU/linroute
aliasfn pages-push-pv pages-push pv

#: @opts derives its prefix from the command name, so without these
#: `@opts yes_p y @ pages-push-pv` would set pages_push_pv_yes_p, which nothing reads.
@opts-setprefixas pages-push-lmu pages-push
@opts-setprefixas pages-push-linroute pages-push
@opts-setprefixas pages-push-pv pages-push
##
