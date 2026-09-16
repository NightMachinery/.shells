##
#: Publishing the html-reports site: interactive HTML served to the phone over Tailscale,
#: and mirrored to eva behind a Cloudflare Access login for everyone else.
#:
#: The registry, the builder and the Access policies are documented in
#: [[zf:~\[nt\]/skills/html-reports/SKILL.md][the skill]]. The one rule worth repeating
#: here: Access matches on hostname *plus path*, so the section a page sits in decides who
#: may read it. `LMU/` is shared with the supervisor; `pv/` is not shared with anyone.
##
typeset -g lilf_pages_bin="${HOME}/notes/skills/html-reports/bin"
##
function lilf-pages-build {
    : "usage: lilf-pages-build
Rebuild the site from the registry. Does not touch the mirror."

    assert "${lilf_pages_bin}/build.py" "$@" @RET
}
##
function lilf-pages-push {
    : "usage: lilf-pages-push [<section> ...]
Rebuild, then push the whole site, or only the named sections, to the mirror host."

    #: Sections are path prefixes as they appear in the registry's "at", e.g. LMU/linroute.
    #: Pushing one also refreshes the index pages above it, which would otherwise keep
    #: advertising a stale page count and freshness.
    #:
    #: push.py stops if the destination is newer than what it is about to send, which is
    #: what a push from another machine, or an edit on the far end, looks like. It asks on
    #: the terminal; with no terminal it refuses rather than hanging, so `yes_p` is what
    #: you want from a script.
    local yes_p="${lilf_pages_push_yes_p:-}"
    local dry_p="${lilf_pages_push_dry_p:-}"
    local build_p="${lilf_pages_push_build_p:-y}"

    local opts=()
    bool "${yes_p}" && opts+=(--yes)
    bool "${dry_p}" && opts+=(--dry-run)
    bool "${build_p}" || opts+=(--no-build)

    assert "${lilf_pages_bin}/push.py" "${opts[@]}" "$@" @RET
}

aliasfn lilf-pages-push-lmu lilf-pages-push LMU
aliasfn lilf-pages-push-linroute lilf-pages-push LMU/linroute
aliasfn lilf-pages-push-pv lilf-pages-push pv

#: @opts derives its prefix from the command name, so without these
#: `@opts yes_p y @ lilf-pages-push-pv` would set lilf_pages_push_pv_yes_p, which nothing reads.
@opts-setprefixas lilf-pages-push-lmu lilf-pages-push
@opts-setprefixas lilf-pages-push-linroute lilf-pages-push
@opts-setprefixas lilf-pages-push-pv lilf-pages-push
##
##
#: Who may open which path.
#:
#: Access decides on hostname *plus* path, and the longest matching path wins, so the only
#: way to answer "can this person read that page" is to look at every application at once.
#: The dashboard shows the same thing at https://one.dash.cloudflare.com -> Access ->
#: Applications; this is here so the answer can be diffed, grepped, and read without a
#: browser.
#:
#: Needs a token. Mint one at https://dash.cloudflare.com/profile/api-tokens with the
#: single permission  Account -> Access: Apps and Policies -> Read, and append it to
#: ~/.privateShell as `export CLOUDFLARE_API_TOKEN=...`. That is the name wrangler,
#: flarectl and terraform all read, so one secret serves every Cloudflare tool. Read-only
#: is the whole point: this command must not be able to change who has access.
##
function h-cloudflare-api {
    : "usage: h-cloudflare-api <path>
GET one Cloudflare API v4 path and check that it succeeded. Prints the raw JSON."

    #: NOT `local path`: in zsh, `path` is the array tied to $PATH, and a special parameter
    #: stays special when it is made local -- so `local path="accounts"` sets PATH to
    #: "accounts" for the rest of the function, and the very next command is not found.
    #: The error it produces says "command not found: curl", which points nowhere near the
    #: assignment that caused it.
    local endpoint="${1#/}"
    assert-args endpoint @RET

    #: The token goes over stdin, never in argv. An argument is visible in `ps` to every
    #: process on the machine, and this token reads every policy and every address allowed
    #: by one. `--config -` is curl's own way of taking headers from a file, and a pipe
    #: is a file.
    local out ret
    out="$(print -r -- "header = \"Authorization: Bearer ${CLOUDFLARE_API_TOKEN}\"
silent
show-error" | command curl --config - "https://api.cloudflare.com/client/v4/${endpoint}")"
    ret=$?
    if (( ret != 0 )) ; then
        ecerr "$0: curl failed (${ret}): ${endpoint}"
        return $ret
    fi

    #: An HTTP 200 carrying {"success": false} is the normal way this API refuses, so the
    #: body decides, not the status line.
    if test "$(print -r -- "${out}" | command jq -r '.success // false')" != 'true' ; then
        ecerr "$0: Cloudflare refused ${endpoint}:"
        print -r -- "${out}" | command jq -r '(.errors // [])[] | "  \(.code): \(.message)"' >&2
        return 1
    fi

    print -r -- "${out}"
}
##
function h-cloudflare-account-id {
    : "usage: h-cloudflare-account-id
\$CLOUDFLARE_ACCOUNT_ID if set, else the only account the token can see."

    if test -n "${CLOUDFLARE_ACCOUNT_ID}" ; then
        print -r -- "${CLOUDFLARE_ACCOUNT_ID}"
        return 0
    fi

    #: Discovering it needs a second permission the Access-read token may not have, which
    #: is fine: the id is not a secret, and naming it in the environment costs one line and
    #: one round trip less. The error says so rather than leaving you guessing.
    local json
    json="$(h-cloudflare-api accounts)" || {
        ecerr "$0: could not list accounts; set CLOUDFLARE_ACCOUNT_ID instead."
        ecerr "  It is in the URL of the Cloudflare dashboard, after /accounts/ ."
        return 1
    }

    local -a ids
    ids=(${(f)"$(print -r -- "${json}" | command jq -r '.result[].id')"})
    if (( ${#ids} == 1 )) ; then
        print -r -- "${ids[1]}"
        return 0
    fi

    ecerr "$0: the token sees ${#ids} accounts; set CLOUDFLARE_ACCOUNT_ID to one of:"
    print -r -- "${json}" | command jq -r '.result[] | "  \(.id)  \(.name)"' >&2
    return 1
}
##
#: The rule shapes are {"everyone": {}}, {"email": {"email": "..."}},
#: {"email_domain": {"domain": "..."}}, {"ip": {"ip": "..."}} and a dozen more, all the
#: same shape: one key naming the kind, an object of values under it. Rendering them
#: generically means a rule type nobody has used yet still prints, instead of vanishing.
typeset -g lilf_pages_access_jq='
def rules:
  [ .[] | to_entries[]
    | .key as $kind
    | (.value | to_entries)
    | if length == 0 then $kind
      else $kind + ": " + ([.[] | .value | tostring] | join(", ")) end
  ] | join(", ");
def clause($label; $rs):
  if ($rs // [] | length) > 0 then "      " + $label + "  " + ($rs | rules) else empty end;
(.domain // "?") + "   (" + (.type // "?") + ", session " + (.session_duration // "-") + ")",
( if ($p | length) == 0 then "  NO POLICY -- this application admits nobody" else empty end ),
( $p[]
  | "  " + (.decision // "?") + "  " + (.name // "(unnamed)"),
    clause("include"; .include),
    clause("require"; .require),
    clause("exclude"; .exclude)
),
""
'
##
function lilf-pages-access {
    : "usage: lilf-pages-access
Print every Cloudflare Access application on the account, and the policies that say who
may open it. Ordered by path, so the entry that decides a given URL is the last one that
matches it.

Set json_p to print the raw API objects instead."

    local json_p="${lilf_pages_access_json_p:-}"

    if test -z "${CLOUDFLARE_API_TOKEN}" ; then
        ecerr "$0: \$CLOUDFLARE_API_TOKEN is not set."
        ecerr "  Mint one at https://dash.cloudflare.com/profile/api-tokens with the single"
        ecerr "  permission  Account -> Access: Apps and Policies -> Read, then append it to"
        ecerr "  ~/.privateShell as:  export CLOUDFLARE_API_TOKEN=..."
        ecerr "  Meanwhile the dashboard shows the same thing:"
        ecerr "  https://one.dash.cloudflare.com -> Access -> Applications"
        return 1
    fi

    local account
    account="$(h-cloudflare-account-id)" @RET

    local apps
    apps="$(h-cloudflare-api "accounts/${account}/access/apps")" @RET

    local -a ids
    #: Sorted by domain, which carries the path, so a parent application sorts above the
    #: longer path that overrides it -- the order in which you have to read them.
    ids=(${(f)"$(print -r -- "${apps}" | command jq -r '[.result[]] | sort_by(.domain // "")[] | .id')"})
    if (( ${#ids} == 0 )) ; then
        ecgray "$0: no Access applications on this account"
        return 0
    fi

    local id app policies
    for id in ${ids[@]} ; do
        app="$(print -r -- "${apps}" | command jq -c --arg id "${id}" '.result[] | select(.id == $id)')" @RET
        policies="$(h-cloudflare-api "accounts/${account}/access/apps/${id}/policies")" @RET

        if bool "${json_p}" ; then
            print -r -- "${app}" | command jq --argjson p "$(print -r -- "${policies}" | command jq -c '.result // []')" '. + {policies: $p}'
        else
            print -r -- "${app}" | command jq -r --argjson p "$(print -r -- "${policies}" | command jq -c '.result // []')" "${lilf_pages_access_jq}"
        fi
    done
}
##
