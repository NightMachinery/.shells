##
function gh-size {
    # @docs https://docs.github.com/en/rest/reference/repos
    #
    # https://stackoverflow.com/questions/8646517/how-can-i-see-the-size-of-a-github-repository-before-cloning-it
    # The size is indeed expressed in kilobytes based on the disk usage of the server-side bare repository. However, in order to avoid wasting too much space with repositories with a large network, GitHub relies on Git Alternates. In this configuration, calculating the disk usage against the bare repository doesn't account for the shared object store and thus returns an "incomplete" value through the API call.
    #
    # @alt If you own the repository, you can find the exact size by opening your /Account Settings/ → /Repositories/ ([[https://github.com/settings/repositories]]), and the repository size is displayed next to its designation.
    #
    # If you do not own the repository, you can fork it and then check the in the same place.
    #
    # This size is the same as the one reported by the API, i.e., it's less than the actual size by an unknown amount.
    ##
    local url="$1"
    local repo
    repo="$(gh-url2repo "$url")" @RET # asserts the nonemptiness of url itself

    local d
    d="$(gurl https://api.github.com/repos/"$repo")" @TRET

    local s
    s="$(ec $d | jqm .size)"
    s=$(( s * 1024 ))
    ec "$s" | numfmt-bytes
}

##
function gh-url2repo {
    local url="$1"
    local repo="$url"
    assert-args repo @RET
    if [[ "$repo" =~ '(?:https://)?github.com/([^/]+/[^/]+)' ]] ; then
        repo="$match[1]"
        # dvar repo
    fi

    ec "$repo"
}
##
function gh-release-get {
    #: Use =deus= to interactively select the version.
    ##
    local url="$1"
    local repo
    repo="$(gh-url2repo "$url")" @RET # asserts the nonemptiness of url itself

    local tag="${gh_release_get_tag}"
    local desired="${gh_release_get_desired}" # regex
    if false && test -z "$desired" ; then
        if isDarwin ; then
            # desired="macos"
            desired="mac|darwin|\.dmg"
        else
            desired="linux|ubuntu"
        fi
    # elif [[ "$desired" == . ]] ; then
    #     desired=''
    fi

    local releases
    releases="$(revaldbg gurl https://api.github.com/repos/"$repo"/releases)" @TRET


    if test -z "$tag" ; then
        if [[ "$url" =~ 'tag/([^/?]+)' ]] ; then
            tag="${match[1]}"
        fi
        if test -z "$tag" ; then
            if isDeus ; then
                tag="$(ec "$releases" | jq -r ".[].tag_name"  | gsort --version-sort --reverse | fz)" @RET
            else
                tag="$(ec "$releases" | jq -r ".[].tag_name"  | gsort --version-sort --reverse | ghead -n 1)" @TRET
                # @warn some repos have messed up version numbers where this won't work.
            fi
        fi
    fi
    ecgray "$0: selected tag: $(gq "$tag")"

    local assets
    assets="$(ec $releases | jq -r --arg tag "$tag" '.[] | if .tag_name == $tag then .assets else empty end' )" @TRET

    local my_asset
    my_asset="$(ec $assets | jq -r --arg desired "$desired" '.[] | select(.name | test($desired)) | .browser_download_url' | fz -1)" @TRET
    # my_asset="$(ec $assets | jq -r --arg desired "$desired" '.[] | select(.name | contains($desired)) | .browser_download_url' | fz -1)" @TRET

    local d="$HOME/.cache/dl/gh/$repo/"
    mkdir -p "$d"
    pushf "$d"
    {
        $proxyenv assert reval-ec aa-gateway "$my_asset" @RET
        unzip2dir_y=y assert unzip2dir ${~archiveglob} @RET

        bell-dl
    } always {
        # popf
    }
}
aliasfn gh-install gh-release-get
aliasfn github-install gh-release-get
##
function gh-pr-pip {
    gh_pr_pip.py "$@" |
        cat-copy-if-tty
}
##
function gh-to-raw-githack {
    in-or-args "$@" |
        perl -pe 's{github\.com}{raw.githack.com}g; s{/blob/}{/}g' |
        cat-copy-if-tty
}

function gh-to-raw-v2 {
    in-or-args "$@" |
        perl -pe 's{github\.com}{raw.githubusercontent.com}g; s{/blob/}{/}g' |
        cat-copy-if-tty
}

function gh-to-raw-v1 {
    in-or-args "$@" | sd '/blob/' '/raw/'
}
##
function gh-visibility {
    reval-ecgray gh repo view --json visibility
}
##
##
function gh-add-read-only-access-to {
    : "usage: gh-add-read-only-access-to GH-USER REPO...
Invites GH-USER as a read-only (pull) collaborator on each REPO, given as
OWNER/NAME or as a NAME under your own account. GH-USER has to accept each
invitation (https://github.com/notifications while logged in as GH-USER).
Audit the result with [agfi:gh-list-all-access].

Organization repositories only: a personal account's repository has a single
collaborator level, which can push, so GitHub refuses permission=pull there.
For read-only access to a personal repository, use a read-only deploy key."
    local user="${1-}"
    (( $# )) && shift
    if [[ -z "$user" ]] || (( $# == 0 )) ; then
        ecerr "usage: $0 GH-USER REPO..."
        return 2
    fi

    local me repo owner_type
    me="$(command gh api user -q .login)" @RET
    for repo in "$@" ; do
        [[ "$repo" == */* ]] || repo="${me}/${repo}"
        owner_type="$(command gh api "repos/${repo}" -q .owner.type)" @RET
        if [[ "$owner_type" != Organization ]] ; then
            ecerr "$0: ${repo} belongs to a personal account, whose collaborators can always push; use a read-only deploy key instead"
            return 1
        fi
        #: 201 with an invitation, or 204 when GH-USER already has access.
        reval-ec command gh api --silent -X PUT "repos/${repo}/collaborators/${user}" -f permission=pull @RET
    done
}

function gh-list-all-access {
    : "usage: gh-list-all-access
Lists everyone and everything besides you that can reach the repositories you
own: collaborators with their permission, pending invitations, and deploy keys.
Tab separated: repo, visibility, kind, who, access. For auditing what
[agfi:gh-add-read-only-access-to] and others granted."
    local me
    me="$(command gh api user -q .login)" @RET

    command gh api graphql --paginate -f query='
query($endCursor: String) {
  viewer {
    repositories(first: 50, after: $endCursor, ownerAffiliations: OWNER) {
      pageInfo { hasNextPage endCursor }
      nodes {
        nameWithOwner isPrivate
        collaborators(affiliation: DIRECT, first: 100) { edges { permission node { login } } }
        deployKeys(first: 100) { nodes { title readOnly } }
      }
    }
  }
}' | command jq -r --arg me "$me" '
  .data.viewer.repositories.nodes[]
  | (if .isPrivate then "private" else "public" end) as $vis
  | .nameWithOwner as $repo
  | ((.collaborators.edges // [])[] | select(.node.login != $me)
       | [$repo, $vis, "collaborator", .node.login, .permission]),
    ((.deployKeys.nodes // [])[]
       | [$repo, $vis, "deploy-key", .title, (if .readOnly then "READ" else "WRITE" end)])
  | @tsv' @RET

    #: Pending invitations are not in the GraphQL schema; one REST call per repo.
    command gh repo list "$me" --limit 10000 --json nameWithOwner,isPrivate \
        -q '.[] | "\(.nameWithOwner)\t\(if .isPrivate then "private" else "public" end)"' |
        command gxargs -P 8 -L 1 sh -c '
            command gh api "repos/$0/invitations" -q ".[] | [\"$0\", \"$1\", \"invited\", .invitee.login, .permissions] | @tsv"'
}
