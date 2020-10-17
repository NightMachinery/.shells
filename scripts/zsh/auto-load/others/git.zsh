### Aliases
alias gdc='git diff --name-only --diff-filter=U' # List conflicted files in git
alias grm='git rm --cached'
alias glcs='glc --depth=1'
###
function git-status-summary() {
  local args=("$@")

  local opts=()
  if isI && istty ; then
    opts+=(-c color.status=always -c color.ui=always)
  fi
  local out="$(git "$opts[@]" submodule foreach git status -s "$args[@]")"
  out+="$(prefix-if-ne $'\n\nMain repo\n' "$(git "$opts[@]" status -s "$args[@]")" "$out")"
  ec "$out"
}
aliasfn gss git-status-summary
function git-status-summary2() {
  # @alt gss [-uno]
  local args=("$@")

  {
    git -c color.status=false submodule foreach git -c color.status=false status "$args[@]"| prefixer -a 'Submodules: '
    git -c color.status=false status "$args[@]"
  } | {
    command rg --color never -e 'deleted:' -e 'modified:' -e 'new file:'| trimsed
    true
  }
}
aliasfn gss2 git-status-summary2
function git-commitmsg() {
  ## alts
  # git diff --cached --diff-filter='M' --name-only # gives names of modified files
  ##
  local msg="$(git-status-summary2 -uno)"

  # ec-tty $msg
  msg="$(ecn $msg | prefixer --skip-empty -o '; ')"
  ec "${msg:-.}"
}
##
function gsync() {
  local msg="${*}"
  local noadd="${gsync_noadd}"
  local branch="${gsync_branch:-${gsync_b:-master}}"
  local remote="${gsync_remote:-${gsync_r}}"

  pushf "$(git rev-parse --show-toplevel)" || return 1
  {
    test -z "$noadd" && {
      git submodule foreach git add --all
      git add --all
    }
    git submodule update --recursive # idk what this does really. Fetching? Don't use `--remote` here.
    
    msg="${msg:-$(git-commitmsg)}"

    git-status-summary -uno

    git submodule foreach git commit -uno -a -m "${msg}"
    ec "Main repo"
    git commit -uno -a -m "${msg}"

    local remotes
    if test -z "$remote" ; then
      local excludedRemotes=(upstream)
      remotes=("${(@)${(@f)$(git remote)}:|excludedRemotes}")
    else
      remotes=("$remote")
    fi
    for remote in $remotes[@]
    do
      ec
      reval-ec git submodule foreach git pull "$remote" "$branch" --no-edit
      reval-ec git pull "$remote" "$branch" --no-edit
      ec
    done
    for remote in $remotes[@]
    do
      ec
      reval-ec git submodule foreach git push "$remote" "$branch"
      reval-ec git push "$remote" "$branch"
      ec
    done
  } always { popf }
}
ghttp() { git remote -v |awk '{print $2}'|inargsf git2http| gsort -u > >(pbcopy) | cat }
guc() {
  mdoc "$0 [<how-many>=1]
Undoes last commits without changing files." MAGIC

  git reset --soft HEAD~"${1:-1}"
}
_git2http() {
  mdoc "Usage: git2http <git-ssh-url> ...
cat <file-with-ssh-urls> | git2http

 -> <git-https-url> ...
This function is idempotent (it passes http URLs through)." MAGIC

  if [[ "$*" =~ '^http' ]]
then
  ec "$*"
  else
    <<<"$*" gsed -e 's|:|/|' -e 's|git@|https://|'
  fi
}
alias git2http='noglob inargsEf "re _git2http"'
git-resolve() {
  local git=("${=gitbinary:-git}")
  STRATEGY="$1"
  FILE_PATH="$2"

  if [ -z "$FILE_PATH" ] || [ -z "$STRATEGY" ]; then
echo "Usage:   <strategy> <file>"
echo ""
echo "Example: --ours package.json"
echo "Example: --union package.json"
echo "Example: --theirs package.json"
return
  fi

  if [ ! -e "$FILE_PATH" ]; then
echo "$FILE_PATH does not exist; aborting."
return
  fi

  # remove leading ./ if present, to match the output of $git[@] diff --name-only
  # (otherwise if user input is './filename.txt' we would not match 'filename.txt')
  FILE_PATH_FOR_GREP=${FILE_PATH#./}
  # grep -Fxq: match string (F), exact (x), quiet (exit with code 0/1) (q)
  if ! $git[@] diff --name-only --diff-filter=U | grep -Fxq "$FILE_PATH_FOR_GREP"; then
echo "$FILE_PATH is not in conflicted state; aborting."
return
  fi

  $git[@] show :1:"$FILE_PATH" > ./tmp.common
  $git[@] show :2:"$FILE_PATH" > ./tmp.ours
  $git[@] show :3:"$FILE_PATH" > ./tmp.theirs

  $git[@] merge-file "$STRATEGY" -p ./tmp.ours ./tmp.common ./tmp.theirs > "$FILE_PATH"
  $git[@] add "$FILE_PATH"

  rm ./tmp.common
  rm ./tmp.ours
  rm ./tmp.theirs
}
git-listblobs() {
  mdoc List blobs sorted by size. MAGIC
  git rev-list --objects --all \
    | git cat-file --batch-check='%(objecttype) %(objectname) %(objectsize) %(rest)' \
    | sed -n 's/^blob //p' \
    | sort --numeric-sort --key=2 \
    | cut -c 1-12,41- \
    | $(command -v gnumfmt || echo numfmt) --field=2 --to=iec-i --suffix=B --padding=7 --round=nearest
}
function git_sparse_clone() (
  # git_sparse_clone "http://github.com/tj/n" "./local/location" "/bin"
  rurl="$1" localdir="$2" && shift 2

  mkdir -p "$localdir"
  cd "$localdir"

  git init
  git remote add -f origin "$rurl"

  git config core.sparseCheckout true

  # Loops over remaining args
  local i
  for i; do
    echo "$i" >> .git/info/sparse-checkout
  done

  git pull origin master
)
function github-dir() {
  svn export "$(sed 's/tree\/master/trunk/' <<< "$1")" "$2"
}
