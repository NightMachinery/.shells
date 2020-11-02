### Aliases
## OMZ
# Outputs the name of the current branch
# Usage example: git pull origin $(git_current_branch)
# Using '--quiet' with 'symbolic-ref' will not cause a fatal error (128) if
# it's not a symbolic ref, but in a Git repo.
function git_current_branch() {
  local ref
  ref=$(command git symbolic-ref --quiet HEAD 2> /dev/null)
  local ret=$?
  if [[ $ret != 0 ]]; then
    [[ $ret == 128 ]] && return  # no git repo.
    ref=$(command git rev-parse --short HEAD 2> /dev/null) || return
  fi
  echo ${ref#refs/heads/}
}
alias g='git'

alias ga='git add'
alias gaa='git add --all'
alias gapa='git add --patch'
alias gau='git add --update'
alias gap='git apply'

alias gb='git branch'
alias gba='git branch -a'
alias gbd='git branch -d'
alias gbda='git branch --no-color --merged | command grep -vE "^(\*|\s*(master|develop|dev)\s*$)" | command xargs -n 1 git branch -d'
alias gbl='git blame -b -w'
alias gbnm='git branch --no-merged'
alias gbr='git branch --remote'
alias gbs='git bisect'
alias gbsb='git bisect bad'
alias gbsg='git bisect good'
alias gbsr='git bisect reset'
alias gbss='git bisect start'

alias gc='git commit -v'
alias gc!='git commit -v --amend'
alias gcn!='git commit -v --no-edit --amend'
alias gca='git commit -v -a'
alias gca!='git commit -v -a --amend'
alias gcan!='git commit -v -a --no-edit --amend'
alias gcans!='git commit -v -a -s --no-edit --amend'
alias gcam='git commit -a -m'
alias gcsm='git commit -s -m'
alias gcb='git checkout -b'
alias gcf='git config --list'
alias gcl='git clone --recursive'
alias gclean='git clean -fd'
alias gpristine='git reset --hard && git clean -dfx'
alias gcm='git checkout master'
alias gcd='git checkout develop'
alias gcmsg='git commit -m'
alias gco='git checkout'
alias gcount='git shortlog -sn'
alias gcp='git cherry-pick'
alias gcpa='git cherry-pick --abort'
alias gcpc='git cherry-pick --continue'
alias gcs='git commit -S'

alias gd='git diff'
alias gdca='git diff --cached'
alias gdcw='git diff --cached --word-diff'
alias gdct='git describe --tags `git rev-list --tags --max-count=1`'
alias gdt='git diff-tree --no-commit-id --name-only -r'
alias gdw='git diff --word-diff'

gdv() { git diff -w "$@" | view - }
alias gf='git fetch'
alias gfa='git fetch --all --prune'
alias gfo='git fetch origin'

function gfg() { git ls-files | grep $@ }
alias gg='git gui citool'
alias gga='git gui citool --amend'

ggf() {
  [[ "$#" != 1 ]] && local b="$(git_current_branch)"
  git push --force origin "${b:=$1}"
}
ggfl() {
[[ "$#" != 1 ]] && local b="$(git_current_branch)"
git push --force-with-lease origin "${b:=$1}"
}
ggl() {
  if [[ "$#" != 0 ]] && [[ "$#" != 1 ]]; then
    git pull origin "${*}"
  else
    [[ "$#" == 0 ]] && local b="$(git_current_branch)"
    git pull origin "${b:=$1}"
  fi
}
ggp() {
  if [[ "$#" != 0 ]] && [[ "$#" != 1 ]]; then
    git push origin "${*}"
  else
    [[ "$#" == 0 ]] && local b="$(git_current_branch)"
    git push origin "${b:=$1}"
  fi
}
ggpnp() {
  if [[ "$#" == 0 ]]; then
    ggl && ggp
  else
    ggl "${*}" && ggp "${*}"
  fi
}
ggu() {
  [[ "$#" != 1 ]] && local b="$(git_current_branch)"
  git pull --rebase origin "${b:=$1}"
}
alias ggpur='ggu'
alias ggpull='git pull origin $(git_current_branch)'
alias ggpush='git push origin $(git_current_branch)'
alias ggsup='git branch --set-upstream-to=origin/$(git_current_branch)'
alias gpsup='git push --set-upstream origin $(git_current_branch)'

alias gignore='git update-index --assume-unchanged'
alias gignored='git ls-files -v | grep "^[[:lower:]]"'
alias git-svn-dcommit-push='git svn dcommit && git push github master:svntrunk'

alias gl='git pull'
alias glg='git log --stat'
alias glgp='git log --stat -p'
alias glgg='git log --graph'
alias glgga='git log --graph --decorate --all'
alias glgm='git log --graph --max-count=10'
alias glo='git log --oneline --decorate'
alias glol="git log --graph --pretty='%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit"
alias glola="git log --graph --pretty='%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit --all"
alias glog='git log --oneline --decorate --graph'
alias gloga='git log --oneline --decorate --graph --all'
alias glp="_git_log_prettily"
alias gm='git merge'
alias gmom='git merge origin/master'
alias gmt='git mergetool --no-prompt'
alias gmtvim='git mergetool --no-prompt --tool=vimdiff'
alias gmum='git merge upstream/master'
alias gma='git merge --abort'

alias gp='git push'
alias gpd='git push --dry-run'
alias gpoat='git push origin --all && git push origin --tags'
alias gpu='git push upstream'
alias gpv='git push -v'

alias gr='git remote'
alias gra='git remote add'
alias grb='git rebase'
alias grba='git rebase --abort'
alias grbc='git rebase --continue'
alias grbi='git rebase -i'
alias grbm='git rebase master'
alias grbs='git rebase --skip'
alias grh='git reset HEAD'
alias grhh='git reset HEAD --hard'
alias grmv='git remote rename'
alias grrm='git remote remove'
alias grset='git remote set-url'
alias grt='cd $(git rev-parse --show-toplevel || echo ".")'
alias gru='git reset --'
alias grup='git remote update'
alias grv='git remote -v'

alias gsb='git status -sb'
alias gsd='git svn dcommit'
alias gsi='git submodule init'
alias gsps='git show --pretty=short --show-signature'
alias gsr='git svn rebase'
alias gss='git status -s'
alias gst='git status'
alias gsta='git stash save'
alias gstaa='git stash apply'
alias gstc='git stash clear'
alias gstd='git stash drop'
alias gstl='git stash list'
alias gstp='git stash pop'
alias gsts='git stash show --text'
alias gsu='git submodule update'

alias gts='git tag -s'
alias gtv='git tag | sort -V'

alias gunignore='git update-index --no-assume-unchanged'
alias gunwip='git log -n 1 | grep -q -c "\-\-wip\-\-" && git reset HEAD~1'
alias gup='git pull --rebase'
alias gupv='git pull --rebase -v'
alias glum='git pull upstream master'

alias gwch='git whatchanged -p --abbrev-commit --pretty=medium'
alias gwip='git add -A; git rm $(git ls-files --deleted) 2> /dev/null; git commit --no-verify -m "--wip-- [skip ci]"'
##
alias gdc='git diff --name-only --diff-filter=U' # List conflicted files in git
alias grm='git rm --cached'
alias glcs='glc --depth=1'
##
# https://sw.kovidgoyal.net/kitty/kittens/diff.html
# Needs some config in git
aliasfn git-diff-kitty git difftool --tool='kitty'
# --dir-diff : concats all the files into a single diff.
function git-diff() {
  if (( $#@ == 0 )) ; then
set -- 'HEAD~1'
  elif [[ "$1" =~ '^\d+$' ]] ; then
set -- "HEAD~$1"
  fi

  if isKitty ; then
    git-diff-kitty "$@"
  else
    git diff "$@"
  fi
}
aliasfn gd git-diff
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
