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
alias 'gc!'='git commit -v --amend'
alias 'gcn!'='git commit -v --no-edit --amend'
alias gca='git commit -v -a'
alias 'gca!'='git commit -v -a --amend'
alias 'gcan!'='git commit -v -a --no-edit --amend'
#: - `--no-edit`: This flag is used in conjunction with the `--amend` option. It tells Git to use the commit message from the previous commit without launching the commit message editor. Typically when you amend a commit, Git opens the editor so you can modify the existing commit message, but `--no-edit` bypasses this, keeping the original message intact.

# alias 'gcans!'='git commit -v -a -s --no-edit --amend'
alias gcam='git commit -a -m'
# alias gcsm='git commit -s -m'

function h-gcm {
  git commit -m "$*"
}
alias gcm='\noglob h-gcm'
# alias gcm='git commit -m'
# alias gcmsg='git commit -m'

alias gcb='git checkout -b'
alias gcf='git config --list'
alias gcl='git clone --recursive'
aliasfn git-clone-shallow git clone --recursive --depth=1 --shallow-submodules
alias gcls='git-clone-shallow'
# alias gclean='git clean -fd'
alias gco='git checkout'
# alias gcm='git checkout master'
# alias gcd='git checkout develop'
alias gcount='git shortlog -sn'
##
# alias gcp='git cherry-pick'
alias gcpa='git cherry-pick --abort'
alias gcpc='git cherry-pick --continue'
##
alias gcs='git commit -S'

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
function glola() {
  LESS=$LESSMIN git log --graph --pretty=format:'%Cred%h%Creset %C(yellow)%ad%Creset %Cgreen(%cr)%Creset %s %C(yellow)%d%Creset %C(bold blue)<%an>%Creset' --date=short --abbrev-commit --all
}
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
##
alias grt='cd "$(git-root || echo ".")"'
function git-root {
   git rev-parse --show-toplevel
}
aliasfn gitroot git-root
##
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

function h-git-status-remove-unstaged {
  perl -ne 'm/\s*\QChanges not staged for commit:\E/ && ($skip = 1) ; m/\s*\QChanges to be committed:\E/ && ($skip = 0) ; $skip || print'
}

function git-status-summary2 {
  # @alt gss [-uno]
  local args=("$@") tail_mode="${gss_tail}"

  {
    git -c color.status=false submodule foreach git -c color.status=false status "$args[@]" | prefixer -a 'Submodules: '
    git -c color.status=false status "$args[@]"
  } | h-git-status-remove-unstaged | {
    command rg --color never -e 'deleted:' -e 'modified:' -e 'new file:'| trimsed | {
      if test -n "$tail_mode" ; then
        h_gss_tail
      else
        cat
      fi
    }
    true
  }
}
@opts-setprefix git-status-summary2 gss
aliasfn gss2 git-status-summary2
@opts-setprefixas gss2 git-status-summary2
function h_gss_tail() {
  # @todo1 @design we can also accumulate the changes into single directives: 'modified: x\nmodified: y' -> 'modified: x, y' (quote x and y using gq)
  local line
  for line in "${(@f)$(cat)}" ; do
    if [[ "$line" =~ '^\s*(\S*)\s*(.*)$' ]] ; then
      ec "$match[1] $(basename "$match[2]")"
    fi
  done
}

function git-commitmsg {
  ## alts
  # git diff --cached --diff-filter='M' --name-only # gives names of modified files
  ##
  local default="${git_commitmsg_default:-autocommit}" ask_edit="${git_commitmsg_ask:-y}"

  local msg="$(@opts tail y @ git-status-summary2 -uno)"

  # ec-tty $msg
  msg="$(ecn $msg | prefixer --skip-empty -o '; ')"
  if [[ "$msg" =~ '^\s*$' ]] ; then
    ec "$default"
  else
    if (( ${#msg} > 100 )) ; then
      if isIReally && bool "$ask_edit" && ask "$0: msg is too long. Do you wish to edit it?" y ; then
        msg="$(ec "$msg" | prefixer -o '; ')"
        ## @zshbug: vared doesn't work in subshells under an interactive shell
        # zmodload zsh/zle
        # vared -p "msg (use =ESC d d= to clear): " msg @RET
        ##
        msg="$(vared-py "msg: " "$msg")" @RET
        ec "$msg"
      else
        ec "${msg[1,300]} ..."
      fi
    else
      ec "$msg"
    fi
  fi
}

function git-commit-with-auto-msg {
  local msg
  msg="${$(git-commitmsg)}" @RET

  reval-ec git commit -m "${msg}"
}
alias gcauto='git-commit-with-auto-msg'
##
function git-branch-name() {
  # @see `git symbolic-ref HEAD`
  git branch --show-current
  ##
  # branch="$(git rev-parse --abbrev-ref HEAD)"
  # ec "$branch" | gtr -d '\n ' | tr -d ' '
  ##
}
##
function gsync {
  local msg="${*}"
  local noadd="${gsync_noadd}"
  local pull_only="${gsync_p}"
  local branch="${gsync_branch:-${gsync_b}}"
  local remote="${gsync_remote:-${gsync_r}}"
  ensure-array gsync_commit_pre_opts
  local commit_pre_opts=("${gsync_commit_pre_opts[@]}")

  pushf "$(git-root)" || return 1
  {
    if test -z "$branch" ; then
      ##
      # all doesn't work for pulling
      # branch='--all'
      # To push all your tags:
      # `git push REMOTE --tags`
      # Finally, I think you can do this all in one command with:
      # `git push REMOTE --mirror`
      # However, in addition --mirror, will also push your remotes, so this might not be exactly what you want.
      ##
        branch="$(git-branch-name)" || branch=master
      ##
    fi

    if ! bool "$noadd" ; then
      git submodule foreach git add --all @TRET
      git add --all @TRET
    fi

    git submodule update --recursive @TRET # idk what this does really. Fetching? Don't use `--remote` here.

    git-status-summary -uno

    if ! bool $pull_only ; then
      msg="${msg:-$(git-commitmsg)}"
      git submodule foreach git "${commit_pre_opts[@]}" commit -uno -m "${msg}" @STRUE
      ec "Main repo"
      git "${commit_pre_opts[@]}" commit -uno -m "${msg}" @STRUE
    fi

    local remotes
    if test -z "$remote" ; then
      local excludedRemotes=(upstream)
      remotes=("${(@)${(@f)$(git remote | command rg -v -e '^_')}:|excludedRemotes}") #: also excludes remotes starting with '_'
    else
      remotes=("$remote")
    fi

    for remote in $remotes[@]
    do
      ec
      if gr-isLocal "$remote" || isNet ; then
        # @todo @maybe get the remotes from the submodule itself. The submodules' remotes don't necessarily match the main repo's, such as when the submodule only has an 'upstream' remote.
        reval-ec git "${commit_pre_opts[@]}" submodule foreach git pull "$remote" "$branch" --no-edit --rebase=false @STRUE
        reval-ec git "${commit_pre_opts[@]}" pull "$remote" "$branch" --no-edit --rebase=false @TRET
      else
        ecerr "$0: Remote '$remote' is not local and there is no internet access. Skipping it."
      fi
      ec
    done

    if ! bool $pull_only ; then
      for remote in $remotes[@]
      do
        ec
        if gr-isLocal "$remote" || isNet ; then
          reval-ec git submodule foreach git push "$remote" "$branch" @STRUE
          reval-ec git push "$remote" "$branch" @STRUE
        else
          ecerr "$0: Remote '$remote' is not local and there is no internet access. Skipping it."
        fi
        ec
      done
    fi
  } always { popf }
}
##
function gr-isLocal() {
  local remote="${1}"
  ensure-args remote @MRET

  local url urls=()
  urls=(${(@f)"$(git remote get-url "$remote")"}) || return $?
  for url in ${urls[@]} ; do
    if [[ "${url:l}" == *(127.0.0.1|localhost)* ]] ; then
      return 0
    fi
  done
  return 1
}

function ghttp() {
  git remote -v |awk '{print $2}'|inargsf git2http| gsort -u > >(pbcopy) | cat
}
##
function git-undo-commits() {
  mdoc "$0 [<how-many>=1]
Undoes last commits without changing files." MAGIC

  git reset --soft HEAD~"${1:-1}"
}
##
function git-http-to-ssh {
  in-or-args "$@" |
    perl -ple 's|^https://([^/]+)/|git\@$1:| && s|(?:\.git)?$|.git|' |
    cat-copy-if-tty
}
aliasfn http2git git-http-to-ssh

function git-remote-2ssh {
  local remotes=($@)

  if (( ${#@} == 0 )) ; then
    remotes=(${(@f)"$(git remote)"})
  fi

  local remote url url_ssh
  for remote in ${remotes[@]} ; do
    url="$(git remote get-url "$remote")" @TRET
    dact var-show url
    url_ssh="$(http2git "$url")" @TRET
    reval-ec git remote set-url "$remote" "$url_ssh" @TRET
  done
}

function h_git2http {
  #: @deprecated
  ##
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
alias git2http-v0='\noglob reval inargsEf "re h_git2http"'

function git2http {
  in-or-args "$@" |
    perl -ple 's|^git@([^:]+):|https://$1/|g' |
    cat-copy-if-tty
}
##
function git-resolve {
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

function git-listblobs() {
  mdoc List blobs sorted by size. MAGIC
  git rev-list --objects --all \
    | git cat-file --batch-check='%(objecttype) %(objectname) %(objectsize) %(rest)' \
    | sed -n 's/^blob //p' \
    | sort --numeric-sort --key=2 \
    | cut -c 1-12,41- \
    | $(command -v gnumfmt || echo numfmt) --field=2 --to=iec-i --suffix=B --padding=7 --round=nearest
}

function git-clone-nonempty-dir {
  local repo_url="$1"
  local dir_path="${2:-.}"
  local branch="${git_branch:-master}"
  assert-args repo_url dir_path

  if [ ! -d "$dir_path" ]; then
    echo "Directory $(gquote-dq $dir_path) does not exist."
    return 1
  fi

  pushf "$dir_path" @RET
  {
    reval-ec git init
    reval-ec git remote add origin "$repo_url"
    reval-ec git fetch
    reval-ec git reset origin/"${branch}"
    reval-ec git branch --set-upstream-to=origin/"${branch}"

    ecbold "Restoring deleted files ..."
    git ls-files -z --deleted | xargs -0 git checkout --
  } always { popf }
}

function git_sparse_clone {
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
}

function github-dir {
  : "@? downloads the given URL of a Github directory to the given destination."

  svn export "$(sed 's/tree\/master/trunk/' <<< "$1")" "$2"
}
##
function git-pristine {
  : 'Single repo: git reset --hard && git clean -dfx'

  git clean -dfx
  #  -x Don't use the standard ignore rules (see gitignore(5)), but still use the ignore rules given with -e options from the command line. This allows removing all untracked files, including build products. This can be used (possibly in conjunction with git restore or git reset) to create a pristine working directory to test a clean build.

  git submodule foreach --recursive git clean -dfx
  git reset --hard
  git submodule foreach --recursive git reset --hard
  git submodule update --init --recursive
}
##
function gp-incremental {
  local i
  for i in $@ ; do
    reval-ec ga "$i"
    git_commitmsg_ask=n gsync_noadd=y gsync "$0: $i"
  done
}
##
function git-conflicts {
  git ls-files --unmerged
  #: --unmerged: show unmerged files in the output (forces --stage)
}

function git-conflicts-paths {
  ##
  # git ls-files --unmerged | awk '{print $4}' | sort -u
  ##
  git diff --name-only --diff-filter=U --relative |
    inargsf grealpath --
  ##
}
##
function git-current-commit {
  git rev-parse "${1:-HEAD}" |
    cat-copy-if-tty
}
##
function git-merge-p {
  #: [[https://stackoverflow.com/questions/29101270/how-to-know-if-a-merge-is-ongoing-in-git][How to know if a merge is ongoing in git - Stack Overflow]]
  ##
  test -e "$(git rev-parse --git-dir)"/MERGE_HEAD
}
##
function git-link {
  local files=($@)
  if (( $#@ == 0 )) ; then
    files+='.'
  fi

  local file repo_root remote_url local github_url
  for file in $files[@] ; do
    repo_root="$(git-root)" @TRET

    file="$(grealpath --relative-to="${repo_root}" "$file")" @TRET
    remote_url="$(git config --get remote.origin.url)" @TRET
    branch="$(git rev-parse --abbrev-ref HEAD)" @TRET
    github_url="https://github.com"

    # Remove .git from the end of the remote URL
    remote_url="${remote_url%.git}"

    ec "${github_url}/${remote_url#*:}/blob/${branch}/${file}"
  done | cat-copy-if-tty
}
##
