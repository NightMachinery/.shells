## * Global Aliases
alias -g ...='../..'
alias -g ....='../../..'
alias -g .....='../../../..'
alias -g ......='../../../../..'

alias -g '@RET'=' || { retcode=$? ; print -r -- "exited ${retcode}" ; return $retcode } '
##
ec() {
    print -r -- "$@"
}

gquote () {
    ec "${(q+@)@[1]}" "${(qq@)@[2,-1]}"
}

function export-from-alias {
  local name="${1:?}"

  local code="${aliases[$name]}"
  if test -n "$code" ; then
    eval export "${code}"
  fi
  ##
  # This is safe:
  #
  # ❯ alias hi='tmp="a ;b; c"'
  # ❯ ec ${aliases[hi]}
  # tmp="a ;b; c"
  ##
}

source ~/.shared.sh
psource ~/base/bootstrap/lib.zsh
###
export HISTFILE="${HOME}/.zsh_history"
export HISTSIZE=1000000
export SAVEHIST=1000000
###
setopt interactivecomments
setopt NO_CASE_GLOB
setopt autocd multios re_match_pcre extendedglob pipefail interactivecomments hash_executables_only # hash_executables_only will not hash dirs instead of executables, but it can be slow.
setopt long_list_jobs complete_in_word always_to_end
setopt append_history extended_history hist_expire_dups_first hist_ignore_dups hist_ignore_space hist_verify inc_append_history share_history
setopt TYPESET_SILENT # Without this, the local/typeset commands display the value of any variable which is already defined.
unsetopt autopushd
unsetopt AUTO_NAME_DIRS
unsetopt BG_NICE # Run all background jobs at a lower priority.
# having this enabled will cause some failures in BTT-issued background brishz commands
###
function cron-commands-reboot-get {
    crontab -l |
        perl -nle 'print $1 if /^\s*\@reboot\s+(.+)/'
}

function cron-commands-reboot-run {
    local cmds
    cmds=(${(@f)"$(cron-commands-reboot-get)"})

    for cmd in $cmds[@] ; do
        echo "$cmd"
        eval "$cmd"
    done
}
##
alias rh=rehash
### * End
psource ~/.privateShell
###
