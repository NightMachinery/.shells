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
##
function wget-dir {
    # @alt dl-dir
    ##
    # https://stackoverflow.com/questions/17282915/how-to-download-an-entire-directory-and-subdirectories-using-wget
    ##
    local depth="${wget_dir_d:-0}" url="${@[-1]}" opts=()

    local parent_count
    if parent_count="$(url_dir_count.pl "$url")" ; then
        opts+=( --cut-dirs $((parent_count - 1)) )
    fi

    wget -e robots=off -r --level="$depth" --no-host-directories --no-parent --reject="index.html*" --no-clobber "${opts[@]}" "$@"
    # --level (-l) determines the depth of the recursion (5 by default, use 0 or 'inf' for unlimited):
    #
    # + -l1 just download the directory (tzivi in your case)
    #
    # + -l2 download the directory and all level 1 subfolders ('tzivi/something' but not 'tivizi/somthing/foo')
    #
    # --no-host-directories (-nH) option with wget to prevent the hostname directory getting created by default with the download directory.
    #
    # --cut-dirs=X (cuts out X directories)(Big X acts effectively like --no-directories)
    #
    # --no-directories (-nd): do not create a hierarchy of directories when retrieving recursively. With this option turned on, all files will get saved to the current directory, without clobbering
    #
    # --no-clobber: When running Wget with -r or -p, but without -N, -nd, or -nc, re-downloading a file will result in the new copy simply overwriting the old.  Adding -nc will prevent this behavior, instead causing the original version to be preserved and any newer copies on the server to be ignored.
    #
    # --timestamping (-N): uses the timestamps of the file and the server to determine if it has changed. (Presumably.)
    # When running Wget with -N, with or without -r or -p, the decision as to whether or not to download a newer copy of a file depends on the local and remote timestamp and size of the file.  -nc may not be specified at the same time as -N.
    ##
}
##
### * End
psource ~/.privateShell
###
