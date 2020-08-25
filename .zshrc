# export TERM="xterm-256color" #Might do a lot of damage. Added for multi-term.
DEFAULT_USER="evar"
# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH=~/.oh-my-zsh

# Set name of the theme to load. Optionally, if you set this to "random"
# it'll load a random theme each time that oh-my-zsh is loaded.
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
# ZSH_THEME="robbyrussell"
# ZSH_THEME="oppa-lana-style"
# ZSH_THEME="powerlevel9k/powerlevel9k"

# Set list of themes to load
# Setting this variable when ZSH_THEME=random
# cause zsh load theme from this variable instead of
# looking in ~/.oh-my-zsh/themes/
# An empty array have no effect
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git git-extras lein pip sbt scala screen sprunge sudo vi-mode redis-cli)
isDarwin && plugins+=(osx)
# ecdbg $options[autopushd]
source $ZSH/oh-my-zsh.sh
# ecdbg $options[autopushd]

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/rsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"


# POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(status)

zsh-defer psource /usr/local/opt/git-extras/share/git-extras/git-extras-completion.zsh 

#This doesn't work. I have no idea why.
bindkey -M viins ‘ii’ vi-cmd-mode
bindkey -v

bindkey '^[^M' self-insert-unmeta # You can use self-insert-unmeta to bind Alt+Return to insert a literal newline without accepting the command

#Requires special .terminfo: l.a. https://emacs.stackexchange.com/questions/32506/conditional-true-color-24-bit-color-support-for-iterm2-and-terminal-app-in-osx
# Use colon separators#.
#xterm-24bit|xterm with 24-bit direct color mode,
#use=xterm-256color,
#setb24=\E[48:2:%p1%{65536}%/%d:%p1%{256}%/%{255}%&%d:%p1%{255}%&%dm,
#setf24=\E[38:2:%p1%{65536}%/%d:%p1%{256}%/%{255}%&%d:%p1%{255}%&%dm,
## Use semicolon separators.
#xterm-24bits|xterm with 24-bit direct color mode,
#use=xterm-256color,
#setb24=\E[48;2;%p1%{65536}%/%d;%p1%{256}%/%{255}%&%d;%p1%{255}%&%dm,
#setf24=\E[38;2;%p1%{65536}%/%d;%p1%{256}%/%{255}%&%d;%p1%{255}%&%dm,
#tic -x -o ~/.terminfo terminfo-24bit.src
isDarwin && export TERM=xterm-24bits || true


# start typing + [Up-Arrow] - fuzzy find history forward
if [[ "${terminfo[kcuu1]}" != "" ]]; then
    autoload -U up-line-or-beginning-search
    zle -N up-line-or-beginning-search
    bindkey "${terminfo[kcuu1]}" up-line-or-beginning-search
fi
# start typing + [Down-Arrow] - fuzzy find history backward
if [[ "${terminfo[kcud1]}" != "" ]]; then
    autoload -U down-line-or-beginning-search
    zle -N down-line-or-beginning-search
    bindkey "${terminfo[kcud1]}" down-line-or-beginning-search
fi

alias imgcat=~/.iterm2/imgcat;alias imgls=~/.iterm2/imgls;alias it2attention=~/.iterm2/it2attention;alias it2check=~/.iterm2/it2check;alias it2copy=~/.iterm2/it2copy;alias it2dl=~/.iterm2/it2dl;alias it2getvar=~/.iterm2/it2getvar;alias it2setcolor=~/.iterm2/it2setcolor;alias it2setkeylabel=~/.iterm2/it2setkeylabel;alias it2ul=~/.iterm2/it2ul;alias it2universion=~/.iterm2/it2universion
unsetopt correct_all
fpath=(~/.zsh.d/ $fpath)
antibody bundle mafredri/zsh-async
zsh-defer antibody bundle zdharma/zui
re source "$NIGHTDIR"/zsh/personal/aliases.zsh "$NIGHTDIR"/bash/auto-load/aliases.bash #To make them have priority. # Sth makes zsh reload all aliases, which breaks `ialiases`.
# autoload -U deer
# zle -N deer
# bindkey '\ek' deer
autoload -U +X bashcompinit && bashcompinit
_comp_options+=(globdots)
zsh-defer source-interactive-all

## fzf zsh integration (obsolete, as we now use fzf-tab
# # sth in .zshrc overrides these so ...
# export FZF_BASE="${$(ge_no_ec=y rp fzf):h:h}/shell"
# # ecdbg "FZF_BASE: $FZF_BASE"
# test -d "$FZF_BASE" && {
#     # ecdbg fzf loaded
#     re source "$FZF_BASE"/*.zsh
#     export FZF_COMPLETION_TRIGGER=''
#     bindkey '^T' fzf-completion
#     bindkey '^I' expand-or-complete
#     bindkey '^[[Z' fzf-completion #Shift+Tab
# }
##
fzf-history-widget() {
  # copied from fzf zsh integration
  local selected num
  setopt localoptions noglobsubst noposixbuiltins pipefail no_aliases 2> /dev/null
  selected=( $(fc -rl 1 | perl -ne 'print if !$seen{($_ =~ s/^\s*[0-9]+\s+//r)}++' |
    FZF_DEFAULT_OPTS="--height ${FZF_TMUX_HEIGHT:-40%} $FZF_DEFAULT_OPTS -n2..,.. --tiebreak=index --bind=ctrl-r:toggle-sort $FZF_CTRL_R_OPTS --query=${(qqq)LBUFFER} +m" fz) )
  local ret=$?
  if [ -n "$selected" ]; then
    num=$selected[1]
    if [ -n "$num" ]; then
      zle vi-fetch-history -n $num
    fi
  fi
  zle reset-prompt
  return $ret
}
zle     -N   fzf-history-widget
bindkey '^R' fzf-history-widget

zle -N fr_zle
bindkey '^[[Z' fr_zle # shift+tab
##

zsh-defer psource ~/Library/Preferences/org.dystroy.broot/launcher/bash/br
##
# antibody bundle intelfx/pure
antibody bundle sindresorhus/pure
function prompt_pure_check_cmd_exec_time () {
  # Obviously, we are abusing pure's private API here :D We can achieve the same using the preexec and precmd hooks, but reusing this seems cheaper
  integer elapsed
  (( elapsed = EPOCHSECONDS - ${prompt_pure_cmd_timestamp:-$EPOCHSECONDS} ))
  typeset -g prompt_pure_cmd_exec_time=
  (( elapsed > ${PURE_CMD_MAX_EXEC_TIME:-5} )) && {
    prompt_pure_human_time_to_var $elapsed "prompt_pure_cmd_exec_time"
  }

  (( elapsed > ${BELL_EXEC_TIME:-10} )) && {
    # @notcrossplatform
    isDarwin && iterm-session-is-active && silent awaysh bella-zsh
  }
}
##
# antibody bundle denysdovhan/spaceship-prompt
# antibody bundle Tarrasch/zsh-bd
# antibody bundle Tarrasch/zsh-colors
# antibody bundle Vifon/deer
zsh-defer antibody bundle unixorn/git-extra-commands
zsh-defer antibody bundle zdharma/zzcomplete # ^F
zsh-defer antibody bundle zsh-users/zsh-autosuggestions
silence unalias =
# antibody bundle zsh-users/zsh-syntax-highlighting
antibody bundle zsh-users/zsh-completions #undeferable
##
export YSU_MESSAGE_POSITION="after"
export YSU_MESSAGE_FORMAT="$(tput setaf 1)found %alias_type for %command: %alias$(tput sgr0)"
export YSU_MODE=ALL #BESTMATCH or ALL
export YSU_IGNORED_ALIASES=("g" "ll")
zsh-defer antibody bundle "MichaelAquilina/zsh-you-should-use"
##

##
# antibody bundle djui/alias-tips # incompatible with our own alias module
# export ZSH_PLUGINS_ALIAS_TIPS_EXCLUDES="_ ll vi"
# export ZSH_PLUGINS_ALIAS_TIPS_REVEAL_EXCLUDES=(_ ll vi)
# alias revealaliases='export ZSH_PLUGINS_ALIAS_TIPS_REVEAL=1'
##

##
expand-or-complete-with-dots() {
    # toggle line-wrapping off and back on again
    [[ -n "$terminfo[rmam]" && -n "$terminfo[smam]" ]] && echoti rmam
    print -Pn "%{%F{red}......%f%}"
    [[ -n "$terminfo[rmam]" && -n "$terminfo[smam]" ]] && echoti smam

    zle expand-or-complete
    zle redisplay
}
zle -N expand-or-complete-with-dots
bindkey "^I" expand-or-complete-with-dots
##
# antibody bundle Aloxaf/fzf-tab
FZF_TAB_OPTS=(
    --ansi   # Enable ANSI color support, necessary for showing groups
    --expect='$continuous_trigger,$print_query' # For continuous completion
    # '--color=hl:$(( $#headers == 0 ? 108 : 255 ))'
    --color=light
    --nth=2,3 --delimiter='\x00'  # Don't search prefix
    --layout=reverse --height='${FZF_TMUX_HEIGHT:=75%}'
    --tiebreak=begin -m --bind=tab:down,btab:up,change:top,ctrl-space:toggle --cycle
    '--query=$query'   # $query will be expanded to query string at runtime.
    '--header-lines=$#headers' # $#headers will be expanded to lines of headers at runtime
    --print-query
)
function fztab() {
    fz "$@"
}
FZF_TAB_COMMAND=( fztab "$FZF_TAB_OPTS[@]" )
# zstyle ':fzf-tab:*' print-query alt-enter # this is the default
zstyle ':fzf-tab:*' command $FZF_TAB_COMMAND
zstyle ':fzf-tab:*' no-group-color $'\033[38;5;24m'
# FZF_TAB_GROUP_COLORS=(
#     $'\033[94m' $'\033[32m' $'\033[33m' $'\033[35m' $'\033[31m' $'\033[38;5;27m' $'\033[36m' \
#         $'\033[38;5;100m' $'\033[38;5;98m' $'\033[91m' $'\033[38;5;80m' $'\033[92m' \
#         $'\033[38;5;214m' $'\033[38;5;165m' $'\033[38;5;124m' $'\033[38;5;120m'
# )
# FZF_TAB_GROUP_COLORS=()
# for i in {1..16} ; do
#     # FZF_TAB_GROUP_COLORS+="$(colorfg 10 255 10)"
#     FZF_TAB_GROUP_COLORS+=$'\033[38;5;19m'
# done
# zstyle ':fzf-tab:*' group-colors $FZF_TAB_GROUP_COLORS

antibody bundle Aloxaf/fzf-tab
fzf-tab-partial-and-complete() {
    if [[ $LASTWIDGET = 'fzf-tab-partial-and-complete' ]]; then
        fzf-tab-complete
    else
        zle complete-word
    fi
}

# zle -N fzf-tab-partial-and-complete
# bindkey '^I' fzf-tab-partial-and-complete
zstyle ':completion:*' matcher-list '+m:{a-zA-Z}={A-Za-z}' '+r:|[._-]=* r:|=*' '+l:|=* r:|=*'
# use `zstyle ':completion:*' matcher-list '' '+m:{a-zA-Z}={A-Za-z}' '+r:|[._-]=* r:|=*' '+l:|=* r:|=*'` if you don't want to see the case-insesitive results if there are case-sensitive results. In general, it seems that the matches stop at the first rule that produces at least one match.

# See http://zsh.sourceforge.net/Doc/Release/Completion-System.html
# These are general rules that make the completion system match better:
# The first rule adds case-insensitivity.
# The second (original) rule allows for partial completion before ., _ or -, e.g. f.b -> foo.bar.
# The third rule allows for completing on the left side of the written text, e.g. bar -> foobar)
##
# (( $+commands[cod] )) && source <(command cod init $$ zsh | sd '\bcod\b' 'command cod')
# https://github.com/dim-an/cod/issues/24
##
zsh-defer antibody bundle zdharma/fast-syntax-highlighting #should be last
zsh-defer antibody bundle zdharma/zbrowse # ^b # should be after fast-syntax, idk why but errors out otherwise

###
export rcLoaded='loading'
zsh-defer export rcLoaded='yes'
