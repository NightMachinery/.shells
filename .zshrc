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
plugins=(git git-extras common-aliases lein pip sbt scala screen sprunge sudo vi-mode)
eval-darwinq 'plugins+=(osx)'
source $ZSH/oh-my-zsh.sh

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

psource /usr/local/opt/git-extras/share/git-extras/git-extras-completion.zsh 

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"

#This doesn't work. I have no idea why.
bindkey -M viins ‘ii’ vi-cmd-mode
bindkey -v

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
eval-darwin export TERM=xterm-24bits


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

#source ~/.iterm2_shell_integration.zsh
alias imgcat=~/.iterm2/imgcat;alias imgls=~/.iterm2/imgls;alias it2attention=~/.iterm2/it2attention;alias it2check=~/.iterm2/it2check;alias it2copy=~/.iterm2/it2copy;alias it2dl=~/.iterm2/it2dl;alias it2getvar=~/.iterm2/it2getvar;alias it2setcolor=~/.iterm2/it2setcolor;alias it2setkeylabel=~/.iterm2/it2setkeylabel;alias it2ul=~/.iterm2/it2ul;alias it2universion=~/.iterm2/it2universion
unsetopt correct_all
fpath=(~/.zsh.d/ $fpath)
#antibody use oh-my-zsh
POWERLEVEL9K_INSTALLATION_PATH=$ANTIGEN_BUNDLES/bhilburn/powerlevel9k
POWERLEVEL9K_COLOR_SCHEME='light'
# antibody theme bhilburn/powerlevel9k powerlevel9k
# antibody theme https://github.com/denysdovhan/spaceship-prompt spaceship
antibody bundle mafredri/zsh-async
antibody bundle intelfx/pure
#antibody bundle mafredri/zsh-async
#antibody bundle sindresorhus/pure
# antibody bundle arialdomartini/oh-my-git
# antibody theme arialdomartini/oh-my-git-themes oppa-lana-style
antibody bundle Tarrasch/zsh-bd
antibody bundle Tarrasch/zsh-colors
antibody bundle Vifon/deer
antibody bundle zsh-users/zsh-autosuggestions
silence unalias =
antibody bundle zsh-users/zsh-syntax-highlighting
antibody bundle zsh-users/zsh-completions
export _ZL_ADD_ONCE=1
export _ZL_MATCH_MODE=1
antibody bundle skywind3000/z.lua
source ~/bal/aliases.bash #To make them have priority.
autoload -U deer
zle -N deer
bindkey '\ek' deer
rcLoaded='yes'
source "$NIGHTDIR"/zsh/completions.zsh

# sth in .zshrc overrides these so ...
export FZF_BASE="${$(ge_no_ec=y rp fzf):h:h}/shell"
# ecdbg "FZF_BASE: $FZF_BASE"
test -d "$FZF_BASE" && {
    # ecdbg fzf loaded
    re source "$FZF_BASE"/*.zsh 
    export FZF_COMPLETION_TRIGGER=''
    bindkey '^T' fzf-completion
    bindkey '^I' expand-or-complete
    bindkey '^[[Z' fzf-completion #Shift+Tab
}
