##
bindkey -e #: emacs keybindings mode

bindkey '^[^M' self-insert-unmeta # You can use self-insert-unmeta to bind Alt+Return to insert a literal newline without accepting the command

#: self-insert-unmeta causes ={=, =}= to not be escaped when typed in the middle of, e.g., a URL. I don't understand why this happens. My guess is that one of my plugins is hooking into =self-insert= to insert the escape, but =self-insert-unmeta= is not hooked.
#: self-insert-unmeta: Insert a character into the buffer after stripping the meta bit and  converting ^M to ^J.
bindkey '\{' self-insert-unmeta
bindkey '\}' self-insert-unmeta

bindkey "^U" backward-kill-line
##
function zle-print-dots() {
    # toggle line-wrapping off and back on again
    [[ -n "$terminfo[rmam]" && -n "$terminfo[smam]" ]] && echoti rmam
    # print -Pn "%{%F{red}......%f%}"
    print -Pn "%{%F{green}...%f%}"
    [[ -n "$terminfo[rmam]" && -n "$terminfo[smam]" ]] && echoti smam
}
function zle-complete-with-dots() {
    zle-print-dots
    ##
    # This expands variables and globs if possible, else it auto-completes:
    # zle expand-or-complete
    ##
    # zle fzf-tab-complete # causes infinite loop
    ##
    zle complete-word
    ##
    zle redisplay
}
zle -N zle-complete-with-dots
bindkey "^I" zle-complete-with-dots # TAB

bindkey '^[/' expand-or-complete #: Alt+/ for normal Zsh completion (to bypass fzf tab completion)

function zle-expand {
    zle expand-word
    # zle redisplay # @bug does not syntax-color
}
zle -N zle-expand
bindkey "^_" zle-expand #: Ctrl-/ C-/
##
# start typing + [Up-Arrow] - fuzzy find history forward
if [[ "${terminfo[kcuu1]}" != "" ]]; then
    autoload -U up-line-or-beginning-search
    zle -N up-line-or-beginning-search
    bindkey "${terminfo[kcuu1]}" up-line-or-beginning-search
    bindkey "^[[A" up-line-or-beginning-search
fi
# start typing + [Down-Arrow] - fuzzy find history backward
if [[ "${terminfo[kcud1]}" != "" ]]; then
    autoload -U down-line-or-beginning-search
    zle -N down-line-or-beginning-search
    bindkey "${terminfo[kcud1]}" down-line-or-beginning-search
    bindkey "^[[B" down-line-or-beginning-search
fi

bindkey '^[[1;3D' backward-word # alt-left
bindkey '^[[1;3C' forward-word # alt-right
bindkey '^[/' forward-word # alt-/
##
# remove '/' from word chars. Affects C-w, alt-left, alt-right, etc
WORDCHARS+="|'"
WORDCHARS="${WORDCHARS/\//}"
WORDCHARS="${WORDCHARS/\[/}"
WORDCHARS="${WORDCHARS/\]/}"
WORDCHARS="${WORDCHARS/\#/}"
WORDCHARS="${WORDCHARS/\./}"
WORDCHARS+='ضصثقفغعهخحجچشسیبلاتنمکگظطزرذدپو۱۲۳۴۵۶۷۸۹۰'

function backward-kill-word2() {
if [[ "$LBUFFER" =~ '(.*)\s+$' ]] ; then
    LBUFFER="$match[1]"
else
    ## if you want to customize WORDCHARS per function:
    local WORDCHARS="${WORDCHARS}"
    zle backward-kill-word
fi
}
zle -N backward-kill-word2
bindkey '^W' backward-kill-word2 # C-w
bindkey '^[^?' backward-kill-word2 # alt-backspace alt-delete
##
eval "$(zoxide init zsh)"
##
### Added by Zinit's installer
if [[ ! -f $HOME/.local/share/zinit/zinit.git/zinit.zsh ]]; then
    print -P "%F{33} %F{220}Installing %F{33}ZDHARMA-CONTINUUM%F{220} Initiative Plugin Manager (%F{33}zdharma-continuum/zinit%F{220})…%f"
    command mkdir -p "$HOME/.local/share/zinit" && command chmod g-rwX "$HOME/.local/share/zinit"
    command git clone https://github.com/zdharma-continuum/zinit "$HOME/.local/share/zinit/zinit.git" && \
        print -P "%F{33} %F{34}Installation successful.%f%b" || \
        print -P "%F{160} The clone has failed.%f%b"
fi

source "$HOME/.local/share/zinit/zinit.git/zinit.zsh"
autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit
###
##
autoload -Uz compinit && compinit
zinit light Aloxaf/fzf-tab #: fzf-tab needs to be loaded after compinit, but before plugins which will wrap widgets, such as zsh-autosuggestions or fast-syntax-highlighting
enable-fzf-tab
# disable-fzf-tab

eval "$(starship init zsh)"
zinit light zsh-users/zsh-autosuggestions
zinit light zdharma-continuum/fast-syntax-highlighting
#: fast-syntax-highlighting should be last
##
unalias zi &> /dev/null || true
###
psource ~/.zshrc.private

cd ~/
#: @workaround [[id:3c5c3e16-9ae8-483b-82c8-bf1a333b058f][{bug}: loading plugins changes PWD · Issue #543 · zdharma-continuum/zinit]]
###
