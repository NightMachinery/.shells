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

function zle-expand() {
    zle expand-word
    # zle redisplay # @bug does not syntax-color
}
zle -N zle-expand
bindkey "^_" zle-expand # C-/
##
FZTAB_OPTS=(
    --color=light # needed, as fzf-tab overrides the colors, so we need to re-override them
    -i # case-insensitive (as opposed to smart-case)
    # --height='${FZF_TMUX_HEIGHT:=75%}'
    # --sync # did not solve the weird fzf bug
)
function fztab() {
    ##
    command fzf "$@" "$FZTAB_OPTS[@]"

    # fnswap isI true ugfz "$@" "$FZTAB_OPTS[@]" ""
    ##
    # Do NOT use --exit-0 or in macOS you will not be able to start typing before the fzf window opens
    # fzf_mru_context= fzf-gateway  "$@" "$FZTAB_OPTS[@]"

    # Check out /Users/evar/Library/Caches/antibody/https-COLON--SLASH--SLASH-github.com-SLASH-Aloxaf-SLASH-fzf-tab/lib/-ftb-fzf to see how this is run
}
# zstyle ':fzf-tab:*' print-query alt-enter # this is the default
zstyle ':fzf-tab:*' fzf-command fztab
# zstyle ':fzf-tab:*' fzf-command fzf
zstyle ':completion:*' format '[%d]' # enables groups
zstyle ':fzf-tab:*' switch-group ',' '.'
zstyle ':fzf-tab:*' prefix ''
zstyle ':fzf-tab:*' default-color $'\033[38;5;24m'
##
FZF_TAB_GROUP_COLORS=(
    $'\033[38;5;24m' $'\033[32m' $'\033[33m' $'\033[35m' $'\033[31m' $'\033[38;5;27m' $'\033[36m' \
        $'\033[38;5;100m' $'\033[38;5;98m' $'\033[91m' $'\033[38;5;80m' $'\033[92m' \
        $'\033[38;5;214m' $'\033[38;5;165m' $'\033[38;5;124m' $'\033[38;5;120m'
)
# FZF_TAB_GROUP_COLORS=()
# for i in {1..16} ; do
#     # FZF_TAB_GROUP_COLORS+="$(colorfg 10 255 10)"
#     FZF_TAB_GROUP_COLORS+=$'\033[38;5;19m'
# done
zstyle ':fzf-tab:*' group-colors $FZF_TAB_GROUP_COLORS
##
zinit light Aloxaf/fzf-tab # should come after all tab keybindings
##
zstyle -d ':completion:*' special-dirs # to not show ./ and ../
# https://unix.stackexchange.com/a/14231/282382
# to make tab work on empty line
zstyle ':completion:*' insert-tab false
##
# zstyle ':completion:*' matcher-list '+m:{a-zA-Z}={A-Za-z}' '+r:|[._-]=* r:|=*' 'r:|?=** m:{a-z\-}={A-Z\_}' '+l:|=* r:|=*'
zstyle ':completion:*' matcher-list 'r:|?=** m:{a-z\-}={A-Z\_}' '+l:|=* r:|=*'
