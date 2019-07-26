isI && {
	source ~/scripts/zsh/widgets.zsh
precmd_pipestatus() {
RPROMPT="${(j.|.)pipestatus}"
if [[ ${(j.|.)pipestatus} = 0 ]]; then
RPROMPT=""
fi
}
add-zsh-hook precmd precmd_pipestatus
}

