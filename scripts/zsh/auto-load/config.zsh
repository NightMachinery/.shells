re "silence unalias" map p fd
typeset -U path #Makes the path array's elements unique. It needs to be run again to fix a bad PATH, or some other array operation needs to be performed.
precmd_pipestatus() {
	RPROMPT="${(j.|.)pipestatus}"
       if [[ ${(j.|.)pipestatus} = 0 ]]; then
              RPROMPT=""
       fi
}
add-zsh-hook precmd precmd_pipestatus
antibody bundle arzzen/calc.plugin.zsh #adds calc and =
