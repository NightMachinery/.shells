# -*- mode: sh; sh-shell: zsh; -*-
# tmux-z plugin loader. Implementation lives in tmux-z.zsh.

if [[ -n "${tmux_z_plugin_loaded_p:-}" ]] ; then
    return 0
fi
typeset -g tmux_z_plugin_loaded_p=y

local tmux_z_plugin_dir
tmux_z_plugin_dir="${${(%):-%x}:A:h}"

source "${tmux_z_plugin_dir}/../../basic/basic.plugin.zsh" || return $?
source "${tmux_z_plugin_dir}/tmux-z.zsh" || return $?
