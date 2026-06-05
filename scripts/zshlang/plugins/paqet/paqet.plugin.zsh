# -*- mode: sh; sh-shell: zsh; -*-
# Paqet plugin loader. Implementation lives in paqet.zsh.

if [[ -n "${paqet_plugin_loaded_p:-}" ]] ; then
    return 0
fi
typeset -g paqet_plugin_loaded_p=y

local paqet_plugin_dir
paqet_plugin_dir="${${(%):-%x}:A:h}"

source "${paqet_plugin_dir}/../../basic/basic.plugin.zsh" || return $?
source "${paqet_plugin_dir}/paqet.zsh" || return $?
