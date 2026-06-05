# -*- mode: sh; sh-shell: zsh; -*-
# Psiphon plugin loader. Implementation lives in psiphon.zsh.

if [[ -n "${psiphon_plugin_loaded_p:-}" ]] ; then
    return 0
fi
typeset -g psiphon_plugin_loaded_p=y

local psiphon_plugin_dir
psiphon_plugin_dir="${${(%):-%x}:A:h}"

source "${psiphon_plugin_dir}/../../basic/basic.plugin.zsh" || return $?
source "${psiphon_plugin_dir}/psiphon.zsh" || return $?
