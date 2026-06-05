# -*- mode: sh; sh-shell: zsh; -*-
# Public, minimal NightMachinary Zsh basics.
# Loader only; implementation lives in plugin-core.zsh.

if [[ -n "${night_basic_plugin_loaded_p:-}" ]] ; then
    return 0
fi
typeset -g night_basic_plugin_loaded_p=y

local night_basic_plugin_dir
night_basic_plugin_dir="${${(%):-%x}:A:h}"

source "${night_basic_plugin_dir}/plugin-core.zsh" || return $?
