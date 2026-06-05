# -*- mode: sh; sh-shell: zsh; -*-
# Public, minimal NightMachinary Zsh basics.
# Loader only; implementation lives in modular .zsh files.

if [[ -n "${night_basic_plugin_loaded_p:-}" ]] ; then
    return 0
fi
typeset -g night_basic_plugin_loaded_p=y

local night_basic_plugin_dir
night_basic_plugin_dir="${${(%):-%x}:A:h}"

source "${night_basic_plugin_dir}/core.zsh" || return $?
source "${night_basic_plugin_dir}/return-aliases.zsh" || return $?
source "${night_basic_plugin_dir}/arrays.zsh" || return $?
source "${night_basic_plugin_dir}/variables.zsh" || return $?
source "${night_basic_plugin_dir}/compatibility.zsh" || return $?
source "${night_basic_plugin_dir}/eval.zsh" || return $?
source "${night_basic_plugin_dir}/files.zsh" || return $?
source "${night_basic_plugin_dir}/tmux.zsh" || return $?
source "${night_basic_plugin_dir}/assert.zsh" || return $?
