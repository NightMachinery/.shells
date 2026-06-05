# -*- mode: sh; sh-shell: zsh; -*-
# Full/opinionated NightMachinary basic stack.
# This file is intentionally only a loader; implementation remains modular.

if [[ -n "${night_basic_full_plugin_loaded_p:-}" ]] ; then
    return 0
fi
typeset -g night_basic_full_plugin_loaded_p=y
typeset -g night_basic_plugin_loaded_p=y

local night_basic_full_dir
night_basic_full_dir="${${(%):-%x}:A:h}"

function source-basic {
    local i
    for i in "$@" ; do
        source "${night_basic_full_dir}/${i}.zsh" || return $?
    done
}

source-basic basic || return $?
# malice is the alias module. :D
source-basic variables compatibility magicmacros deps cached conditions crossplatform args colors debug text-manipulation ssh malice history eval enhancers redirections functional macros redis || return $?

local i
for i in "${night_basic_full_dir}"/auto-load/**/*.zsh(.) ; do
    source "${i}" || return $?
done

source-basic proxy || return $?

@opts-setprefix assert ensure
