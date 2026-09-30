#!/usr/bin/env -S zsh -f
# -*- mode: sh; sh-shell: zsh; -*-
#: Usage: fim-get.zsh <provider> <prefix> [<suffix>]
#:
#: [agfi:fim-get] for hammerspoon/core/fim.lua, without BrishGarden. The same
#: function as everywhere else: this sources zshlang/auto-load/others/fim.zsh,
#: its one implementation, over the minimal basic stack, so FIM keeps working
#: while BrishGarden is down. Nothing of fim-get is copied here.
#:
#: The keys come from ~/.privateShell, sourced the way brishzq.zsh sources it,
#: so no key is ever an argument. The other files are what fim-get needs
#: beyond the public basic plugin: proxy.zsh for `should-proxy-p', which it
#: asks before every request (and which needs `isZii' from
#: conditions-personal.zsh), and colors.zsh for `ecerr', whose errors
#: fim.lua shows in its band and which otherwise prints "command not found:
#: colorfg" first. About 20 ms in all.
##
local root="${${(%):-%x}:A:h:h:h}"

source "${root}/zshlang/basic/basic.plugin.zsh" || exit 70
source "${root}/zshlang/basic/colors.zsh" || exit 70
source "${root}/zshlang/basic/conditions-personal.zsh" || exit 70
source "${root}/zshlang/basic/proxy.zsh" || exit 70
if [[ -r ~/.privateShell ]] ; then
    source ~/.privateShell || exit 70
fi
source "${root}/zshlang/auto-load/others/fim.zsh" || exit 70

fim_provider="${1}" fim-get "${@[2,-1]}"
