#!/usr/bin/env -S zsh -f
#: Detached agent completion worker. Request text arrives only on stdin.
local root="${${(%):-%x}:A:h:h}"
path=("${HOME}/go/bin" "${HOME}/bin" /opt/homebrew/bin "${path[@]}")
local binary="$HOME/go/bin/llm_complete" stale=n file
if [[ -x "$binary" ]] ; then
    for file in "${root}/golang/llm_complete/"**/*(.N) ; do
        if [[ "$file" == *.go || "$file" == */go.mod || "$file" == */go.sum ]] && [[ "$file" -nt "$binary" ]] ; then
            stale=y
            break
        fi
    done
    if [[ "$stale" == n ]] ; then exec "$binary" terminal "$@" ; fi
fi
source "${root}/zshlang/basic/basic.plugin.zsh" || exit 70
source "${root}/zshlang/basic/colors.zsh" || exit 70
source "${root}/zshlang/basic/conditions-personal.zsh" || exit 70
#: kitty may have only the system PATH. The normal Go install destination is
#: added explicitly; [agfi:go-local-dep] still owns building and freshness.
go-local-dep llm_complete "${root}/golang/llm_complete" || exit 70
exec command llm_complete terminal "$@"
