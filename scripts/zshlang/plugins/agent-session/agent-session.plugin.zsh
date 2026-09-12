# Loader for the portable exact-conversation and tmux pane runtime.
[[ ${agent_session_plugin_loaded_p:-} == y ]] && return 0
local agent_session_plugin_dir="${${(%):-%x}:A:h}"
source "${agent_session_plugin_dir}/session.zsh" || return $?
typeset -g agent_session_plugin_loaded_p=y
