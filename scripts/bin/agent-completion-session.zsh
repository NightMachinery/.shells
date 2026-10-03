#!/usr/bin/env zsh
#: Resolve using the established session policy. kitty ls JSON is stdin.
local ls_json="$(command cat)"
h-agent-session-of-kitty-window "${ls_json}" "${1}" "${2}"
