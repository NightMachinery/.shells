#!/usr/bin/env -S zsh -f
# Compatibility launcher for Hammerspoon. One FIM request arrives on stdin.
local root="${${(%):-%x}:A:h:h:h}"
exec "${root}/bin/llm-complete.zsh" fim
