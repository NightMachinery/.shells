realpath /proc/$$/exe; xargs -0 </proc/$$/cmdline; printf "%s " "$0" "$@"; echo
