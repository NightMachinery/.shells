## Vars
export mpv_ipc=~/tmp/.mpvipc

## Functions
function mpv() {
    command mpv --sub-auto=fuzzy --fs --input-ipc-server "$mpv_ipc" "${(0@)$(rpargs "$@")}"
}
mpv-get() {
    <<<'{ "command": ["get_property", "'"${1:-path}"'"] }' socat - "${2:-$mpv_audio_ipc}"|jq --raw-output -e .data
}
mpv-getv() mpv-get "$1" "$mpv_ipc"

