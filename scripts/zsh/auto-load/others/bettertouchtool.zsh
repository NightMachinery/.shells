## https://docs.bettertouchtool.net/docs/1104_webserver.html
# @darwinonly We might need to add a `isDarwin || return 0` to these
function btt-refresh {
    local uuid="${1:? UUID required}"
    command curl --fail --silent "http://127.0.0.1:8855/refresh_widget/?uuid=$uuid"
}
function btt-update {
     local uuid="${1:? UUID required}" text="$2"
     # text="$(url-encode.py <<<"$text")" # doesn't encode newlines by current design
     command curl --fail --silent "http://127.0.0.1:8855/update_touch_bar_widget/?uuid=$uuid&text=$text"
}
##
function btt-datej() {
    # DEPRECATED: it's better to customize these stuff in BTT itself and just use `datej`
     <<EOF
{"text": "$(datej)",
"background_color": "100,10,255,255",
"font_size": 30}
EOF
}
