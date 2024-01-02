function yabai-focus-app {
    local app_name="$1"
    assert-args app_name @RET

    local window_id
    window_id="$(yabai -m query --windows | jq 'map(select(.app == "'${app_name}'")) | .[0].id')" @RET
    #: @upstreamBug This works, but you might need to cycle through all the windows manually once for yabai to "see" them.

    reval-ecgray yabai -m window --focus "${window_id}"
}
