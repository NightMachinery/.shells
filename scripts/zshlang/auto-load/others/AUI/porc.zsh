function porc-listen() {
    #  porc.py in snips-alice
    porc.py --keyword_paths $PURGATORY/miss_alice_mac_2020-11-13_v1.8.0.ppn --keywords grasshopper blueberry bumblebee grapefruit terminator americano # --sensitivities 1 1 1 1 1 1
}
function porc-process() {
    local keyword="${1:?}"
    typeset -p keyword
    # @todo https://stackoverflow.com/questions/64419066/macos-get-the-current-output-level-of-the-speakers

    local  app="$(frontapp-get)"
    case "$keyword" in
        grasshopper) hammerspoon -c 'scrollHandler(3)' ;;
        bumblebee)
            # hammerspoon -c 'hs.eventtap.scrollWheel({0,1000},{}, "pixel")'
            case "$app" in
                io.mpv)
                    hammerspoon -c "hs.eventtap.keyStroke({}, hs.keycodes.map['space'])"
                ;;
            esac
            hammerspoon -c 'scrollHandler(3) ; scrollHandler(3) ; scrollHandler(3)'
            ;;
        terminator)
            # hammerspoon -c 'hs.eventtap.scrollWheel({0,3000},{}, "pixel")'

            # case "$app" in
            #     io.mpv)
            #         hammerspoon -c "hs.eventtap.keyStroke({}, hs.keycodes.map['space'])"
            #     ;;
            # esac
            ;;
        miss)
            # hs-popclickPlayPause
              ;;
    esac
}
