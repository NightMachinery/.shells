function porc-listen() {
    #  porc.py in snips-alice
    porc.py --keyword_paths $PURGATORY/miss_alice_mac_2020-11-13_v1.8.0.ppn --keywords grasshopper blueberry bumblebee grapefruit terminator americano # --sensitivities 1 1 1 1 1 1
}
function porc-process() {
    local keyword="${1:?}"
    # @todo https://stackoverflow.com/questions/64419066/macos-get-the-current-output-level-of-the-speakers

    typeset -p keyword
    case "$keyword" in
        grasshopper) hammerspoon -c 'scrollHandler(3)' ;;
        bumblebee)
            # hammerspoon -c 'hs.eventtap.scrollWheel({0,1000},{}, "pixel")'
            hammerspoon -c 'scrollHandler(3) ; scrollHandler(3) ; scrollHandler(3)'
            ;;
        terminator)
            # hammerspoon -c 'hs.eventtap.scrollWheel({0,3000},{}, "pixel")'
            ;;
        miss) ec miss
              hs-popclickPlayPause
              ;;
    esac
}
