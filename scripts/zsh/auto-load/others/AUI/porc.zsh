function porc-listen() {
    #  porc.py in snips-alice
    porc.py --keywords grasshopper blueberry bumblebee grapefruit terminator americano --sensitivities 1 1 1 1 1 1
}
function porc-process() {
    local keyword="${1:?}"

    case "$keyword" in
        grasshopper) hammerspoon -c 'scrollHandler(3)' ;;
        bumblebee) hammerspoon -c 'scrollHandler(3) ; scrollHandler(3) ; scrollHandler(3)' ;;
    esac
}
