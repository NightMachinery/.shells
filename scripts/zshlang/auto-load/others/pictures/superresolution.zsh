function waifu2x-ncnn-vulkan() {
    local model="${waifu2x_ncnn_vulkan_m:-models-cunet}" # cunet: 2D illustration

    # needs abs path of model or it'll segfault
    assert revaldbg command waifu2x-ncnn-vulkan -g -1 -m "${$(realpath2 waifu2x-ncnn-vulkan):h}/${model}" "$@" @RET
}
function superres-anime-image() {
    ## @alt
    # * isr_simple.py
    # * https://github.com/jiny2001/dcscn-super-resolution
    ##

    local i="${1}" # input image path (jpg/png/webp) or directory
    local o="${2:-${i:r}_enhanced.png}" # output image path (jpg/png/webp) or directory

    assert waifu2x-ncnn-vulkan -i "$i" -o "$o" "${@[3,-1]}" @RET

    ## borg:
    # .ad superres-anime-image * o.png -n 3 -s 4
    # jdoc
    ##
}
