function waifu2x-ncnn-vulkan() {
    local model="${waifu2x_ncnn_vulkan_m:-models-cunet}"

    # needs abs path of model or it'll segfault
    assert revaldbg command waifu2x-ncnn-vulkan -g -1 -m "${$(realpath2 waifu2x-ncnn-vulkan):h}/${model}" "$@" @RET
}
function superres-anime-image() {
    local i="${1}" # input image path (jpg/png/webp) or directory
    local o="${2:-${i:r}_enhanced.png}" # output image path (jpg/png/webp) or directory

    assert waifu2x-ncnn-vulkan -i "$i" -o "$o" "${@[3,-1]}" @RET
}
