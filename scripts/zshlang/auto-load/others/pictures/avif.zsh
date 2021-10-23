function tiff2avif {
    local i="$1"
    assert-args i @RET
    local o="${2:-${i:r}.avif}"

    local tmp
    {
        # @toFuture/1402 heif-enc's avif support should have gotten fixed on macOS, no?
        ##
        tmp="$(gmktemp --suffix=.png)" @TRET
        magick convert "$i" "$tmp" @TRET

        reval-ec avifenc --speed 4 --jobs all "$tmp" --output "$o"
        # --speed=0 takes ~2m! The difference is also negligible (around ~5kb).
    } always { silent trs-rm "$tmp" }
}
