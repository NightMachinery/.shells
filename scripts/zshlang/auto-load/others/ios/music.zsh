function npp() {
    local q="$*"

    assert pushf /Volumes/hyper-diva/Songs @RET
    {
        music_dir=. playlister "$q" > _pl.m3u @RET
        cat _pl.m3u | prefixer -a '../../' > ./_playlists/auto/"$( { ecn "$q ; " ; dateshort } | str2filename)".m3u @TRET
    } always { popf }
}
