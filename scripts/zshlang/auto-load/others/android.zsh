function apkdl() {
    local links
    links=( ${(@f)"$(apkdl.py $1)"} )
    aa -o "${links[1]}.apk" $links[2]
}
reify apkdl
noglobfn apkdl
