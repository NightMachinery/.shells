##
function apkdl() {
    local links
    links=( ${(@f)"$(apkdl.py $1)"} )
    aa -o "${links[1]}.apk" $links[2]
}
reify apkdl
noglobfn apkdl
##
function h-hotspot-1081-to-9081 {
    reval-ec gost -L socks5://127.0.0.1:9081 -L http://127.0.0.1:9087 -F "socks5://$(router-ip):1081"
}

function hotspot-1081-to-9081 {
    tmuxnewsh2 hotspot-1081-to-9081 h-hotspot-1081-to-9081
}

function 1070-to-9081 {
    tmuxnewsh2 hotspot-1081-to-9081 gost -L socks5://127.0.0.1:9081 -L http://127.0.0.1:9087 -F "socks5://localhost:1070"
}

function 1097-to-9081 {
    tmuxnewsh2 hotspot-1081-to-9081 gost -L socks5://127.0.0.1:9081 -L http://127.0.0.1:9087 -F "socks5://localhost:1097"
}
##
