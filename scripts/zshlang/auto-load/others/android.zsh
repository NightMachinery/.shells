##
function apkdl() {
    local links
    links=( ${(@f)"$(apkdl.py $1)"} )
    aa -o "${links[1]}.apk" $links[2]
}
reify apkdl
noglobfn apkdl
##
function h-hotspot-to-9081 {
    local hotspot_port="${hotspot_port:-10886}"

    reval-ec gost -L socks5://127.0.0.1:9081 -L http://127.0.0.1:9087 -F "socks5://$(router-ip):${hotspot_port}"
}

function hotspot-to-9081 {
    tmuxnewsh2 hotspot-to-9081 hotspot_port="${hotspot_port}" h-hotspot-to-9081
}

function 1070-to-9081 {
    tmuxnewsh2 hotspot-to-9081 gost -L socks5://127.0.0.1:9081 -L http://127.0.0.1:9087 -F "socks5://localhost:1070"
}

function 1097-to-9081 {
    tmuxnewsh2 hotspot-to-9081 gost -L socks5://127.0.0.1:9081 -L http://127.0.0.1:9087 -F "socks5://localhost:1097"
}
##
