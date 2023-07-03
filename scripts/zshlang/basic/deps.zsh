function ensure-dep-pdftk {
    ensure-dep1 pdftk brew install pdftk-java || return $?
}
##
function kitty-theme-install {
    pip-install psutil 'git+git://github.com/fretboardfreak/kitty-theme-changer.git@master'
    # psutil is needed by kitty_theme_changer.conf.py
}

function ensure-dep-kitty-theme {
    ensure-dep1 kitty-theme kitty-theme-install || return $?
}
##
function bicon-install {
    assert brew install fribidi @RET

    assert cdm ~/code/misc @RET
    trs ~/code/misc/bicon
    assert git clone https://github.com/NightMachinary/bicon @RET
    assert cd bicon @RET
    assert ./autogen.sh @RET
    assert ./configure @RET
    make || true # will error
    reval-ec sudo make install ||true # will error, but it will install bicon.bin
}
##
function golang-install {
    #: https://go.dev/doc/install
    #: https://files.lilf.ir/go1.19.1.linux-amd64.tar.gz
    ##
    local v="${1:-go1.19.1.linux-amd64.tar.gz}"
    if [[ "$v" != http* ]] ; then
        v="https://golang.org/dl/${v}"
    fi
    local name="${v:t}"
    name="${name:-go.tar.gz}"

    local d=~/code

    mkdir -p "$d" || return $?
    cd "$d" || return $?
    rm -rf "./${name}" && curl -L --output "$name" "${v}" || return $?
    sudo rm -rf /usr/local/go && sudo tar -C /usr/local -xzf "./${name}" || return $?

    echo "You might need to add 'export PATH=\"\$PATH:/usr/local/go/bin\"'." >&2
}
##
function caddy-install {
    assert go install github.com/caddyserver/xcaddy/cmd/xcaddy@latest @RET

    local d=~/bin/
    assert mkdir -p "$d" @RET
    assert cd "$d" @RET
    assert xcaddy build --with github.com/mholt/caddy-webdav --with github.com/caddyserver/forwardproxy@caddy2=github.com/klzgrad/forwardproxy@naive @RET

    if isLinux ; then
        rehash
        assert sudo setcap 'cap_net_bind_service=+ep' "$(grealpath -- "$(which caddy)")" @RET
    fi
}
##
function epubmerge-install {
    local dest=~/bin/epubmerge.py

    ensure-dir "$dest" @TRET

    gurl 'https://github.com/JimmXinu/EpubMerge/raw/main/epubmerge.py' > "$dest" @TRET
    chmod +x "$dest" @TRET

    rehash
    reval-ec epubmerge.py --help
}
##
