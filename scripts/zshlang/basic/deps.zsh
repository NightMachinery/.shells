function ensure-dep-pdftk {
    ensure-dep1 pdftk brew install pdftk-java || return $?
}
##
function kitty-theme-install {
    pip-install psutil 'git+git://github.com/fretboardfreak/kitty-theme-changer.git@master'
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
