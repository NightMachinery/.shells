##
function nvtop-install {
    #: https://github.com/Syllo/nvtop#distribution-specific-installation-process
    ##

    if true ; then
        sudo apt-get install -y nvtop #: @Ubuntu/21.10
    else
        sudo apt-get install -y cmake libdrm-dev libncurses5-dev libncursesw5-dev git @RET

        local d=~/code/misc
        mkdir -p "$d" @RET
        cd "$d" @RET

        { git clone https://github.com/Syllo/nvtop.git || { cd nvtop && git pull & cd .. } } @RET
        mkdir -p nvtop/build && cd nvtop/build @RET
        cmake .. @RET
        make @RET

        sudo make install @RET
    fi
}
##
