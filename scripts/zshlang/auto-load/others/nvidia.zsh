##
function nvtop-install {
    #: https://github.com/Syllo/nvtop#distribution-specific-installation-process
    ##

    sudo apt-get install -y cmake libncurses5-dev libncursesw5-dev git @RET

    local d=~/code/misc
    mkdir -p "$d" @RET
    cd "$d" @RET

    git clone https://github.com/Syllo/nvtop.git @RET
    mkdir -p nvtop/build && cd nvtop/build @RET
    cmake .. @RET
    make @RET

    sudo make install @RET
}
##
