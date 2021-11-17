##
function nvtop-install {
    #: https://github.com/Syllo/nvtop#distribution-specific-installation-process

    sudo apt install cmake libncurses5-dev libncursesw5-dev git

    cdm ~/code/misc
    git clone https://github.com/Syllo/nvtop.git
    mkdir -p nvtop/build && cd nvtop/build
    cmake ..
    make

    sudo make install
}
##
