function bell-sc-can-rolling() {
    awaysh silent gtimeout 9s sclang $NIGHTDIR/supercollider/canRolling.sc
}
function bell-sc-dial() {
    awaysh silent gtimeout 8s sclang $NIGHTDIR/supercollider/phonedial.sc
}
function bell-sc-finished() {
    awaysh silent gtimeout 6s sclang $NIGHTDIR/supercollider/finished.sc
}
