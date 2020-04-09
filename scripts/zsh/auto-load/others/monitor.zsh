function jprocs() {
	jah procs -c always "$@"
}
function jprocs-pic() {
	procs "$@" | convert -page  4000x4000 -font FreeMono -pointsize 20 -fill black -background white -trim +repage -bordercolor white  -border 15 text:- png:"$0 $*".png
	jdoc
}
function getidle-darwin() {
    ioreg -c IOHIDSystem | awk '/HIDIdleTime/ {print $NF/1000000000; exit}'
}
