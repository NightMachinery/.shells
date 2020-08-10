OVERTONE_DIR=~/code/Clj/i-laugh
OVERTONE_PORT=7699
aliasfn in-ot indir "$OVERTONE_DIR"
OVERTONE_LOADED=''
function ot-server() {
    OVERTONE_LOADED=''
    in-ot lein repl :headless :port "$OVERTONE_PORT"
}
function ot-rep() {
    if test -z "$OVERTONE_LOADED" ; then
        OVERTONE_LOADED='y'
        ot-rep "(use 'overtone.live)"
    fi
    rep --port "$OVERTONE_PORT" "$@"
}
function ot-rep-test() {
    ot-rep '(demo (sin-osc 440))'
}
