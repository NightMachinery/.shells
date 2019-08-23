aa-raw() {
    local opts=()
    isI || opts+=(--show-console-readout false --summary-interval 0)
    aria2c --seed-time=0 --max-tries=0 --retry-wait=1 $opts[@] "$@" #-Z has some unsavory sideeffects so I have not included it in this.
}
