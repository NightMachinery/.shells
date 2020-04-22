aa-raw() {
    local opts=('--stderr=true')
    # Redirect all console output that would be otherwise printed in stdout to stderr.  Default: false

    isI || opts+=(--show-console-readout false --summary-interval 0)
    aria2c --seed-time=0 --max-tries=0 --retry-wait=1 $opts[@] "$@" #-Z has some unsavory sideeffects so I have not included it in this.
}
aagh() { aa "${(@f)$(gh-to-raw "$@")}" }
aacookies() {
    mdoc "$0 <aa-args>
Uses |theCookies| var or else feeds first URL to |cookies|." MAGIC

    aa-raw --header="$(cookies-auto "$@")" $@
}
