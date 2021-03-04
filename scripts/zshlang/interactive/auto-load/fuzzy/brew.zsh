function ffbrew() {
    local engine=("${ffbrew_engine:-${ffbrew_e:-info}}")
    local search_engine=("${ffbrew_search_engine:-${ffbrew_se:-leaves}}")

    brew $search_engine | fz --query="$@" | inargsf brew "$engine[@]"
}
aliasfn ffbinfo ffbrew
aliasfn ffbi @opts e install search-engine search @ ffbrew
aliasfn bif ffbi
aliasfn ffbu @opts e upgrade @ ffbrew
aliasfn ffbrm @opts e remove @ ffbrew
