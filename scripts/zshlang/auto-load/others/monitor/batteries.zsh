##
function batteries {
    : "usage: batteries [--local | --host HOST ...] [--json] [--watch SECONDS]
Tailnet battery dashboard, with shared peripherals deduplicated by address."
    local root="${NIGHTDIR}"
    local no_color="${NO_COLOR+x}"
    ensure-cmd python3 @RET

    local color=never
    if [[ -z "${no_color}" ]] && isColorTty ; then
        color=always
    fi
    command python3 "${root}/python/batteries.py" --color="${color}" "$@"
}
alias bb='batteries'
##
