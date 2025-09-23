function calc-julia() {
    local boot="using Base.MathConstants"
    # would be faster if did not import these stuff
    # booti+=" ; using Distributions, StatsBase, Statistics"
    julia --startup-file=no --print "$boot ; $*"
}
alias xj='\noglob calc-julia'
##
function calc-raw {
    python3 -c "from math import *; print($*)" |
        cat-copy-if-tty
}
ialias calc='noglob calc-raw'
ialias x='noglob calc-raw'
