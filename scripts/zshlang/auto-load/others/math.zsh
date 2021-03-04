function calc-julia() {
    # would be faster if did not import these stuff
    julia --startup-file=no --print "using Distributions, StatsBase, Statistics, Base.MathConstants ; $*"
}
alias xj='\noglob calc-julia'
calc-raw() {
    python3 -c "from math import *; print($*)"
}
ialias calc='noglob calc-raw'
ialias x='noglob calc-raw'
