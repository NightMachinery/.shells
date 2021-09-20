##
function julia-install-packages() {
  juliaplain.dash -E "using Pkg ; Pkg.instantiate()"
}
##
function julia-startup-link {
  lnrp ~[stochastic]/common/startup.jl ~/.julia/config/startup.jl
}
##
