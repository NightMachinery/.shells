##
function julia-install-packages() {
  juliaplain.dash -E "using Pkg ; Pkg.instantiate()"
}

function julia.add {
  local pkgs=($@)

  local cmd="import Pkg ; "

  local pkg
  for pkg in $pkgs[@] ; do
    cmd+="Pkg.add($(gquote-dq "$pkg")) ; "
  done

  reval-ec julia --startup-file=no --project="$PWD" -e "$cmd"
}
##
function julia-startup-link {
  lnrp ~[stochastic]/common/startup.jl ~/.julia/config/startup.jl
}
##
