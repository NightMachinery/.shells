##
function julia-install-packages() {
  juliaplain.dash -E "using Pkg ; Pkg.instantiate()"
}

function julia.add {
  local pkgs=($@)
  local project="${julia_add_p-$PWD}"

  local cmd="import Pkg ; "
  local pkg
  for pkg in $pkgs[@] ; do
    cmd+="Pkg.add($(gquote-dq "$pkg")) ; "
  done
  cmd+="using Dates ; Pkg.gc(collect_delay=Day(0)) ; "
  cmd+="Pkg.instantiate() ; Pkg.precompile() ; "

  local opts=()
  if test -n "$project" ; then
    opts+=(--project="$project")
  fi

  reval-ec julia --startup-file=no "$opts[@]" -e "$cmd"
}
aliasfn jins julia_add_p='' julia.add
##
function julia-startup-link {
  lnrp ~[stochastic]/common/startup.jl ~/.julia/config/startup.jl
}
##
