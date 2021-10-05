##
function h-jupyter-kernel-julia-name-get {
    jupyter kernelspec list | rget '^\s*(julia-\d+\.\d+)\s+' | ghead -n 1
}
aliasfn jupyter-kernel-julia-name-get eval-memoi h-jupyter-kernel-julia-name-get
##
