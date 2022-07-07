##
function conda-init {
    test -n "$CONDA_IS_LOADED" && return 0
    local __conda_setup="$(conda 'shell.zsh' 'hook' 2> /dev/null)"
    if [ $? -eq 0 ]; then
        eval "$__conda_setup"
        CONDA_IS_LOADED=y
    else
        ecerr conda not loaded
    fi
}
# aliasfn cin conda-init

function conda-activate {
    typeset -g _conda_last_activation=("$@")
    conda-init
    conda deactivate #: needed to make conda update stuff when it thinks the env is already active
    conda activate "${_conda_last_activation[@]}"

    if false && proxy-auto-p ; then
        #: conda will source its own useless 'conda' function wrapper, which we need to again replace by our own proxifier wrapper
        pxify-command conda
    fi
}
aliasfn cina conda-activate

function conda-reactivate {
    #: '__conda_reactivate' did not do the job.
    ##
    if test -n "$CONDA_DEFAULT_ENV" || (( ${#_conda_last_activation} > 0 )) ; then
        ###
        #: These work when I invoke them manually, but adding them to 'sb' doesn't work ...
        # reval-ec conda-activate "${_conda_last_activation[@]}"
        ###
        #: We can at least deconfuse the prompt.
        export CONDA_DEFAULT_ENV='?'
        ##
        # conda-init
        # reval-ec conda deactivate
        ###
    fi
}
##
