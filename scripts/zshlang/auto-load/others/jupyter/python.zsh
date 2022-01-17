##
function jupyter-requirements {
    #: Usage: cat requirements.txt | jupyter-requirements
    ##
    prefixer --skip-empty --add-prefix='!pip install -U '
}
##
