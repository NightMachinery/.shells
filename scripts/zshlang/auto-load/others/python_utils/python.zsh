##
function python-package-version {
    local import_p="${import_p:-y}"
    if bool "$import_p" ; then
        import_p=True
    else
        import_p=False
    fi

    python -c "import sys ; import json ; from pynight.common_package import packages_commit_get ; print(json.dumps(packages_commit_get(sys.argv[1:], import_p=${import_p},), indent=2))" "$@"
}
##
function comment-rm-py {
    cat-paste-if-tty |
        perl -CS -lne 'm/^\s*#/ || print' |
        cat-copy-if-tty
}

function comment-rm-latex {
    cat-paste-if-tty |
        perl -CS -lne 'm/^\s*%/ || print' |
        cat-copy-if-tty
}
##
