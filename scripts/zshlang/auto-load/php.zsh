##
function php-mod-ls {
    php --modules
    #: shows compiled in modules
}

function php-mod-ensure {
    local mods=( $@ )

    local mods_installed
    mods_installed="$(php -m)" @TRET

    local mod r=0
    for mod in ${mods[@]} ; do
        if ec "$mods_installed" | rg --null-data -v --quiet "$mod" ; then
            ec "$mod"
            r=1
        fi
    done

    return $r
}
##
function php-mod-install {
    local mods=( $@ )
    assert-args mods @RET

    ##
    #: * [[https://github.com/Homebrew/homebrew-core/issues/41081][pecl fails by default · Issue #41081 · Homebrew/homebrew-core]]
    #: ** https://github.com/Homebrew/homebrew-core/issues/97962
    mkdir -p "$(pecl config-get ext_dir)" @TRET
    # mkdir -p /usr/local/lib/php/pecl @TRET
    # mkdir -p "$(brew --prefix php)"/pecl @TRET #: does not work, symlink problems
    ##

    env-clean NIGHTDIR_PATH_MODE=d bash -ic "pecl install $(gq "${mods[@]}")" @RET
}
##
