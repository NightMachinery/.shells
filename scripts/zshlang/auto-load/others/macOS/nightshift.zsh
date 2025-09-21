##
function nightshift-on-darwin {
    @darwinOnly

    #: No need to hardcode the temperature here
    # nightlight temp 100

    nightlight on
}
aliasfn nightshift-on nightshift-on-darwin

function nightshift-off-darwin {
    @darwinOnly

    nightlight off
}
aliasfn nightshift-off nightshift-off-darwin

function nightshift-toggle-darwin {
    @darwinOnly

    nightlight toggle
}
aliasfn nightshift-toggle nightshift-toggle-darwin

function nightshift-status-darwin {
    @darwinOnly

    ec "status: $(nightlight status)" @STRUE
    ec "temperature: $(nightlight temp)"
}
aliasfn nightshift-status nightshift-status-darwin
##
#: nightshift-auto will set the night shift mode depending on the frontmost app.
redis-defvar nightshift_auto_enabled
nightshift_auto_enabled_setnx 1
function nightshift-auto {
    # `nightshift_auto_enabled_set 0` to disable
    if (( $(nightshift_auto_enabled_get) != 1 )) ; then
        return 0
    fi

    local  app="$(frontapp-get)"
    case "$app" in
        io.mpv|mpv|*/bin/mpv)
            nightshift-off
            ;;
        *) nightshift-on ;
    esac
}
##
