##
function dock-bounce-enable() {
    defaults write com.apple.dock no-bouncing -bool FALSE
    killall Dock
}
function dock-bounce-disable() {
    defaults write com.apple.dock no-bouncing -bool TRUE
    killall Dock
}
##
function screen-gray-set-darwin () {
    # @deprecated Use =display-gray-on= and co
    ##
    defaults write com.apple.universalaccess grayscale -bool $1
    defaults write com.apple.CoreGraphics DisplayUseForcedGray -bool $1
    launchctl unload /System/Library/LaunchAgents/com.apple.universalaccessd.plist # Operation not permitted while System Integrity Protection is engaged
    launchctl load /System/Library/LaunchAgents/com.apple.universalaccessd.plist

    case "$1" in
        "NO")
            echo "  Changing Display to use color. This will take a moment..."
        ;;
        "YES")
            echo "  Changing Display to use grayscale. This will take a moment..."
        ;;
    esac
    ##

}
##
function frontapp-get {
    #: [[id:5192ebaf-95c8-43df-be58-c153ab412564][macOS/front app]]
    ##
    local frontapp="${front_app_cached}"
    if test -n "$frontapp" ; then
        ec "${frontapp}"
        return 0
    fi

    {
        if isDarwin ; then
            if true ; then
            frontapp-get-bundle-id-osa
            else
                #: @upstreamBug? [[id:1163a291-fbc8-4f77-8b53-9b58be684e66][=lsappinfo info= doesn't return anything.]]
                ##
                local info
                info="$(lsappinfo info "$(lsappinfo front)")" @TRET

                local bundle_id
                if bundle_id="$(ec "$info" | command rg --only-matching --replace='$1' 'bundleID\s*=\s*"([^"]*)"\s*$')" ; then
                ec "$bundle_id"
                return 0
                fi

                local bundle_path
                if bundle_path="$(ec "$info" | command rg --only-matching --replace='$1' 'bundle\s*path="([^"]*)"')" ; then
                ec "$bundle_path"
                fi
            fi
        else
            ectrace "Linux not supported"
            return 1
        fi
    } | cat-copy-if-tty
}

function frontapp-get-bundle-id-osa {
    #: @slow
    #: `time2 frontapp-get-bundle-id-osa`
    #: 0.2s
    ##
    osascript <<EOF
tell application "System Events"
    bundle identifier of (first process whose frontmost is true)
end tell
EOF
}

function frontapp-get-name-osa {
    osascript <<EOF
tell application "System Events"
    name of (first process whose frontmost is true)
end tell
EOF
}

##
alias mac-mail-log="sudo log stream --predicate  '(process == \"smtpd\") || (process == \"smtp\")' --info" #this command starts filtering, so after that you get log messages when you start accessing smtp.

alias lock='"/System/Library/CoreServices/Menu Extras/User.menu/Contents/Resources/CGSession" -suspend ; pmset displaysleepnow' # Command+Ctrl+q locks natively; Use lock.as to press them ;)) (Needs assistive access)

function finder-hideicons() {
    defaults write com.apple.finder CreateDesktop false
    killall Finder
}
##
function macos-version {
    if isDarwin ; then
    command sw_vers |
        rget 'ProductVersion:\s+(\S+)' |
        cat-copy-if-tty
    else
        return 1
    fi
}

function macos-version-major {
    macos-version | rget '^(\d+)' |
      cat-copy-if-tty
}

function macos-ventura-or-higher-p {
    local v
    v="$(macos-version-major)" @RET
    (( v >= 13 ))
}
##
function app-icon-get {
    #: @warn Keep backticks out of the `:` docstrings; they are double-quoted, so zsh
    #: runs command substitution on them.
    : "outputs the path to a cached PNG of <app>'s icon, extracting it on first use

<app> is an app name or a path, e.g. app-icon-get Claude"
    # @darwinOnly
    ##
    ensure isDarwin @MRET

    local app="${1}"
    assert-args app @RET

    if [[ "$app" != /* ]] ; then
        app="/Applications/${app%.app}.app"
    fi
    test -d "$app" || {
        ecerr "$0: no such app: ${app}"
        return 1
    }

    local cache_dir="${HOME}/tmp/app-icons"
    local out="${cache_dir}/${${app:t:r}// /_}.png"
    if test -s "$out" ; then
        ec "$out"
        return 0
    fi

    local icns="${app}/Contents/Resources/$(defaults read "${app}/Contents/Info" CFBundleIconFile 2>/dev/null)"
    #: CFBundleIconFile is allowed to omit the extension.
    [[ "$icns" == *.icns ]] || icns="${icns}.icns"
    test -e "$icns" || {
        ecerr "$0: could not find an icns for: ${app}"
        return 1
    }

    mkdir -p "$cache_dir" @RET

    local tmp
    tmp="$(gmktemp -d)" @TRET
    {
        #: sips also converts icns, but picks an arbitrary (often tiny) representation.
        iconutil -c iconset "$icns" -o "$tmp/i.iconset" @RET

        local best='' candidate
        #: 256px is plenty for a notification; the 1024px @2x representation is a
        #: ~750KB file for no visible gain.
        for candidate in icon_256x256.png icon_512x512.png icon_128x128.png ; do
            if test -s "$tmp/i.iconset/${candidate}" ; then
                best="$tmp/i.iconset/${candidate}"
                break
            fi
        done
        if test -z "$best" ; then
            best="$(command ls "$tmp/i.iconset"/*.png(N) 2>/dev/null | gtail -n 1)"
        fi
        test -n "$best" || {
            ecerr "$0: the iconset came out empty for: ${app}"
            return 1
        }

        command cp "$best" "$out" @RET
    } always {
        command rm -rf "$tmp"
    }

    ec "$out"
}
##
