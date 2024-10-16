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
#### nightshift
# @darwinonly https://github.com/leberwurstsaft/nshift/releases
aliasfn nightshift-on nshift 100
aliasfn nightshift-off nshift off

redis-defvar nightshift_auto_enabled
nightshift_auto_enabled_setnx 1
function nightshift-auto() {
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
###
# ### Doesn't work. From https://gist.github.com/thomasfinch/14bd3181799734c872d2ad3b207cc01c
# CORE_BRIGHTNESS="/var/root/Library/Preferences/com.apple.CoreBrightness.plist"
# CBUser="CBUser-$(dscl . -read ~ GeneratedUID | sed 's/GeneratedUID: //')"
# ## read:
# # sudo defaults read $CORE_BRIGHTNESS "CBUser-$(dscl . -read ~ GeneratedUID | sed 's/GeneratedUID: //')"
# NIGHTSHIFT_ALWAYS='{
#     CBBlueLightReductionCCTTargetRaw = 2700;
#     CBBlueReductionStatus =     {
#         AutoBlueReductionEnabled = 1;
#         BlueLightReductionAlgoOverride = 4;
#         BlueLightReductionAlgoOverrideTimestamp = "1399-07-18 17:38:24 +0000";
#         BlueLightReductionDisableScheduleAlertCounter = 3;
#         BlueLightReductionSchedule =         {
#             DayStartHour = 7;
#             DayStartMinute = 0;
#             NightStartHour = 7;
#             NightStartMinute = 1;
#         };
#         BlueReductionAvailable = 1;
#         BlueReductionEnabled = 1;
#         BlueReductionMode = 2;
#         BlueReductionSunScheduleAllowed = 1;
#         Version = 1;
#     };
# }'
# ##
# function nightshift-always-darwin() {
#     sudo defaults write $CORE_BRIGHTNESS "$CBUser" "$NIGHTSHIFT_ALWAYS"
# }
# ###
####
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
