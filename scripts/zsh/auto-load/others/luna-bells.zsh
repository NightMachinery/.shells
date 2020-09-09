function lunar() {
    deluna ${deluna} & # timeout of deluna
    # lo_min should include the rest time as well, as the bells are sounded in the background currently.
    lo_s=$((60*${lo_min:-50})) lo_noinit=y lo_p=${lo_p:-~/tmp/.luna} loop "$@"
}
luna() {
    lunar pmset displaysleepnow
}
##
lunas() {
    lunar awaysh-bnamed LUNA_MARKER bell-many
}
bell-many() {
    setopt localtraps
    # So I don't understand these all that well, but here is my guess:
    trap "" INT # disables INT handling in this function, so we don't quit by INT
    (redo bell-luna 25) # this is a new process so it has its own signal handling and so does quit on INT
    # 25  1:23.87 total
    # each is about 3.5s

    ecdate "Luna iterated."
}
function bell-zsh1() {
    @opts v 70 @ hearinvisible "$(rndarr $NIGHTDIR/resources/audio/zsh1/$~audioglob)"
}
bell-avarice() {
    # say "disquiet creatures of avarice have risen yet again ..."
    @opts v 70 @ hearinvisible "$(rndarr $NIGHTDIR/resources/audio/luna/$~audioglob)"
}
bell-toy() {
    # say "disquiet creatures of avarice have risen yet again ..."
    @opts v 140 @ hearinvisible "$(rndarr $GREENCASE_DIR/toystory2/**/$~audioglob)"
}
function greencase_audio_init() {
    { test -z "$greencase_audio_init" || test -n "$*" } && {
        greencase_audio=( $GREENCASE_DIR/**/$~audioglob )
    }
}
bell-greencase() {
    ##
    # @todo0 this doesn't result in a constantish duration, so we'll additional code to check the duration in a while loop in bell-many
    #
    ## Perf:
    # The delay is in the files themselves, these below have same time and both sound delayed:
    # time (@opts v 70 @ hearinvisible '/Users/evar/Base/Music/greencase/PC Computer - Portal 2 - Turret/turretlaunched05.wav')
    # time (hearinvisible '/Users/evar/Base/Music/greencase/PC Computer - Portal 2 - Turret/turretlaunched05.wav')
    ##
    greencase_audio_init
    reval-ec @opts v 140 @ hearinvisible "$(rndarr $greencase_audio[@])"
}
aliasfn bell-luna bell-avarice
# aliasfn bell-luna bell-greencase
##
function lunaquit() {
    loop-startover ~/tmp/.luna "$@"
    pgrep LUNA_MARKER | inargsf reval-ec serr kill-withchildren
}
aliasfn lq lunaquit
function deluna() {
    local nonce
    nonce="$(oneinstance-setup $0)" || return 1
    local timeout="${1:-240}" # 150 is good for PC work, but 800 might be better for reading, as the screen dims in 10 minutes
    ec "deluna (nonce: $nonce) started with timeout $timeout"
    while oneinstance $0 $nonce
    do
        (( $(getidle-darwin) >= $timeout || $(getlastunlock-darwin) <= 80 )) && {
            edPre=$'\n' ecdate "$(color 255 100 255 'Deluna committed homicide!')"
            lunaquit " via deluna"
            # { isDbg && sleep 1 } || sleep 30
        }
        sleep 30 # to avoid cpu usage
    done
    ec deluna exited "(nonce: $nonce)"
}
function notnowluna() {
    mdoc "DEPRECATED: Using lunaquit handles this usecase as well nowadays.
Not Now Luna!" MAGIC
    local vol="$(get-volume)"
    local timeout=150

    set-volume 0

    # No need for this much force:
    # local started="$(date +"%s")"
    # while (( ($(date +"%s") - $started) <= $timeout ))
    # do
    #     set-volume 0
    # done

    sleep $timeout

    set-volume "$vol"
}
##
bellj_socket=~/.sockets/bellj
bellj_say="Jingle bells, jingle bells,
Jingle all the way.
Oh! what fun it is to ride
In a one-horse open sleigh."
function bell-jingles() {
    fsay $bellj_say
}
function bell-ReichPhase() {
    @opts v 130 @ hearinvisible $NIGHTDIR/resources/audio/ReichPhase.wav
}
function bell-helicopter() {
    local duration="${1:-3}"

    ot-play-helicopter # it'd be better if this supported a duration itself, as ot-stop will stop everything.
    sleep "$duration"
    ot-stop
}
function bell-diwhite() {
    ot-play-diwhite "${1:-1}"
}
##
function bell-repeat() {
    isSSH && {
        ecerr "$0: You're on SSH."
        return 1
    }
    local bell="${1:-bell-ReichPhase}"
    lo_sig2cancel=y lo_s=0.2 lo_p=${lo_p:-$bellj_socket} loop "$bell" #bell-helicopter
}
aliasfn bell-repeat-stop retry_sleep=0.1 retry-limited 500 loop-startover $bellj_socket
function bell-auto() {
    isDarwin || {
        ecerr "$0: Not running on Darwin."
        return 1
    }
    # You might need to use `sleep $timeout ; ...` to test bell-auto.
    local engine=( "${@:-bello}" )
    local timeout="${bell_auto_t:-30}" # The timeout should perhaps be bigger than sleep+engine, otherwise activity can get ignored.
    local sleep="${bell_auto_sleep:-${bell_auto_st:-0}}" # The lower, the more CPU usage in single mode.
    local single="${bell_auto_single:-${bell_auto_sm}}"
    local skipfirst="${bell_skip_first:-${bell_auto_sf}}"
    local exit_cmd=("${(@)bell_auto_exit}")

    local nonce
    nonce="$(oneinstance-setup bell-auto)" || return 1

    ec "$0 (nonce: $nonce) started with timeout $timeout and engine: $engine[@]"

    if test -z "$skipfirst" ; then
        reval "$engine[@]"
        sleep "$sleep" # necessary for succesfully exiting sc bells because they take time to initialize. It also makes it act as a onetime bell in case of an active user.
    fi

    while oneinstance $0 $nonce
    do
        if (( $(getidle-darwin) <= $timeout )) ; then
            ec "$0 exited because of user activity."
            break
        fi
        if test -z "$single" ; then # it's better that we we play first and then sleep
            reval "$engine[@]"
        fi
        # @optional We can repeat the checks here, to exit more aggressively
        sleep "$sleep"
    done
    test -n "$exit_cmd[*]" && reval-ec "$exit_cmd[@]"
    ec "$0 exited. (nonce: $nonce)"
}
aliasfn bell-auto-stop oneinstance-setup bell-auto
aliasfn bellaok bell-auto-stop
aliasfn bellsc-stop ot-stop
##
aliasfn bellsc-heli ot-play-helicopter
aliasfn bell-auto-sc @opts single y sleep 1 t 1.3 exit bellsc-stop @ bell-auto
aliasfn bella-heli bell-auto-sc bellsc-heli
aliasfn bella-diwhite bell-auto-sc bell-diwhite 9999999 # 9999999/3600/24 = 115.74072916666667
aliasfn bellr-toy bell-repeat bell-toy
aliasfn bellr-gc bell-repeat bell-greencase
aliasfn bella-toy bell-auto bell-toy
aliasfn bella-gc bell-auto bell-greencase
##
aliasfn bella bella-toy
aliasfn bellj bella
aliasfn okj bell-auto-stop
aliasfn bello bell-diwhite # main gateway of a single alarm bell
aliasfn bell-zsh bell-zsh1
aliasfn bella-zsh bell-auto bell-zsh
@opts-setprefix bella-zsh bell-auto
# aliasfn bellz bella-zsh # just use bellj?
## https://www.iterm2.com/triggers.html
# We might need to use `reset` after these magic commands, as they cause more than once activation in certain situations (tmux, mosh, emacs, etc)(Because the redraw the screen): https://iterm2-discuss.narkive.com/BQCgkSxC/trigger-only-in-new-output
# If this is a big issue, add a UID to the magic commands, and store them in redis. Don't run commands for duplicate IDs. Periodically clean the stored IDs to minimize conflict chance.
aliasfn bella-magic ec '${ITERMMAGIC}'_BELLA
function bella-zsh-magic() {
    test -z "$ITERM_SESSION_ID" && return 1
    ec "${ITERMMAGIC}_ZSH_BELLA_${ITERM_SESSION_ID}"
}
function bella-zsh-gateway() {
    if isSSH ; then
        bella-zsh-magic
    else
        bella-zsh-maybe
    fi
}
function bella-zsh-maybe() {
    # @notcrossplatform
    local ITERM_SESSION_ID="${1:-$ITERM_SESSION_ID}" # brishz has its own bogus ITERM_SESSION_ID, so prefer explicit input
    
    isDarwin && iterm-session-is-active && {
        local skipfirst=''
        iterm-focus-is && skipfirst=y
        silent awaysh @opts sf "$skipfirst" t 60 @ bella-zsh
        }
}
##
