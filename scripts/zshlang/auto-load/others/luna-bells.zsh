## See $nightNotes/cheatsheets/zsh/bells.org
###
function lunar() {
    tmuxnewsh2 deluna reval-notifexit deluna ${deluna} # timeout of deluna
    # lo_min should include the rest time as well, as the bells are sounded in the background currently.
    lo_s=$((60*${lo_min:-50})) lo_noinit=y lo_p=${lo_p:-~/tmp/.luna} loop "$@"
}
luna() {
    lunar pmset displaysleepnow
}
##
lunas() {
    lunar luna-advanced-bell
    display-gray-off # probably redundant
}
luna-advanced-bell() {
    awaysh-bnamed LUNA_MARKER h_luna-advanced-bell
}
redis-defvar luna_signal1
h_luna-advanced-bell() {
    setopt localtraps
    # So I don't understand these all that well, but here is my guess:
    trap "" INT # disables INT handling in this function, so we don't quit by INT
    (
        local signal1="$(luna_signal1_get)"
        luna_signal1_del

        display-gray-on

        if [[ "$(browser-current-url)" == *"https://vc.sharif.edu/ch/"* ]] ; then
            redo2 1 bell-visual-flash1
            sleep 10 # keeps screen gray
        else
            if [[ "$signal1" == flash ]] ; then
                redo2 10 bell-visual-flash1
            else
                local count=25
                # I did not find out how BTT knows whether there is sth playing. Anyhow, mpv-getv works as long as you don't play multiple videos simultaneously, and is cross-platform.
                if [[ "$(mpv-getv pause)" == 'false' ]] ; then
                    # fsay "Luna sees MPV"
                    bell-luna-mpv
                else
                    redo bell-luna "$count"
                fi
            fi
        fi
    ) # this is a new process so it has its own signal handling and so does quit on INT
    # 25  1:23.87 total
    # each is about 3.5s

    display-gray-off
    ecdate "Luna iterated."
}
function bell-zsh1() {
    @opts v 70 @ hearinvisible "$(rndarr $NIGHTDIR/resources/audio/zsh1/$~audioglob)"
}
bell-avarice() {
    # fsay "disquiet creatures of avarice have risen yet again ..."
    @opts v 70 @ hearinvisible-mpv "$(rndarr $NIGHTDIR/resources/audio/luna/$~audioglob)"
}
bell-luna-mpv() {
    @opts v 100 @ hearinvisible-mpv "$(rndarr $NIGHTDIR/resources/audio/luna_mpv/$~audioglob)"
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
    # @retiredtodo this doesn't result in a constantish duration, so we'll need additional code to check the duration in a while loop in luna-advanced-bell
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
    local gray="${lunaquit_grayoff:-y}"

    loop-startover ~/tmp/.luna "$@"
    pgrep LUNA_MARKER | inargsf reval-ec serr kill-withchildren
    if bool $gray ; then
        display-gray-off
    fi
}
function lunaquit-finalize() {
    display-gray-off ; bell-lm-amiindanger
}
function lq() {
    local gray="${1:-240}"

    @opts lunaquit_grayoff no @ lunaquit
    awaysh eval "sleep ${gray} ; lunaquit-finalize"
    # bell-lm-amiindanger
}
function deluna() {
    local nonce
    nonce="$(oneinstance-setup $0)" || return 1
    local timeout="${1:-240}" # 150 is good for PC work, but 800 might be better for reading, as the screen dims in 10 minutes
    ec "deluna (nonce: $nonce) started with timeout $timeout"
    while oneinstance $0 $nonce
    do
        (( $(getidle-darwin) >= $timeout || $(getlastunlock-darwin) <= 80 )) && {
            edPre=$'\n' ecdate "$(color 255 100 255 "Deluna committed homicide! (idle: $(getidle-darwin), last_unlock: $(getlastunlock-darwin))")"
            lunaquit " via deluna"
            # { isDbg && sleep 1 } || sleep 30
        }
        sleep 3 # to avoid cpu usage
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
aliasfn bello awaysh bell-diwhite # main gateway of a single alarm bell
aliasfn bell-zsh bell-zsh1
aliasfn bella-zsh bell-auto bell-zsh
@opts-setprefix bella-zsh bell-auto
# aliasfn bellz bella-zsh # just use bellj?
## https://www.iterm2.com/triggers.html
# We might need to use `reset` after these magic commands, as they cause more than once activation in certain situations (tmux, mosh, emacs, etc)(Because the redraw the screen): https://iterm2-discuss.narkive.com/BQCgkSxC/trigger-only-in-new-output
# If this is a big issue, add a UID to the magic commands, and store them in redis. Don't run commands for duplicate IDs. Periodically clean the stored IDs to minimize conflict chance.
aliasfnq bella-magic ec $'\n'"${ITERMMAGIC}_BELLA" # slow activation is due to iTerm. The bell 'rings' almost immediately once it reaches BrishGarden.
function bella-zsh-magic() {
    test -z "$ITERM_SESSION_ID" && return 1
    ec $'\n'"${ITERMMAGIC}_ZSH_BELLA_${ITERM_SESSION_ID}"
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
    
    if isDarwin && iterm-session-is-active ; then
        # this is called via BrishGarden by iTerm
        local skipfirst=''
        iterm-focus-is && skipfirst=y
        silent awaysh @opts sf "$skipfirst" t 60 @ bella-zsh
    fi
}
##
aliasfn bell-dl tts-glados1-cached 'Download, complete'
##
function bell-maker() {
    local name="${1:?}" f="${2:?}"

    fndef "bell-$name" awaysh-named "BELL_${name:u}_MARKER" hearinvisible "$GREENCASE_DIR/$f"
}
## Little Misfortune
function bell-lm-maker() {
    local name="${1:?}" f="${2:?}"

    fndef "bell-lm-$name" awaysh-named "BELL_LM_${name:u}_MARKER" hearinvisible "$GREENCASE_DIR/LittleMisfortune/$f"
}
function bell-lm-maker-dir() {
    local name="${1:?}" f="${2:?}"

    fndef "bell-lm-$name" awaysh-named "BELL_LM_${name:u}_MARKER" hear-rnd $GREENCASE_DIR/LittleMisfortune/$f/${~audioglob}
}
bell-lm-maker eternalhappiness 01_09_MI_eternalhappiness.flac
bell-lm-maker whattimeisit 20_02_MI_whattimeisit.flac
bell-lm-maker timetoparty flac/08_06_MI_timetocheckouttheparty..blue..flac
bell-lm-maker strawberryjuice flac/10.3_09_MI_strawberryjuice..blue..flac
bell-lm-maker amiindanger flac/06.4_04_MI_amiindanger.flac
bell-lm-maker shouldisitdown flac/16-1_15_MI_shouldisitdown.flac
bell-lm-maker-dir mhm mhm
# `fr heari 'flac/ MI cool'`
##
function reval-onhold() {
    local id="$(uuidm)_REVAL_ONHOLD_MARKER"

    setopt localtraps
    trap "" INT
    {
        (
            awaysh-bnamed "$id" @opts s 0 @ loop hearinvisible-mpv "$ONHOLD"
            reval "$@"
        )
    } always {
        kill-marker "$id"
    }
}
aliasfn kill-marker-onhold kill-marker REVAL_ONHOLD_MARKER
function reval-bell() {
    local bell=("${reval_bell_engine[@]:-${reval_bell_e[@]:-${reval_bell_bell[@]:-bello}}}")
    setopt localtraps
    trap "" INT
    {
        (
            reval "$@"
        )
    } always {
        silent reval "$bell[@]"
    }
}
aliasfn reval-bell-lm-mhm @opts bell bell-lm-mhm @ reval-bell
## Pink Panther:
bell-maker pp-electricity1 "PlayStation - Pink Panther Pinkadelic Pursuit - Everything/AMB07_etincel..blue..wav"
bell-maker pp-electricity2 "PlayStation - Pink Panther Pinkadelic Pursuit - Everything/AMB11_etincel..blue..wav" # shorter
bell-pp-electricity() { do-rnd bell-pp-electricity{1..2} }
bell-maker pp-nok "PlayStation - Pink Panther Pinkadelic Pursuit - Everything/SOUND_00_nok..blue..wav"
bell-maker pp-ok "PlayStation - Pink Panther Pinkadelic Pursuit - Everything/SOUND_00_ok..blue..wav"
bell-maker pp-attack-rotational1 "PlayStation - Pink Panther Pinkadelic Pursuit - Everything/SOUND_p_attac..blue..wav"
bell-maker pp-attack-rotational2 "PlayStation - Pink Panther Pinkadelic Pursuit - Everything/SOUND_p_attac2..blue..wav"
bell-maker pp-attack-rotational3 "PlayStation - Pink Panther Pinkadelic Pursuit - Everything/SOUND_p_attac3..blue..wav"
bell-pp-attack-rotational() { do-rnd bell-pp-attack-rotational{1..3} }
bell-maker pp-piece1 "PlayStation - Pink Panther Pinkadelic Pursuit - Everything/SOUND_piece1..blue..wav"
bell-maker pp-piece2 "PlayStation - Pink Panther Pinkadelic Pursuit - Everything/SOUND_piece2..blue..wav"
bell-maker pp-piece3 "PlayStation - Pink Panther Pinkadelic Pursuit - Everything/SOUND_piece3..blue..wav"
bell-pp-piece() { do-rnd bell-pp-piece{1..3} }
bell-pp-piece-r() { redo2 10 bell-pp-piece }
### Starcraft:
# @alt tts-gateway 'Garden, Online'
bell-maker sc2-nav_online "Starcraft/Starcraft II/Heart of the Swarm/PC Computer - StarCraft II Heart of the Swarm - Adjutant/Adjutant/zCutscene_Zerg04_DropShipAdjutant_020_navigation online..blue...ogg"
aliasfn reval-bell-sc2-nav_online @opts bell bell-sc2-nav_online @ reval-bell
##

bell-maker sc2-activating_bots "Starcraft/Starcraft II/Heart of the Swarm/PC Computer - StarCraft II Heart of the Swarm - Adjutant/Adjutant/zMission_Lab01_DropShipAdjutant_066_activating automated sentry bots..blue...ogg"
# aliasfn reval-bell-sc2-activating_bots @opts bell bell-sc2-activating_bots @ reval-bell

bell-maker sc2-eradicator_destroyed "Starcraft/Starcraft II/Heart of the Swarm/PC Computer - StarCraft II Heart of the Swarm - Adjutant/Adjutant/zMission_Lab01_DropShipAdjutant_172_eradicator destroyed situation critical..blue...ogg"
###
## Madagascar:
bell-maker penguins-smileandwave "madagascar movie/smileandwave.wav"
##
function bell-m-beeps() {
    : "Plays continuous beeps"
    
    perl -e 'for($i=0;;$i++){
    print pack("n", ((($i*($i>>8|$i>>9)&46&$i>>8))^($i&$i>>13|$i>>6)));
}' | silent play -c 1 -b 8 -e unsigned -t raw -r 8k -
}
##
function warn-posture() {
    lo_s=$((60*10)) loop tts-glados1-cached "Correct your posture. The tail should be backward."
}
##
aliasfnq bell-gibberish1-long tts-gateway-g1 'An apple is an edible fruit produced by an apple tree (Malus domestica). Apple trees are cultivated worldwide and are the most widely grown species in the genus Malus. The tree originated in Central Asia, where its wild ancestor, Malus sieversii, is still found today. Apples have been grown for thousands of years in Asia and Europe and were brought to North America by European colonists. Apples have religious and mythological significance in many cultures, including Norse, Greek, and European Christian tradition.'
##
function bell-visual-flash1() {
    local b off="${1:-0.1}" s="${2:-0.2}"
    b="$(brightness-get)" || return $?
    if (( (b) <= (off + 0.1) )) ; then
        off=0
    fi
    brightness-set "$off" || return $?
    sleep "$s"
    brightness-set "$b" || return $?
}
##
