##
#: * See $nightNotes/cheatsheets/zsh/bells.org
#:
#: * We cache the results of some of the expensive macros like this:
#: =wh bell-lm-mhm| perl -ple 's|\Q$ENV{GREENCASE_DIR}\E|\$GREENCASE_DIR|gi'" ; s/'/\"/g "|cat-copy=
###
typeset -g LUNA_LOG=~/tmp/.luna_log
###
# return 0
if isNotExpensive ; then
    #: This single file costs a second of loading time :|
    #: [jalali:1401/09/07/02:19] @M2 after some optimizations, takes ~0.5s
    #: Update: now ~7s
    return 0
fi
###
function luna-log-tail {
    tail -f "${LUNA_LOG}"
}
aliasfn lunalog luna-log-tail
aliasfn llog luna-log-tail

function luna-skip-reset {
    luna_skipped_set 0
}

function lunaquit-force-skip-reset {
    lunaquit-force
    luna-skip-reset
}
alias lqs='lunaquit-force-skip-reset'
##
function lunar {
    tmuxnewsh2 deluna reval-notifexit deluna ${deluna} # timeout of deluna
    # lo_min should include the rest time as well, as the bells are sounded in the background currently.
    lo_s=$((60*${lo_min:-50})) lo_noinit=y lo_p=${lo_p:-~/tmp/.luna} loop "$@"
}

function luna {
    lunar pmset displaysleepnow
}
##
function lunas {
    : "lo_min=20 luna_duration=60 lunas"

    # local lo_min="${1:-$lo_min}"

    lunar luna-advanced-bell
    display-gray-off #: probably redundant
}

function luna-advanced-bell {
    awaysh-bnamed LUNA_MARKER h_luna-advanced-bell
}

redis-defvar luna_signal1
redis-defvar luna_skipped

function h_luna-advanced-bell {
    setopt localtraps
    # So I don't understand these all that well, but here is my guess:
    trap "" INT # disables INT handling in this function, so we don't quit by INT

    local duration="${luna_duration:-180}"
    local signal1="$(luna_signal1_get)"
    luna_signal1_del
    local skipped="${$(luna_skipped_get):-0}"
    local res
    res=$(( skipped + 1 )) || ectrace
    assert luna_skipped_set "$res"

    (
        local bell_awaysh=no hear_loudidle=no i
        if sharif-vc-is ; then
            display-gray-on

            if true ; then
                if (( skipped >= 1 )) ; then
                    alert_dur=0.5 alert "LUNA/skip: ${skipped}"
                fi
            else
                if (( skipped >= 1 )) ; then
                    tts-gateway "CRITICAL: You are at your ${skipped}th skip"
                fi
                @opts redo 1 @ bell-visual-flash1
                sleep 10 #: keeps screen gray
            fi
        elif true ; then
            if (( skipped >= 1 )) ; then
                display-gray-on-v1
                sleep 2 #: to wait for the macOS popup to fade
                alert_dur=4 alert "LUNA/skip: ${skipped}"
            else
                display-gray-on
            fi

            fnswap isColor True sleep-neon "${duration}" > "${LUNA_LOG}"
        else
            display-gray-on
            sleep 20 #: gives us time to finish our work or cancel the alarms

            if (( skipped >= 1 )) ; then
                brishz awaysh lunaquit-monitor '' ''
                for i in {1..3} ; do
                    @opts redo 1 @ bell-visual-flash1
                    tts-gateway "CRITICAL: You are at your ${skipped}th skip"
                    bell-hp3-be-careful-harry
                    bell-helicopter 20
                    bell-lm-amiindanger
                done
            else
                if [[ "$signal1" == flash ]] ; then
                    @opts redo 10 @ bell-visual-flash1
                else
                    local count=25
                    # I did not find out how BTT knows whether there is sth playing. Anyhow, mpv-get works as long as you don't play multiple videos simultaneously, and is cross-platform.
                    if [[ "$(mpv-get pause)" == 'false' ]] ; then
                        # fsay "Luna sees MPV"
                        bell-luna-mpv
                    else
                        redo bell-luna "$count"
                    fi
                fi
            fi
        fi
    ) # this is a new process so it has its own signal handling and so does quit on INT
    # 25  1:23.87 total
    # each is about 3.5s



    display-gray-off-v1
    display-gray-off
    luna_skipped_set 0
    ecdate "Luna iterated."
}

function sharif-vc-is {
    [[ "$(browser-current-url)" == *"https://vc.sharif.edu/ch/"* ]]
}

function bell-avarice {
    bella_zsh_disable1

    # fsay "disquiet creatures of avarice have risen yet again ..."
    @opts v 70 @ hearinvisible-mpv "$(rndarr $NIGHTDIR/resources/audio/luna/$~audioglob)"
}

function bell-luna-mpv {
    bella_zsh_disable1

    @opts v 100 @ hearinvisible-mpv "$(rndarr $NIGHTDIR/resources/audio/luna_mpv/$~audioglob)"
}

function bell-toy {
    bella_zsh_disable1

    # say "disquiet creatures of avarice have risen yet again ..."
    @opts v 140 @ hearinvisible "$(rndarr $GREENCASE_DIR/toystory2/**/$~audioglob)"
}

function greencase_audio_init {
    typeset -g greencase_audio_init

    { test -z "$greencase_audio_init" || test -n "$*" } && {
        greencase_audio=( $GREENCASE_DIR/**/$~audioglob )
        greencase_audio_init=y
    }
}

function bell-greencase {
    ##
    # @retiredtodo this doesn't result in a constantish duration, so we'll need additional code to check the duration in a while loop in luna-advanced-bell
    #
    ## Perf:
    # The delay is in the files themselves, these below have same time and both sound delayed:
    # time (@opts v 70 @ hearinvisible '/Users/evar/Base/Music/greencase/PC Computer - Portal 2 - Turret/turretlaunched05.wav')
    # time (hearinvisible '/Users/evar/Base/Music/greencase/PC Computer - Portal 2 - Turret/turretlaunched05.wav')
    ##
    bella_zsh_disable1

    greencase_audio_init
    reval-ec @opts v 140 @ hearinvisible "$(rndarr $greencase_audio[@])"
}

aliasfn bell-luna bell-avarice
# aliasfn bell-luna bell-greencase
##
function lunaquit {
    : "Global outputs: out"

    bella_zsh_disable1

    out=""

    local gray="${lunaquit_grayoff:-y}"

    loop-startover ~/tmp/.luna "$@" @TRET

    if bool $gray ; then
        display-gray-off-v1
        display-gray-off
    fi

    local pids
    pids="$(pgrep LUNA_MARKER)" || return 0
    [[ "$pids[*]" =~ '^\s*$' ]] && return 0
    out="$pids"
    ec "$pids" | inargsf reval-ec serr kill-withchildren || true
}

function h-lunaquit-force {
    lunaquit ; pkill LUNA ; sleep 1 && kill-marker-luna-timer-late
}

function h-lunaquit-force-dynamic {
    # bb_say_speed=1 fsay2 'sustain not crucify; great evil lies ahead!'
    tts-glados1-cached 'sustain not crucify; great evil lies ahead!'

    h-lunaquit-force
}

function lunaquit-force {
    #: This forces the updated version to be used, which allows us to dynamically swap the function called from any terminal session.
    brishz h-lunaquit-force-dynamic
}

function lunaquit-monitor {
    # @todo make this single-instance

    local finished=''
    trapexits
    {
        (
            local rest_dur="${1:-100}" lq_dur="${2:-240}"

            local i idle recent_idle=0
            mark-me "$0"
            for i in {1..${lq_dur}} ; do
                idle="$(idle-get)"
                if (( idle > recent_idle )) ; then
                    recent_idle="$idle"
                fi
                sleep 1
            done
            if (( recent_idle >= rest_dur )) ; then
                display-gray-off
                luna_skipped_set 0
                if (( idle <= 10 )) ; then
                    # tts-glados1-cached 'Good job'
                    bell-lm-mo-welldone
                fi
            else
                # awaysh-bnamed BELL_EVACUATE_MARKER bell-evacuate
                bell-evacuate
            fi
            mark-me zsh
        ) && finished=y # commenting this line might be better, as deluna will kill the timer-late anyway on inactivity
    } always {
        kill-marker-luna-timer-late
        if test -z "$finished" ; then
            mark-me "LUNA_MONITOR_TIMER_LATE"
            timer-late 0
        fi
        trapexits-release
    }
}
function kill-marker-luna-timer-late() {
    kill-marker "LUNA_MONITOR_TIMER_LATE" -9
}

function bell-evacuate() {
    kill-marker-luna-timer-late

    # `x (4*46)/0.78` = 235
    awaysh-bnamed LUNA_BELL_EVACUATE_MARKER @opts redo 235 @ bell-visual-flash1 # each iter takes ~0.78 (outdated?)

    local bell_awaysh=no hear_loudidle=no i
    for i in {1..4} ; do # each iter takes ~46s
        bell-sc2-personnel-must-evacuate
        bell-sc2-zerg-detected
        bell-sc2-will-face-justice
        bell-sc2-personnel-must-evacuate
        bell-helicopter
        bell-sc2-my-sins
        bell-sc2-extreme-zerg-infestation
        bell-sc2-personnel-must-evacuate
        bell-sc2-containment-breach
        bell-sc2-personnel-must-evacuate
        bell-sc2-my-sins
        bell-sc2-containment-breach
    done
    display-gray-off # if you're still here, I doubt keeping the display gray will be able to help you any. :(
}

function lunaquit-quick {
    # if there is no LUNA process around, we'll do nothing:
    local out
    # @opts grayoff no @ lunaquit
    lunaquit

    if test -n "$out" ; then
        brishz awaysh lunaquit-monitor "$@" # it marks itself
        # bell-lm-amiindanger
    fi
    display-gray-off
}
aliasfn lq lunaquit-quick
##
function deluna {
    local nonce
    nonce="$(oneinstance-setup $0)" || return 1
    local timeout="${1:-240}" # 150 is good for PC work, but 800 might be better for reading, as the screen dims in 10 minutes
    ec "deluna (nonce: $nonce) started with timeout $timeout"
    while oneinstance $0 $nonce
    do
        (( $(idle-get) >= $timeout || $(lastunlock-get) <= 120 )) && {
            edPre=$'\n' ecdate "$(color 255 100 255 "Deluna committed homicide! (idle: $(idle-get), last_unlock: $(lastunlock-get-min))")"
            ##
            lunaquit " via deluna" @STRUE
            kill-marker-luna-timer-late || true
            luna_skipped_set 0 @STRUE
            ##
            lock-hook
            ##
        }
        sleep 30 # to avoid cpu usage
    done
    ec deluna exited "(nonce: $nonce)"
}

function notnowluna {
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
bellj_socket=~/.sockets/bellj # @correct ensure-dir is called in `loop`

bellj_say="Jingle bells, jingle bells,
Jingle all the way.
Oh! what fun it is to ride
In a one-horse open sleigh."

function bell-jingles() {
    tts-glados1-cached $bellj_say
    # tts-gateway-i2 $bellj_say
}
##
function bell-ReichPhase() {
    @opts v 130 @ hearinvisible $NIGHTDIR/resources/audio/ReichPhase.wav
}
function bell-helicopter() {
    local duration="${1:-3}"

    trapexits
    {
        (
            ot-play-helicopter # it'd be better if this supported a duration itself, as ot-stop will stop everything.
            sleep "$duration"
        )
    } always {
        ot-stop
        trapexits-release
    }
}

function bell-diwhite {
    ot-play-diwhite "${1:-1}" "${@[2,-1]}"
}
##
function bell-repeat() {
    isSSH && {
        ecerr "$0: You're on SSH."
        return 1
    }
    local bell="${1:-bell-ReichPhase}"

    local bell_awaysh=no
    lo_sig2cancel=y lo_s=0.2 lo_p=${lo_p:-$bellj_socket} loop "$bell"
}
aliasfn bell-repeat-stop retry_sleep=0.1 retry-limited 500 loop-startover $bellj_socket
##
function bell-auto() {
    isDarwin || {
        ecerr "$0: Not running on Darwin."
        return 1
    }

    # You might need to use `sleep $timeout ; ...` to test bell-auto.

    bella_zsh_disable1
    local bell_awaysh=no

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
        if (( $(idle-get) <= $timeout )) ; then
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
aliasfn bell-auto-stop oneinstance-setup bell-auto # forces active bell-autos to exit
aliasfn bellaok bell-auto-stop
##
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

#: main gateway of a single alarm bell
# aliasfn bello awaysh bell-diwhite
aliasfn bello awaysh bell-ReichPhase
##
function bella_zsh_disable1 {
    if (( $#@ == 0 )) ; then
        typeset -g bella_zsh_disable1=y
    else
        bella_zsh_disable1=y reval "$@"
    fi
}

# aliasfn bell-zsh-start bell-sc2-I-return
aliasfn bell-zsh-start bell-lm-mhm

function bell-zsh1() {
    @opts v 70 @ hearinvisible "$(rndarr $NIGHTDIR/resources/audio/zsh1/$~audioglob)"
}
# aliasfn bell-zsh bell-zsh1
function bell-zsh() {
     local cmd head=''
        if cmd=($(hist-last)) ; then
            @opts dur 2 @ alert "Completed: $cmd"
            ##
            head="$(sout fnrep which 'print -r "$@" >&1 >&2' whichm "$cmd[1]" |& gtail -n 1)"
            if [[ "$head" =~ '^.*=\S*\s*(\S*)' ]] ; then
                head="${match[1]}"
            fi
            if test -z "$(ec "$head" | sd '\W' '')" ; then
                head="$cmd[1]"
            fi
            if test -n "$head" ; then
                local msg="Completed: $head"
                dact var-show msg

                if isDarwin ; then
                    # bell_awaysh=no redo2 2 tts-say-i1 $msg
                    ##
                    local fsay_v fsay_r
                    fsay_r=140
                    ##
                    #: @bad
                    # fsay_v=Bubbles
                    # fsay_v=Boing
                    ##
                    # fsay_v=Sandy
                    # fsay_v=Trinoids
                    # fsay_v=Zarvox
                    # fsay_v=Whisper
                    fsay_v=Cellos
                    # fsay_v='Good News'
                    # fsay_v=Organ
                    # bell_awaysh=no redo2 2 fsay $msg
                    fsay "$msg, $msg"
                fi
            fi
        fi
}
aliasfn bella-zsh bell-auto bell-zsh
@opts-setprefix bella-zsh bell-auto
# aliasfn bellz bella-zsh # just use bellj?
## https://www.iterm2.com/triggers.html
# We might need to use `reset` after these magic commands, as they cause more than once activation in certain situations (tmux, mosh, emacs, etc)(Because the redraw the screen): https://iterm2-discuss.narkive.com/BQCgkSxC/trigger-only-in-new-output
# If this is a big issue, add a UID to the magic commands, and store them in redis. Don't run commands for duplicate IDs. Periodically clean the stored IDs to minimize conflict chance.
aliasfnq bella-magic ec $'\n'"${ITERMMAGIC}_BELLA" # slow activation is due to iTerm. The bell 'rings' almost immediately once it reaches BrishGarden.

function bella-zsh-magic {
    test -z "$ITERM_SESSION_ID" && return 1
    ec $'\n'"${ITERMMAGIC}_ZSH_BELLA_${ITERM_SESSION_ID}"
}

function bella-zsh-gateway {
    if { isGuest && ! isGrayfur } || bool "$bella_zsh_disable1" || test -z "$HISTFILE" ; then
        # bella-zsh is a tad opinionated, so I am disabling it for guests.

        return 0
    fi

    if isSSH ; then
        # bella-zsh-magic
    elif terminal-supported-p ; then
        bella-zsh-maybe
    fi
}

function bella-zsh-maybe {
    # @notcrossplatform
    # this is called via BrishGarden by iTerm

    if isDarwin ; then
        local ITERM_SESSION_ID="${1:-$ITERM_SESSION_ID}" # brishz has its own bogus ITERM_SESSION_ID, so prefer explicit input

        # https://github.com/marzocchi/zsh-notify does essentially the same thing as us, so we can copy it to support Linux and Apple Terminal.
        if terminal-session-is-focused ; then
            local skipfirst=''
            terminal-is-focused && skipfirst=y
            silent awaysh @opts sf "$skipfirst" t 60 @ bella-zsh
        else
            # Do not use bell-auto for inactive sessions
            awaysh bell-zsh
        fi
    fi
}
##
aliasfn bell-dl tts-glados1-cached 'Download, complete'
##
function bell-enabled-p {
    typeset -g bell_enabled_p

    if test -n "$bell_enabled_p" ; then
        bool "$bell_enabled_p"
        return $?
    else
        if h-bell-enabled-p; then
            bell_enabled_p=y
            return 0
        else
            bell_enabled_p=n
            return 1
        fi
    fi
}

function h-bell-enabled-p {
    if isMBP || isMB2 ; then
        return 0
    fi

    if isServer || ! test -d "$GREENCASE_DIR" ; then
        return 1
    else
        return 0
    fi
}

function bell-maker {
    #: @warning This function gets called a lot, so do not create subshells in it.
    ##
    local name="${1}" fs=( ${@[2,-1]} )

    local fn="bell-$name"
    if (( ${#fs} == 0 )) || ! bell-enabled-p ; then
        fndef $fn true
    else
        assert-args name fs @RET
        local i
        for i in {1..${#fs}} ; do
            local f="${fs[$i]}"
            ##: This is too expensive:
            # if ! test -e "$f" ; then
            #     fs[$i]="$GREENCASE_DIR/$f"
            # fi
            # fs[$i]="$(grealpath -- "${fs[i]}")"
            ##
            if [[ "$f" != /* ]] ; then
                fs[$i]="$GREENCASE_DIR/$f"
            fi
            ##
        done

        local marker
        marker="BELL_${${name:u}//-/_}_MARKER"
        # marker="BELL_${name:u}_MARKER"

        fndef $fn bell-ringer "$marker" "$fs[@]"
    fi
}

function bell-ringer {
    local marker="${1}" fs=("${@[2,-1]}") awaysh="${bell_awaysh:-y}"
    assert-args marker fs

    bella_zsh_disable1
    if @opts p [ redo loop ] r y xp [ bell- tts- ] @ fn-isTop ; then
        if test -z "$bell_awaysh" ; then
            ecgray "$0: parent 'redo' (etc) detected; Switching awaysh off."
            awaysh="no"
        fi
    fi
    if bool "$awaysh" ; then
        awaysh-bnamed "$marker" @opts loudidle y @ hear-rnd "$fs[@]"
    else
        hear_loudidle="${hear_loudidle:-y}" hear-rnd "$fs[@]"
    fi
}
## Little Misfortune
function bell-lm-maker {
    local name="${1}" fs=("${@[2,-1]}")

    if (( ${#fs} == 0 )) || ! bell-enabled-p ; then
        fndef "bell-lm-$name" true
    else
        assert-args name fs @RET

        local i
        for i in {1..${#fs}} ; do
            local f="${fs[$i]}"
            if [[ "$f" != /* ]] ; then
                fs[$i]="$GREENCASE_DIR/LittleMisfortune/$f"
            fi
        done

        bell-maker "lm-$name" "$fs[@]"
    fi
}

function bell-lm-maker-dir {
    local name="${1}" f="${2}"

    if isServer || ! test -e "$GREENCASE_DIR" ; then
        fndef "bell-lm-$name" true
    else
        assert-args name f @RET

        bell-lm-maker "$name" $GREENCASE_DIR/LittleMisfortune/$f/${~audioglob}
    fi
}

typeset -ga bell_lm_MI_files=()
function bell-lm-MI {
    if (( ${#bell_lm_MI_files} == 0 )) ; then
        typeset -ga bell_lm_MI_files=("${GREENCASE_DIR}"/LittleMisfortune/flac/*MI*.flac(.DN))
    fi
    bell-ringer $bell_lm_MI_files{@}
}

bell-lm-maker diary-search-fx 'flac/BenjaminDiarySearch.flac'
bell-lm-maker eternalhappiness 01_09_MI_eternalhappiness.flac
bell-lm-maker whattimeisit 20_02_MI_whattimeisit.flac
bell-lm-maker timetoparty flac/08_06_MI_timetocheckouttheparty..blue..flac
bell-lm-maker strawberryjuice flac/10.3_09_MI_strawberryjuice..blue..flac
bell-lm-maker amiindanger flac/06.4_04_MI_amiindanger.flac
bell-lm-maker shouldisitdown flac/16-1_15_MI_shouldisitdown.flac
bell-maker lm-fail LittleMisfortune/flac/10.4_27_MI_fail1..blue..flac
aliasfn bell-fail bell-lm-fail
##
# bell-lm-maker-dir mhm mhm
function bell-lm-mhm {
	bell-ringer "BELL_LM_MHM_MARKER" "$GREENCASE_DIR/LittleMisfortune/mhm/16_31_MI_mhm..blue..flac" "$GREENCASE_DIR/LittleMisfortune/mhm/16_36_MI_mmhm..blue..flac" "$GREENCASE_DIR/LittleMisfortune/mhm/17.2_08_MI_mmhmm..blue..flac" "$GREENCASE_DIR/LittleMisfortune/mhm/18_09_MI_mmhmm..blue..flac"
}
##
bell-lm-maker mo-welldone flac/17.1_11_MO_welldone.flac
# `fr heari 'flac/ MI cool'`
##
function reval-onhold() {
    local id="$(uuidm)_REVAL_ONHOLD_MARKER"
    id="${id:u}"

    setopt localtraps
    trap "" INT
    {
        (
            ##
            # awaysh-bnamed "$id" @opts s 0 @ loop hearinvisible "$ONHOLD"
            awaysh-bnamed "$id" play "$ONHOLD" repeat 999999999 # repeat is faster than loop
            ##
            reval "$@"
        )
    } always {
        # @raceCondition it's possible to kill the parent shell before the player has spawned, and then the player can go on live forever. Since switching to 'play' from mpv, this problem should have become somewhat mitigated
        kill-marker $id
        awaysh-bnamed KILLER eval "sleep 1 && kill-marker $id"
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
        bell_awaysh=n silent reval "$bell[@]" # bell_awaysh seems to not work when we use 'xl', idk why
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

bell-maker sc2-activating-defense-turrets 'Starcraft/Starcraft II/Heart of the Swarm/PC Computer - StarCraft II Heart of the Swarm - Adjutant/Adjutant/zMission_Lab01_DropShipAdjutant_067..activating automated defense turrets..blue..ogg'

bell-maker sc2-activate-warbot-shield 'Starcraft/Starcraft II/Heart of the Swarm/PC Computer - StarCraft II Heart of the Swarm - Adjutant/Adjutant/zMission_Lab01_DropShipAdjutant_079..activate warbot shield..blue..ogg'

bell-maker sc2-activating_bots "Starcraft/Starcraft II/Heart of the Swarm/PC Computer - StarCraft II Heart of the Swarm - Adjutant/Adjutant/zMission_Lab01_DropShipAdjutant_066_activating automated sentry bots..blue...ogg"
# aliasfn reval-bell-sc2-activating_bots @opts bell bell-sc2-activating_bots @ reval-bell

bell-maker sc2-eradicator_destroyed "Starcraft/Starcraft II/Heart of the Swarm/PC Computer - StarCraft II Heart of the Swarm - Adjutant/Adjutant/zMission_Lab01_DropShipAdjutant_172_eradicator destroyed situation critical..blue...ogg"

bell-maker sc2-containment-breach 'Starcraft/Starcraft II/Heart of the Swarm/PC Computer - StarCraft II Heart of the Swarm - Adjutant/Adjutant/zMission_Lab01_DropShipAdjutant_061..containment breach, zerg specimens free..blue..ogg'

bell-maker sc2-zerg-detected 'Starcraft/Starcraft II/Heart of the Swarm/PC Computer - StarCraft II Heart of the Swarm - Adjutant/Adjutant/zScripted_ZAdjutantIntro_DropShipAdjutant_019_warning zerg organisms detected..blue..ogg'

bell-maker sc2-extreme-zerg-infestation 'Starcraft/Starcraft II/Heart of the Swarm/PC Computer - StarCraft II Heart of the Swarm - Adjutant/Adjutant/zCutscene_Zerg04_DropShipAdjutant_014_warning destination contains extreme levels of zerg infestation, confirm..blue..ogg'

bell-maker sc2-personnel-must-evacuate 'Starcraft/Starcraft II/Heart of the Swarm/PC Computer - StarCraft II Heart of the Swarm - Adjutant/Adjutant/zCutscene_Zerg02_DropShipAdjutant_016_all personal must evacuate..blue..ogg'

bell-maker sc2-no-more-personnel-remain 'Starcraft/Starcraft II/Heart of the Swarm/PC Computer - StarCraft II Heart of the Swarm - Adjutant/Adjutant/zMission_Lab01_DropShipAdjutant_118..no more personnel remain on sublevel..blue..ogg'

bell-maker sc2-testing-repeating-instructions 'Starcraft/Starcraft II/Heart of the Swarm/PC Computer - StarCraft II Heart of the Swarm - Adjutant/Adjutant/zMission_Lab01_DropShipAdjutant_150..testing, no playback errors detected, repeating instructions..blue..ogg'

bell-maker sc2-subject-unresponsive-translating 'Starcraft/Starcraft II/Heart of the Swarm/PC Computer - StarCraft II Heart of the Swarm - Adjutant/Adjutant/zMission_Lab01_DropShipAdjutant_151..subject is unresponsive, translating isntructions into native language..blue..ogg'
##
bell-maker sc2-my-sins 'Starcraft/Starcraft II/Heart of the Swarm/PC Computer - StarCraft II Heart of the Swarm - Zeratul/Zeratul/zSMAmbient_Zeratul_Zeratul_003_my sins weigh heavy..blue..ogg'

bell-maker sc2-will-face-justice 'Starcraft/Starcraft II/Heart of the Swarm/PC Computer - StarCraft II Heart of the Swarm - Zeratul/Zeratul/zSMAmbient_Zeratul_Zeratul_004_I will face justice for my acts..blue..ogg'

bell-maker sc2-serve-xelnaga "Starcraft/Starcraft II/Heart of the Swarm/PC Computer - StarCraft II Heart of the Swarm - Zeratul/Zeratul/zSMAmbient_Zeratul_Zeratul_005_I serve the \"Xel'naga\"..blue..ogg"
##
bell-maker sc2-I-return 'Starcraft/Starcraft II/Heart of the Swarm/PC Computer - StarCraft II Heart of the Swarm - Zurvan/Zurvan/zMission_Zerus01_AncientOneUnnamed_025..sc2-I-return..blue..ogg'

bell-maker sc2-evil-laugh 'Starcraft/Starcraft II/Heart of the Swarm/PC Computer - StarCraft II Heart of the Swarm - Zurvan/Zurvan/zMission_Zerus01_AncientOneUnnamed_133_alt1..zerus evil laugh..blue..ogg'

bell-maker sc2-it-is-time 'Starcraft/Starcraft II/Heart of the Swarm/PC Computer - StarCraft II Heart of the Swarm - Zurvan/Zurvan/zMission_Zerus03_AncientOneNamed_011..it is time..blue..ogg'

bell-maker sc2-become-primal 'Starcraft/Starcraft II/Heart of the Swarm/PC Computer - StarCraft II Heart of the Swarm - Zurvan/Zurvan/zSMAmbient_ZerusZurvan_AncientOneNamed_004..become pure, become primal..blue..ogg'

bell-maker sc2-evo-perfection 'Starcraft/Starcraft II/Heart of the Swarm/PC Computer - StarCraft II Heart of the Swarm - Abathur/Abathur/zSMAmbient_EvolutionMaster_EvolutionMaster_003_perfection, deep in the core, in strands..blue..ogg'
##
bell-maker sc-error 'Starcraft/Starcraft I Brood War/PC Computer - StarCraft Brood War - Other/sounds/perror..sc-error..blue..wav'

# bell-maker sc2- ''
###
## Madagascar:
bell-maker penguins-smileandwave "madagascar movie/smileandwave.wav"
## HP3:
bell-maker hp3-platform-movement 'HP3/PC Computer - Harry Potter & the Prisoner of Azkaban - Sound Effects/soundeffects.uax/common/platform_movement.wav'

bell-maker hp3-be-careful-harry 'HP3/PC Computer - Harry Potter & the Prisoner of Azkaban - English Dialogue 12/Dialog part 1/pc_her_Adv6_7_be careful harry..blue..wav'

bell-maker hp3-water-trickle \
    "$GREENCASE_DIR/HP3/PC Computer - Harry Potter & the Prisoner of Azkaban - Sound Effects/soundeffects.uax/ambience/water_trickle01.wav" \
    "$GREENCASE_DIR/HP3/PC Computer - Harry Potter & the Prisoner of Azkaban - Sound Effects/soundeffects.uax/ambience/water_trickle02.wav" \
    "$GREENCASE_DIR/HP3/PC Computer - Harry Potter & the Prisoner of Azkaban - Sound Effects/soundeffects.uax/ambience/water_trickle03.wav" \
    "$GREENCASE_DIR/HP3/PC Computer - Harry Potter & the Prisoner of Azkaban - Sound Effects/soundeffects.uax/ambience/water_trickle04.wav" \

bell-maker hp3-star-pickup 'HP3/PC Computer - Harry Potter & the Prisoner of Azkaban - Sound Effects/soundeffects.uax/Magic/pickup_star1..blue..wav'
## Sonic:
bell-maker sonic-fx-zone-moved 'Sonic and SEGA All-Stars Racing/PC Computer - Sonic and SEGA All-Stars Racing - General Sound Effects/Sonic and Sega All-Stars Racing/General SFX/MSN_ZONE_MOVED.wav'

bell-maker sonic-fx-ready 'Sonic and SEGA All-Stars Racing/PC Computer - Sonic and SEGA All-Stars Racing - General Sound Effects/Sonic and Sega All-Stars Racing/General SFX/FE_READY.wav'

bell-maker sonic-fx-finish 'Sonic and SEGA All-Stars Racing/PC Computer - Sonic and SEGA All-Stars Racing - General Sound Effects/Sonic and Sega All-Stars Racing/General SFX/HUD_FINISHLINE.wav'
##
aliasfn bell-dl-ready tts-glados1-cached "Media ready to download"

aliasfn bell-files-moved tts-glados1-cached 'files moved!'
##
function bell-m-beeps() {
    : "Plays continuous beeps"
    
    perl -e 'for($i=0;;$i++){
    print pack("n", ((($i*($i>>8|$i>>9)&46&$i>>8))^($i&$i>>13|$i>>6)));
}' | silent play -c 1 -b 8 -e unsigned -t raw -r 8k -
}
##
function warn-posture() {
    lo_s=$((60*10)) loop h_warn-posture
}
function h_warn-posture() {
    local o
    o="$(idle-get)" @TRET
    if (( o < 120 )) ; then
        tts-glados1-cached "Correct your posture. The tail should be backward."
    fi
}
##
function bell-entropy-gained {
    tts-glados1-cached "Entropy gained"
}
##
aliasfnq bell-gibberish1-long tts-gateway-g1 'An apple is an edible fruit produced by an apple tree (Malus domestica). Apple trees are cultivated worldwide and are the most widely grown species in the genus Malus. The tree originated in Central Asia, where its wild ancestor, Malus sieversii, is still found today. Apples have been grown for thousands of years in Asia and Europe and were brought to North America by European colonists. Apples have religious and mythological significance in many cultures, including Norse, Greek, and European Christian tradition.'
##
redis-defvar bell_visual_flash1_lock
function bell-visual-flash1() {
    if [[ "$(bell_visual_flash1_lock_get)" == y ]] ; then
        tts-gateway "Flash disabled because of lock"
        return 0
    fi

    trapexits

    bell_visual_flash1_lock_set y # @todo2 use better locking primitives? This is still open to race conditions


    {
        local b off="${1:-0.1}" s="${2:-0.2}" redo="${bell_visual_flash1_redo:-1}" s_after="${bell_visual_flash1_sa:-0.3}"
        b="$(brightness-get)" || return $?
        if (( (b) <= (off + 0.1) )) ; then
            off=0
        fi

        (
            for i in {1..${redo}} ; do
                brightness-set "$off" || return $?
                sleep "$s"
                brightness-set "$b" || return $?
                sleep "$s_after"
            done
        )
    } always {
        brightness-set "$b"
        bell_visual_flash1_lock_del
        trapexits-release
    }
}
##
# aliasfn bell-image-uploaded tts-glados1-cached 'image, uploaded'
aliasfn bell-image-uploaded tts-glados1-cached 'picture, uploaded'
# aliasfn bell-image-uploaded bell-pp-attack-rotational
##
bell-maker 'batman-cave-open' 'batman/PlayStation 2 - LEGO Batman - Sound Effects/BATCAVE_BIGDOOR_OPEN.wav'
##
bell-maker 'p2-searching' 'PC Computer - Portal 2 - Turret/turret_search_4..blue..wav'

bell-maker 'p2-target-lost' 'PC Computer - Portal 2 - Turret/turret_search_2..blue..wav'

bell-maker 'p2-target-acquired' 'PC Computer - Portal 2 - Turret/turret_active_2.wav'
##
bell-maker "attention-needed-rk" "${NIGHTDIR}/resources/audio/tts/15.ai/Rise Kujikawa/Your attention is needed.wav"

bell-maker "attention-needed-st" "${NIGHTDIR}/resources/audio/tts/15.ai/sentry\ turret/Your\ attention\ is\ needed.wav"

aliasfn bell-attention-needed bell-attention-needed-st
##
function bell-continuous {
    bell_awaysh=n loop bell-lm-MI
}
aliasfn bellc bell-continuous
##
function bell-ddd {
    bella_zsh_disable1

    if isDarwin ; then
        command say -v cello "di di di di di di di di di di di di di di di di di di di di di di di di di di"
    else
        return 1
    fi
}

function bell-pi {
    bella_zsh_disable1

    if isDarwin ; then
        command say -v cello 3.14159265358979323846264
        #: 3.141592653589793
    else
        return 1
    fi
}
##
function bell-great-evil {
    bella_zsh_disable1

    if isDarwin ; then
        fsay_r=140 fsay_v=Trinoids fsay "Great Evil"
    else
        return 1
    fi
}
##
function bell-awaiting-commands {
    fsay2 'Awaiting commands!'
}
##
function bell-gpt {
    awaysh command say -v Whisper -r 90 "GPT"
}
##
