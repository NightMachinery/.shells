##
aliasfn asitop TERM=xterm-256color sudo asitop
##
function glan() {
    # --time sets the refresh delay in seconds
    # --byte display network rate in byte per second
    glances --config ~/.glances --time 10 --theme-white --disable-webui --fs-free-space --byte --process-short-name "$@"
}
##
function fftop {
    # linux: top -p
    # darwin: top -pid
    htop -p "${(j.,.)${(@f)$(ffps "$@")}}"
}
# aliasfn pt fftop # process-top
##
function cpu-core-count {
    nproc
    #: part of GNU coreutils
}

function cpu-usage-get() {
    ps -A -o %cpu | awk '{s+=$1} END {print s "%"}'
}
##
function vm_stat-h {
    vm_stat |
        perl -ne '/page size of (\d+)/ and $size=$1; /Pages\s+([^:]+)[^\d]+(\d+)/ and printf("%-16s % 16.2f Mi\n", "$1:", $2 * $size / 1048576);'
}

function memory-free-get {
    local free_ram
    if isDarwin ; then
        free_ram=$(( $({
                    vm_stat | rget 'Pages free:\s+(\d+)\.'
                    vm_stat | rget 'Pages speculative:\s+(\d+)\.'
                    vm_stat | rget 'Pages inactive:\s+(\d+)\.'
                } | in-sum) * $(vm_stat | rget 'page size of (\d+)') ))
        ##
        # https://developer.apple.com/library/archive/documentation/Performance/Conceptual/ManagingMemory/Articles/AboutMemory.html
        #
        # - *Free memory:* This is RAM that's not being used.

        # - *Wired memory:* Information in this memory can't be moved to the hard disk, so it must stay in RAM. The amount of Wired memory depends on the applications you are using.

        # - *Active memory:* This information is currently in memory, and has been recently used.

        # - *Inactive memory:* This information in memory is not actively being used, but was recently used.

        # - *Used:* This is the total amount of memory used.
        #
        # By using purgeable memory, you allow the system to quickly recover memory if it needs to, thereby increasing performance. Memory that is marked as purgeable is not paged to disk when it is reclaimed by the virtual memory system because paging is a time-consuming process. Instead, the data is discarded, and if needed later, it will have to be recomputed.
    else
        free_ram="$(command free --bytes | rg 'Mem: '| awkn 7)" @TRET
        ## @backends
        # - =cat /proc/meminfo=
        # - =free=
        # - =vmstat=
        ##
    fi

    ec "$free_ram" | numfmt-humanfriendly-bytes
}
##
function  pt-cpu-get() {
    procs --or "$@" | gtail -n +3 | awk '{print $4}' | in-sum
}

function  pt-cpu-get-grep() {
    procs | rg "$@" | awk '{print $4}' | in-sum
}

function pt-cpu-get-plus() {
    local q="$*" a
    if [[ "$q" =~ '^\d+$' ]] ; then
        pt-cpu-get "$q"
    else
            a=( ${(@f)"$(pgrep -f -i "$q")"} )
            if test -n "${a[*]}" ; then
                pt-cpu-get "$a[@]"
            else
                ec 0 # no process found so 0 cpu used by them
            fi
    fi
}
##
function t_cpu-get-lang-icon() {
    # did not work well. Buffering issues?
    procs | rg "input_lang_get_icon" | rg -v rg
}
##
function pt-cpu-i() {
    : "Supports multiple processes"
    local pids
    pids=("${(@f)$(ffps "$@")}") @RET

    lo_s=0.05 serr loop pt-cpu-get $pids[@] | plot-stdin
}
aliasfn ffcpu pt-cpu-i

function pt-cpu() {
    lo_s=0.05 serr loop pt-cpu-get-plus "$@" | plot-stdin
}
##
function ppgrep() {
    case "$(uname)" in
        Darwin)
            \pgrep -i "$@" | gxargs --no-run-if-empty ps -fp
            ;;
        Linux)
            \pgrep "$@" | gxargs --no-run-if-empty ps -fp
            # Linux's pgrep doesn't support -i
            ;;
    esac
}
##
function jtop {
    if isLinux ; then
        jah top -b -n1
    else
        @NA
    fi
}

function  jglan() {
    # doesn't work all that well (skips some newlines)
    fnswap glances 'gtimeout 10s unbuffer glances' glan | aha --line-fix > jglan.html # --black is worse
}

function jhtop() {
    gtimeout 1s htop | aha --line-fix --black > jhtop.html
}

function jprocs() {
    procs --pager disable --color always | aha --black > jprocs.html
}

function jprocs-pic() {
    procs "$@" | text2img "$0 $*"
    jdoc
}
##
function lid-opened-log {
    #: pretty useless?
    pmset -g log | rg --smart-case lidopen
}

function sleep-log-darwin {
    pmset -g log | rg -e '\sSleep\s{2}' -e '\s(?:Dark)?Wake\s{2}'
}

function idle-get {
    # output in seconds
    assert isDarwin @RET

    ioreg -c IOHIDSystem | sponge | awk '/HIDIdleTime/ {print $NF/1000000000; exit}'
}

function idle-p {
    local timeout="${1:-120}"

    (( "$(idle-get)" >= "$timeout" ))
}


function lastunlock-get {
    assert isDarwin @RET

    # Using lower precision helps a lot with performance
    # hyperfine --warmup 5 "log show --style syslog --predicate 'process == \"loginwindow\"' --debug --info --last 3h" "log show --style syslog --predicate 'process == \"loginwindow\"' --debug --info --last 30h"
    local precision="${1:-2h}" # can only spot the last unlock in this timeframe
    [[ "$precision" =~ '^\d+$' ]] && precision+=h

    unset date
    date="$(assert revaldbg command log show --style syslog --predicate 'process == "loginwindow"' --debug --info --last "$precision" | command rg "going inactive, create activity semaphore|releasing the activity semaphore" | tail -n1 |cut -c 1-31)" || {
        # This means the last login was before the precision set
        ec 9999998
        return 0
    }

    date="$date" fromnow || {
        ectrace "$(retcode 2>&1)"
        ec 9999999
        return 1
    }
}

function lastunlock-get-min {
    ec $(( $(lastunlock-get "$@") / 60 ))
}

function last-idle-get-epoch {
    last_idle_time_get
}

function last-idle-get-sec {
    ec $(( ${EPOCHREALTIME} - $(last-idle-get-epoch) ))
}

function last-idle-get-min {
    ec $(( $(last-idle-get-sec) / 60 ))
}
##
function load-average {
    #: 1 5 15 minutes

    if isDarwin ; then
        sysctl -n vm.loadavg | gtr -d '{}'
    else
        uptime | rget 'load average:\s*(.*)' | gtr -d ','
    fi | trim
}

function load5() {
    load-average | awk '{print $3}'
}
##
function lsport {
    #: @alt `command ss -tuln | grep $port`
    ##
    local p opts=()
    for p in "$@" ; do
        opts+=(-i :"$p")
    done
    ((${#opts})) && sudo lsof -n $opts[@]
}
##
#: The h-audio-default-get* helpers below are shared by the input and output
#: families; `audio_default_what` selects which (input|output). Use the
#: user-facing [agfi:audio-output-get] / [agfi:audio-input-get] wrappers, which
#: set it for you.
function h-audio-default-get-hs {
    # @darwinOnly
    ##
    ensure isDarwin @MRET

    local what="${audio_default_what:-output}"
    local whatC="$what" ; whatC[1]="${whatC[1]:u}"

    #: Fast (~8ms warm, ~140ms cold): IPC to the already-running Hammerspoon instance.
    hammerspoon -c "local d = hs.audiodevice.default${whatC}Device(); return d:name() .. \"\n\" .. d:transportType()"
}

function h-audio-default-get-system-profiler {
    # @darwinOnly
    ##
    ensure isDarwin @MRET

    local what="${audio_default_what:-output}"
    local whatC="$what" ; whatC[1]="${whatC[1]:u}"

    #: Slow (~200ms): spawns a fresh process and re-enumerates all audio devices,
    #: but works even when Hammerspoon is not running.
    #: Prints the name and Transport of the device block marked "Default <What> Device: Yes".
    #: (Each device is an 8-space-indented "Name:" line followed by 10-space-indented properties;
    #: the default device can appear twice, once as input and once as output.)
    #: The What token is passed through the environment so the perl script can stay
    #: single-quoted and keep its own $1/$b/$t unmangled by the shell.
    system_profiler SPAudioDataType |
        audio_default_what_c="$whatC" perl -0777 -ne 'while (m/^ {8}(.+?):\n\n((?: {10}.*\n)+)/mg) { my ($n, $b) = ($1, $2); if ($b =~ m/^ {10}Default $ENV{audio_default_what_c} Device: Yes/m) { my ($t) = $b =~ m/^ {10}Transport: (.*?)\s*$/m; print "$n\n$t\n"; exit 0 } } exit 1'
}

function h-audio-default-get {
    #: Gateway: tries the fast Hammerspoon helper, falls back to system_profiler.
    # @darwinOnly
    ##
    ensure isDarwin @MRET

    local out
    if out="$(h-audio-default-get-hs 2>/dev/null)" && [[ -n "$out" ]] ; then
        ec "$out"
    else
        h-audio-default-get-system-profiler
    fi
}
##
function audio-output-get-hs {
    : "outputs: default output device name \n transport (Bluetooth|Built-in|USB|DisplayPort|...)"
    # @darwinOnly
    ##
    audio_default_what=output h-audio-default-get-hs
}

function audio-output-get-system-profiler {
    : "outputs: default output device name \n transport (Bluetooth|Built-in|USB|DisplayPort|...)"
    # @darwinOnly
    ##
    audio_default_what=output h-audio-default-get-system-profiler
}

function audio-output-get {
    : "outputs: default output device name \n transport (Bluetooth|Built-in|USB|DisplayPort|...)"
    #: Gateway: tries the fast Hammerspoon helper, falls back to system_profiler.
    # @darwinOnly
    ##
    audio_default_what=output h-audio-default-get
}
##
function audio-input-get-hs {
    : "outputs: default input (microphone) device name \n transport (Bluetooth|Built-in|USB|...)"
    # @darwinOnly
    ##
    audio_default_what=input h-audio-default-get-hs
}

function audio-input-get-system-profiler {
    : "outputs: default input (microphone) device name \n transport (Bluetooth|Built-in|USB|...)"
    # @darwinOnly
    ##
    audio_default_what=input h-audio-default-get-system-profiler
}

function audio-input-get {
    : "outputs: default input (microphone) device name \n transport (Bluetooth|Built-in|USB|...)"
    #: Gateway: tries the fast Hammerspoon helper, falls back to system_profiler.
    #: Used by [agfi:ffmpeg-record]; see [[file:~/scripts/docs/stt-input-device.md]].
    # @darwinOnly
    ##
    audio_default_what=input h-audio-default-get
}
##
#: * Default audio input: listing, switching, and a glyph for the menubar
#:
#: <spec> is `builtin', `iphone', a device UID, or an exact device name. The
#: two kinds are resolved by transport, never by name: the built-in name is
#: model dependent, and the iPhone's name is personal. With the lid shut the
#: built-in microphone is disconnected in hardware and records digital silence.
#: See [[file:~/scripts/docs/audio-input-switch.md]].

function clamshell-p {
    : "returns 0 iff the laptop lid is closed"
    @darwinOnly
    ##
    local out
    out="$(ioreg -r -k AppleClamshellState -d 4)" @TRET

    [[ "$out" == *'"AppleClamshellState" = Yes'* ]]
}
aliasfn clamshell-is clamshell-p

function h-audio-input-kind-classify {
    : "<name> <transport> -> builtin|bluetooth|iphone|other

Pure classification, no lookup. Takes the transport in either spelling:
Hammerspoon's (Built-in, UNKNOWN) or system_profiler's JSON (builtin, unknown)."
    local name="${(L)1}" transport="${(L)${2//[^[:alnum:]]/}}"

    if [[ "$transport" == builtin ]] ; then
        ec builtin
    elif [[ "$transport" == bluetooth* ]] ; then
        ec bluetooth
    elif [[ "$transport" == unknown || "$name" == *iphone* ]] ; then
        #: A Continuity microphone. CoreAudio gives it a transport type that
        #: neither Hammerspoon nor system_profiler has a name for.
        ec iphone
    else
        ec other
    fi
}

function audio-input-devices-get-hs {
    : "outputs: one line per input device: name<TAB>transport<TAB>uid"
    @darwinOnly
    ##
    local out
    out="$(h-hammerspoon-eval 'return audioInputDevicesGet()')" @RET
    if [[ -z "$out" ]] ; then
        return 1
    fi

    ec "$out"
}

function audio-input-devices-get-system-profiler {
    : "outputs: one line per input device: name<TAB>transport<TAB>(no uid)"
    @darwinOnly
    ##
    #: Slow (~200ms), but needs no Hammerspoon.
    system_profiler -json SPAudioDataType 2>/dev/null |
        jq -r '.SPAudioDataType[]?._items[]?
            | select(.coreaudio_device_input)
            | [._name, ((.coreaudio_device_transport // "") | sub("^coreaudio_device_type_"; "")), ""]
            | @tsv'
}

function audio-input-devices-get {
    : "outputs: one line per input device: name<TAB>transport<TAB>uid
Gateway: tries the fast Hammerspoon helper, falls back to system_profiler."
    @darwinOnly
    ##
    local out
    if out="$(audio-input-devices-get-hs 2>/dev/null)" ; then
        ec "$out"
    else
        audio-input-devices-get-system-profiler
    fi
}

function audio-input-list {
    : "outputs: the name of each input device, one per line"
    @darwinOnly
    ##
    local out line
    out="$(audio-input-devices-get)" @RET

    for line in "${(@f)out}" ; do
        ec "${line%%$'\t'*}"
    done
}

function h-audio-input-resolve {
    : "<spec> -> the name of the first input device matching it"
    local spec="${1}"
    assert-args spec @RET

    local out
    out="$(audio-input-devices-get)" @RET

    local line fields name transport uid
    for line in "${(@f)out}" ; do
        fields=("${(@ps:\t:)line}")
        name="${fields[1]}" transport="${fields[2]}" uid="${fields[3]}"

        if [[ "$spec" == (builtin|iphone) ]] ; then
            if [[ "$(h-audio-input-kind-classify "$name" "$transport")" == "$spec" ]] ; then
                ec "$name"
                return 0
            fi
        elif [[ "$spec" == "$name" || ( -n "$uid" && "$spec" == "$uid" ) ]] ; then
            ec "$name"
            return 0
        fi
    done

    ecerr "$0: no input device matches: ${spec}"
    return 1
}

function audio-input-switch-hs {
    : "<name>: makes the exactly named input device the default, via Hammerspoon"
    @darwinOnly
    ##
    local name="${1}"
    assert-args name @RET

    if [[ "$name" == *']]'* ]] ; then
        #: It would close the Lua long string the name travels in.
        ecerr "$0: cannot pass a name containing ']]': ${name}"
        return 1
    fi

    local res
    res="$(h-hammerspoon-eval "return audioInputDefaultSetByName([[${name}]])")" @RET
    if [[ "$res" != ok ]] ; then
        ecerr "$0: ${name}: ${res:-no answer}"
        return 1
    fi
}

function audio-input-switch-sas {
    : "<name>: makes the exactly named input device the default, via SwitchAudioSource"
    @darwinOnly
    ##
    local name="${1}"
    assert-args name @RET

    ensure-dep-switchaudio @RET

    command SwitchAudioSource -t input -s "$name" >/dev/null @RET

    #: Re-read, rather than trust the exit status.
    local now
    now="$(command SwitchAudioSource -c -t input)" @TRET
    if [[ "$now" != "$name" ]] ; then
        ecerr "$0: asked for ${name}, the default input is ${now}"
        return 1
    fi
}

function audio-input-switch-darwin {
    : "<spec>: makes the matching input device the default
Gateway: Hammerspoon, falling back to SwitchAudioSource."
    @darwinOnly
    ##
    local clamshell_warn_p="${audio_input_switch_clamshell_warn_p:-y}"
    local keep_soft_mute_p="${audio_input_switch_keep_soft_mute_p:-n}"
    local spec="${1}"
    assert-args spec @RET

    local name
    name="$(h-audio-input-resolve "$spec")" @RET

    if ! bool "$keep_soft_mute_p" ; then
        #: An explicit switch ends any soft mute: switching BACK is no longer
        #: what a mute press should do, and the built-in mic gets its own mute
        #: state back (so an explicit `builtin' is live, as asked).
        h-audio-input-soft-mute-clear-if-set
    fi

    if ! audio-input-switch-hs "$name" 2>/dev/null ; then
        ecgray "$0: Hammerspoon could not switch, falling back to SwitchAudioSource."
        audio-input-switch-sas "$name" @RET
    fi

    ecgray "input: ${name}"
    if [[ "$spec" == builtin ]] && bool "$clamshell_warn_p" && clamshell-p ; then
        ecerr "$0: warning: the lid is closed, so the built-in microphone records silence."
    fi

    menubar-refresh @STRUE
}

function audio-input-switch {
    : "<spec>: makes the matching input device the default
spec: builtin, iphone, a device UID, or an exact device name (see audio-input-list)"
    if isDarwin ; then
        audio-input-switch-darwin "$@"
    else
        @NA
    fi
}

function audio-input-p {
    : "<spec>: returns 0 iff the default input device matches spec"
    @darwinOnly
    ##
    local spec="${1}"
    assert-args spec @RET

    local want now
    want="$(h-audio-input-resolve "$spec" 2>/dev/null)" || return 1
    now="$(audio-input-get)" @RET

    [[ "${now%%$'\n'*}" == "$want" ]]
}

function audio-input-toggle {
    : "<spec> [<other spec>=builtin]: switches to spec, or back to the other one if spec is already the default"
    local spec="${1}" other="${2:-builtin}"
    assert-args spec @RET

    if audio-input-p "$spec" ; then
        audio-input-switch "$other"
    else
        audio-input-switch "$spec"
    fi
}

aliasfn iphone-mic-on audio-input-switch iphone
aliasfn iphone-mic-off audio-input-switch builtin
aliasfn iphone-mic-p audio-input-p iphone
aliasfn iphone-mic-toggle audio-input-toggle iphone builtin
##
#: ** Soft mute, for a microphone with no mute control
#:
#: A Continuity (iPhone) microphone exposes no CoreAudio mute or volume at all,
#: so [agfi:input-volume-mute-toggle] cannot mute it. Soft mute switches to the
#: built-in microphone instead, with the built-in's own mute flag set. That
#: flag holds even if the lid is later opened, so a soft mute never turns into
#: a live built-in mic. The device to go back to, and the built-in's mute state
#: before we touched it, live in redis so every shell and the hotkey agree.

redis-defvar input_soft_mute_device
redis-defvar input_soft_mute_builtin_was_muted

function h-audio-input-muted-get-hs {
    : "<spec> -> true|false|nodevice|nomute, for a specific input device; fails iff Hammerspoon is unreachable"
    h-hammerspoon-eval "return audioInputMutedGet([[${1}]])" 2>/dev/null
}

function h-audio-input-muted-set-hs {
    : "<spec> <true|false> -> the state AFTER the write, or nodevice|nomute"
    h-hammerspoon-eval "return audioInputMutedSet([[${1}]], ${2})" 2>/dev/null
}

function audio-input-mute-control-p {
    : "returns 0 iff the default input device has a mute control"
    @darwinOnly
    ##
    local out
    out="$(audio-input-state-get-hs)" @RET

    local lines=("${(@f)out}")
    [[ "${lines[3]}" == (true|false) ]]
}

function h-audio-input-soft-mute-clear {
    : "forgets the soft mute and gives the built-in mic back the mute state it had before"
    local was
    was="$(input_soft_mute_builtin_was_muted_get)" || was=''

    if [[ "$was" == (true|false) ]] ; then
        h-audio-input-muted-set-hs builtin "$was" >/dev/null @STRUE
    fi

    input_soft_mute_device_del @STRUE
    input_soft_mute_builtin_was_muted_del @STRUE
}

function h-audio-input-soft-mute-clear-if-set {
    : "h-audio-input-soft-mute-clear, but only when a soft mute is recorded"
    local device
    device="$(input_soft_mute_device_get)" || device=''

    if [[ -n "$device" ]] ; then
        h-audio-input-soft-mute-clear
    fi
}

function audio-input-soft-mute-p {
    : "returns 0 iff a soft mute is in effect"
    @darwinOnly
    ##
    local device
    device="$(input_soft_mute_device_get)" || device=''
    if [[ -z "$device" ]] ; then
        return 1
    fi

    if audio-input-p builtin ; then
        return 0
    fi

    #: The default was moved by hand since, so the claim is stale.
    h-audio-input-soft-mute-clear
    return 1
}

function audio-input-soft-mute {
    : "mutes the default input by switching to the built-in mic, muted"
    @darwinOnly
    ##
    local out name
    out="$(audio-input-get)" @TRET
    name="${out%%$'\n'*}"

    local was
    was="$(h-audio-input-muted-get-hs builtin)" @RET
    if [[ "$was" != (true|false) ]] ; then
        ecerr "$0: cannot read the built-in mic's mute state: ${was:-no answer}"
        return 1
    fi

    #: Mute BEFORE switching, so the built-in mic is never the live default,
    #: not even for a moment.
    if [[ "$(h-audio-input-muted-set-hs builtin true)" != true ]] ; then
        ecerr "$0: could not mute the built-in mic"
        return 1
    fi

    input_soft_mute_builtin_was_muted_set "$was" @RET
    input_soft_mute_device_set "$name" @RET

    #: The closed-lid warning is noise here: the mic is muted on purpose.
    audio_input_switch_clamshell_warn_p=n audio_input_switch_keep_soft_mute_p=y audio-input-switch builtin @RET
}

function audio-input-soft-unmute {
    : "undoes audio-input-soft-mute: switches back, then restores the built-in mic's own mute state"
    @darwinOnly
    ##
    local device
    device="$(input_soft_mute_device_get)" || device=''
    if [[ -z "$device" ]] ; then
        ecerr "$0: no soft mute in effect"
        return 1
    fi

    #: Switch away FIRST, for the same reason soft mute mutes first.
    if ! audio_input_switch_keep_soft_mute_p=y audio-input-switch "$device" ; then
        ecerr "$0: cannot switch back to ${device}; staying on the built-in mic, muted"
        #: Forget the claim without restoring, so the built-in stays muted
        #: and the next press unmutes it the ordinary way.
        input_soft_mute_device_del @STRUE
        input_soft_mute_builtin_was_muted_del @STRUE
        return 1
    fi

    h-audio-input-soft-mute-clear
}
##
function audio-input-state-get-hs {
    : "outputs: name, transport, muted (true|false|nomute), volume (0-100|novolume), one per line"
    @darwinOnly
    ##
    local out
    out="$(h-hammerspoon-eval 'return audioInputStateGet()')" @RET
    if [[ -z "$out" ]] ; then
        return 1
    fi

    ec "$out"
}

function audio-input-state-get {
    : "outputs: name, transport, muted (true|false|nomute), volume (0-100|novolume), one per line
Gateway: Hammerspoon, falling back to system_profiler, which knows nothing about mute."
    @darwinOnly
    ##
    local out
    if out="$(audio-input-state-get-hs 2>/dev/null)" ; then
        ec "$out"
    else
        out="$(audio-input-get-system-profiler)" @RET
        ec "${out}"$'\nnomute\nnovolume'
    fi
}

function h-audio-input-kind {
    : "<name> <transport> -> the kind, with builtin split into builtin-clamshell while the lid is closed"
    local kind
    kind="$(h-audio-input-kind-classify "$@")" @RET

    if [[ "$kind" == builtin ]] && clamshell-p ; then
        kind=builtin-clamshell
    fi

    ec "$kind"
}

function audio-input-kind-get {
    : "outputs: builtin|builtin-clamshell|bluetooth|iphone|other|none, for the default input"
    @darwinOnly
    ##
    local out
    out="$(audio-input-state-get 2>/dev/null)" || out=''

    local lines=("${(@f)out}")
    if [[ -z "${lines[1]}" ]] ; then
        ec none
        return 0
    fi

    h-audio-input-kind "${lines[1]}" "${lines[2]}"
}

#: Overridable: set it after this file loads. `<kind>-muted' wins over `muted';
#: a muted built-in mic shows as muted even with the lid shut, since muting is the
#: deliberate state (and what a soft mute leaves behind).
#: iphone is the Apple logo, U+F8FF: private use, so it renders only in Apple fonts;
#: the phone emoji was too small and dark to read in the menubar.
if (( ! ${+audio_input_glyphs} )) ; then
    typeset -gA audio_input_glyphs=(
        builtin '💻'
        builtin-clamshell '🚫'
        bluetooth '🎧'
        iphone $'\uF8FF'
        other '🎤'
        none '❔'
        muted '🔇'
    )
fi

function audio-input-glyph-get {
    : "outputs: one symbol for the default input device and whether it is muted
Used by the xbar menubar plugin zshlang/menubar/date.sh."
    @darwinOnly
    ##
    local out
    out="$(audio-input-state-get 2>/dev/null)" || out=''

    local lines=("${(@f)out}")
    local name="${lines[1]}" transport="${lines[2]}" muted="${lines[3]}" volume="${lines[4]}"

    local kind=none
    if [[ -n "$name" ]] ; then
        kind="$(h-audio-input-kind "$name" "$transport")" @RET
    fi

    #: The osascript mute backend mutes an input by setting its volume to 0,
    #: so that counts as muted too.
    if [[ "$muted" == true || "$volume" == 0 ]] ; then
        ec "${audio_input_glyphs[${kind}-muted]:-${audio_input_glyphs[muted]}}"
    else
        ec "${audio_input_glyphs[$kind]}"
    fi
}

function menubar-refresh {
    : "[<plugin>=date.1m.bash]: asks xbar to rerun a plugin now rather than at its next interval"
    @darwinOnly
    ##
    local plugin="${1:-date.1m.bash}"

    command open -g "xbar://app.xbarapp.com/refreshPlugin?path=${plugin}"
}

function h-headphones-classify-p {
    : "returns 0 iff <name> <transport> describes a device likely worn in/on the ear

Pure classification, no lookup. Split out of [agfi:headphones-p] so that a caller
which already knows the device can decide without paying for a query -- notably
the Hammerspoon audio watcher, whose Lua callback hands us the new device and
must not be made to call back into Hammerspoon to re-discover it."
    local name="${(L)1}" transport="${2}"

    if [[ "$name" == *(headphone|earphone|headset|buds|pods)* ]] ; then
        #: Name says it's worn: the wired jack shows up as "External Headphones",
        #: and this also catches AirPods, Galaxy Buds, etc.
        return 0
    elif [[ "$transport" == "Bluetooth" && "$name" != *speaker* ]] ; then
        #: Heuristic: a Bluetooth audio device without "speaker" in its name is assumed worn
        #: (catches e.g. Sony WF/WH-1000XM*, Bose QC, whose names lack headphone-ish words).
        return 0
    else
        return 1
    fi
}

function headphones-p {
    : "returns 0 iff the default audio output is likely worn in/on the ear (headphones, earbuds, headset)

With no arguments, asks [agfi:audio-output-get]. Given <name> <transport>, skips
the lookup and classifies those directly."
    # @darwinOnly
    ##
    if (( $# >= 1 )) ; then
        h-headphones-classify-p "$@"
        return $?
    fi

    if isDarwin ; then
        local out
        out="$(audio-output-get)" @RET

        local lines=("${(@f)out}")
        h-headphones-classify-p "${lines[1]}" "${lines[2]}"
    else
        ecgray "$0: NA"
        return 1
    fi
}
aliasfn headphones-is headphones-p
aliasfn is-headphones headphones-is
##
function lsof-openfiles {
    local user="$(whoami)"
    ec "Total open files for $user: $(lsof -u "$user" | wc -l)"
    
    # sudo lsof -n | cut -f1 -d' ' | uniq -c | sort | tail -n30
    sudo lsof -n | cut -f1 -d' ' | gsort | guniq -c | gsort | tail -n30 # this merges different processes with the same name. idk what happens in the unmerged case exactly.
}
##
function displays-get-hs {
    : "outputs: the name of each attached display, one per line"
    # @darwinOnly
    ##
    ensure isDarwin @MRET

    #: Fast (~8ms warm): IPC to the already-running Hammerspoon instance.
    hammerspoon -c 'local t = {} ; for _, s in ipairs(hs.screen.allScreens()) do t[#t+1] = s:name() end ; return table.concat(t, "\n")'
}

function displays-get-system-profiler {
    : "outputs: the name of each attached display, one per line"
    # @darwinOnly
    ##
    ensure isDarwin @MRET

    #: Slow (~400ms): spawns a fresh process, but works when Hammerspoon is not running.
    #: The JSON form is used because the indentation-based plain-text output is brittle.
    system_profiler -json SPDisplaysDataType 2>/dev/null |
        jq -r '.SPDisplaysDataType[]? | .spdisplays_ndrvs[]? | ._name // empty'
}

function displays-get {
    : "outputs: the name of each attached display, one per line
Gateway: tries the fast Hammerspoon helper, falls back to system_profiler."
    # @darwinOnly
    ##
    ensure isDarwin @MRET

    local out
    if out="$(displays-get-hs 2>/dev/null)" && [[ -n "$out" ]] ; then
        ec "$out"
    else
        displays-get-system-profiler
    fi
}

function external-display-p {
    : "returns 0 iff at least one attached display is not the built-in panel"
    # @darwinOnly
    ##
    if ! isDarwin ; then
        ecgray "$0: NA"
        return 1
    fi

    #: @warn Do NOT count displays. In clamshell mode the lid is shut, the built-in
    #: panel is not reported at all, and the single remaining screen IS the external
    #: one -- so `(( #screens > 1 ))` is wrong exactly when we most need an answer.
    ##
    local out
    out="$(displays-get)" @RET

    local line name
    for line in "${(@f)out}" ; do
        name="${(L)line}"

        #: Built-in panels are named "Built-in Retina Display", "Built-in Liquid Retina
        #: XDR Display", or (on older Macs) "Color LCD". Same test as
        #: `ModalMode.screenIsInternal` in [[../../../../hammerspoon/modal-mode.lua]].
        if [[ -z "$name" ]] || [[ "$name" == *built-in* ]] || [[ "$name" == *'color lcd'* ]] ; then
            continue
        fi

        return 0
    done

    return 1
}
aliasfn external-display-is external-display-p
##
function screen-resolution() {
    : "outputs: width \n height"
    ensure isDarwin @MRET

    system_profiler SPDisplaysDataType | @opts r '$1'$'\n''$2' @ rget 'Resolution:\s*(\d+)\s*x\s*(\d+)'
}
function screen-width() {
    screen-resolution | ghead -n 1
}
function screen-height() {
    screen-resolution | gtail -n 1
}
##
function ps-parents-pid() {
    local pid="$1"
    assert-args pid @RET

    local ppid="$pid"
    while true ; do
        ppid="$(ps-parent-pid "$ppid")" @TRET
        if (( ppid == 1 )) ; then
            break
        fi
        ec "$ppid"
    done
}
function ps-parent-pid() {
    ps -fp "$1" | gtail -n1 | awkn 3
}
function ps-parents-print() {
    procs --tree --or "$@" | ansifold --width=$COLUMNS
    # you can add `--pager always --color always`, and remove ansifold. The colors don't play well with ansifold ...
}
function ffparents() {
    local pids
    pids=( ${(@f)"$(ffps "$@")"} ) @RET

    if (( $#pids >= 1 )) ; then
        reval-ec ps-parents-print $pids[@] @RET
    else
        return 1
    fi
}
function ps-parents-print1() {
    local pid="$1"
    assert-args pid @RET

    local ppids=( ${(@f)"$(ps-parents-pid "$pid")"} )

    ##
    assert command ps -fp "$pid" "$ppids[@]"
    ##
    # local i
    # for i in $ppids[@] ; do
    #     assert command ps -fp "$i"
    # done
    ##
}
function ffparents1() {
    local pid
    for pid in ${(@f)"$(ffps "$@")"} ; do
        reval-ec ps-parents-print "$pid" @RET
    done
}

##
function ps-zombies() {
    command ps axo pid=,stat= | gawk '$2~/^Z/ { print $1 }'
}
##
function cpu-get() {
    if isDarwin ; then
        # [[https://stackoverflow.com/questions/65259300/detect-apple-silicon-from-command-line][bash - Detect Apple Silicon from command line - Stack Overflow]]
        ##
        sysctl -n machdep.cpu.vendor
        sysctl -n machdep.cpu.brand_string # outputs 'Apple M1' on my M1 Mac Mini
    else
        # [[https://askubuntu.com/questions/806532/getting-information-about-cpu][cpuinfo - Getting information about CPU - Ask Ubuntu]]
        ##
        cat /proc/cpuinfo

        cat /proc/cpuinfo | rg 'model name'
    fi

    reval-ec uname -m # 'arm64' on M1
    # @warn M1 users can run Terminal in Rosetta mode. In this case "uname -m" returns "x86_64".
}
##
function firmware-version-get-darwin {
    system_profiler SPHardwareDataType | rg --smart-case firmware
}
##
