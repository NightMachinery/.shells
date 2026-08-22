crondisable() {
	local user="${1:-$(whoami)}"
	local cronpath="/tmp/$user.cron.tmp"
	test -e "$cronpath" && {
		ecerr "There is already a disabled crontab at $cronpath. Remove that manually if you want to proceed."
		return 1
	}
	crontab -l -u $user > "$cronpath"
	crontab -r -u $user
}
cronenable() {
	local user="${1:-$(whoami)}"
	local cronpath="/tmp/$user.cron.tmp"
	test -e "$cronpath" || {
		ecerr "No disabled cron at $cronpath"
		return 1
	}
	crontab -u $user "$cronpath"
	mv "$cronpath" "${cronpath}.bak"
}
##
#: * @alt [[https://www.hammerspoon.org/docs/hs.audiodevice.html][Hammerspoon docs: hs.audiodevice]]

function volume-mute-p-hs {
  local what="${volume_what:-output}"

  local whatC="$what"
  whatC[1]="${whatC[1]:u}"

  local res
  res="$(revaldbg hammerspoon -c "hs.audiodevice.default${whatC}Device():${what}Muted()")" @TRET
  dact typ res

  if [[ "$res" == true ]] ; then
    return 0
  else
    return 1
  fi
}

function volume-get-hs {
  local what="${volume_what:-output}"

  local whatC="$what"
  whatC[1]="${whatC[1]:u}"

  revaldbg hammerspoon -c "hs.audiodevice.default${whatC}Device():${what}Volume()"
}

function volume-mute-hs {
  local v="${volume_what_v:-true}"

  local what="${volume_what:-output}"

  local whatC="$what"
  whatC[1]="${whatC[1]:u}"

  if [[ "$(revaldbg hammerspoon -c "hs.audiodevice.default${whatC}Device():set${whatC}Muted($v)")" == true ]] ; then
    return 0
  else
    return 1
  fi
}
aliasfn volume-unmute-hs volume_what_v=false volume-mute-hs

function volume-mute-toggle-hs {
  if volume-mute-p-hs ; then
    volume-unmute-hs
  else
    volume-mute-hs
  fi
}

function volume-inc-hs {
  local amount="${1:-5}"
  local what="${volume_what:-output}"

  local whatC="$what"
  whatC[1]="${whatC[1]:u}"

  revaldbg hammerspoon -c "volumeInc(${amount}, hs.audiodevice.default${whatC}Device())"
}

function volume-dec-hs {
    local amount="${1:-5}"

    volume-inc-hs $((amount*-1))
}

function volume-mute-p {
  local what="${volume_what:-output}"

  if isDarwin ; then
    if [[ "$what" == input ]] ; then
        local vol
        vol="$(volume-get)" @TRET

        if (( vol == 0 )) ; then
            return 0
        else
            return 1
        fi
    else
        local res
        res="$(osascript -e "output muted of (get volume settings)")" @TRET
        if [[ "$res" == true ]] ; then
          return 0
        else
          return 1
        fi
    fi
  else
    @NA
  fi
}

function volume-mute {
  local what="${volume_what:-output}"

  if isDarwin ; then
    if [[ "$what" == input ]] ; then
      local vol
      vol="$(volume-get)" @TRET
      input_volume_cached_set "$vol" @STRUE

      volume-set 0
    else
      osascript -e "set volume with ${what} muted"
    fi
  else
    @NA
  fi
}

redis-defvar input_volume_cached
function volume-unmute {
  local what="${volume_what:-output}"

  if isDarwin ; then
    if [[ "$what" == input ]] ; then
      volume-set "${$(input_volume_cached_get):-75}"
    else
      osascript -e "set volume without ${what} muted"
    fi
  else
    @NA
  fi
}

function volume-mute-toggle {
  local what="${volume_what:-output}"

  local alert_dur=0.5

  if volume-mute-p ; then
    volume-unmute

    awaysh-fast alert "$what volume UNMUTED"
  else
    volume-mute

    awaysh-fast alert "$what volume muted"
  fi
}

function input-volume-mute-toggle {
    {
      ##
      # with-input-volume volume-mute-toggle @TRET
      ##
      with-input-volume volume-mute-toggle-hs @TRET
      ##
    } always {
      local alert_dur=2
      ##
      if with-input-volume volume-mute-p-hs  ; then
        alert "input muted"
      else
        alert "INPUT UNMUTED"
      fi
      ##
      # awaysh-fast alert "input-volume: $(with-input-volume volume-get)"
    }
}
aliasfn microphone-mute-toggle input-volume-mute-toggle

function volget {
  : "0-100"

  local what="${volume_what:-output}"

  if isDarwin ; then
	  osascript -e "set ovol to ${what} volume of (get volume settings)"
  else
    @NA
  fi
}

function volset {
	: "0-100"

    local v="${1}"
    assert-args v @RET
    local what="${volume_what:-output}"

    if isDarwin ; then
        osascript -e "set volume ${what} volume $v"

        # alert_dur=0.5 awaysh-fast alert "${what}-volume: $(volume-get)"
        # notif-kitty "${what}-volume" "$(volume-get)"
    else
        @NA
    fi
}
aliasfn setv volset
aliasfn set-volume volset
aliasfn volume-set volset
aliasfn get-volume volget
aliasfn volume-get volget
aliasfn getv volget
aliasfn with-input-volume volume_what=input

function volume-inc {
  local amount="${1:-5}"

  local vol
  vol="$(volume-get)" @TRET
  reval-ec volume-set $(( vol+amount ))
}

function volume-dec {
  local amount="${1:-5}"

  volume-inc $((amount*-1))
}
##
function mute-external_() {
	: "You probably want to use mute-external which calls this in a loop.
Usage: mute-external_ [<headphone-volume-from-100>=1]"
	: "Note that headphones-is takes ~10ms via Hammerspoon (~200ms if it falls back to system_profiler)"

	{
		local lev="${1:-1}"
		if headphones-is ; then
			volset "$lev"
		else
			volset 0
			display-gray-on
		fi
	}
}
function mute-external() {
	local lev="${1:-1}"

	{ lo_s=0 loop mute-external_ "$lev" }  always { volset 0 }
}
##
function resetdns-darwin() {
	sudo dscacheutil -flushcache
}
##
function logout() {
	# @darwinonly
	logout-darwin "$@"
}
function logout-darwin() {
	sudo launchctl bootout user/$(id -u "${1:$(whoami)}")
}
function logout-darwin2() {
	osascript -e 'tell application \"System Events\" to log out'
}
##
function screen-gamma-set-dur {
	#: @darwinonly, see https://stackoverflow.com/questions/3552037/how-to-programmatically-invert-screen-colors-in-linux
    #: @alt [[https://www.hammerspoon.org/docs/hs.screen.html#setGamma]]
    #: @alt `hammerspoon -c "hs.screen.setInvertedPolarity(true)"`
    ##
	local dur="${1:-3}" # duration in seconds
	local t1="${2:-1}"
	local t2="${3:-0}"

	invert_darwin.c "$dur" "$t1" "$t2"
}
aliasfn screen-invert-dur screen-gamma-set-dur
### @darwinonly
#: * @alt [[https://www.hammerspoon.org/docs/hs.screen.html#setForceToGray]]
#: ** https://github.com/Hammerspoon/hammerspoon/issues/3329
#: * [[https://github.com/rkbhochalya/grayscale-mode][rkbhochalya/grayscale-mode: A macOS menu bar app that gives you more control over Grayscale Mode.]]
function display-gray-toggle-v1 {
  screen_color_filter_color=gray screen_color_filter_enable_p='toggle' screen_color_filter.py
}

function display-gray-on-v1 {
  screen_color_filter_color=gray screen_color_filter_enable_p='y' screen_color_filter.py
}

function display-gray-off-v1 {
  screen_color_filter_color=gray screen_color_filter_enable_p='n' screen_color_filter.py
}

function display-gray-is-v1 {
  screen_color_filter_color=gray screen_color_filter_enable_p='return' screen_color_filter.py
}

function display-gray-is-v0 {
	[[ "$(gray_darwin.c s)" == "Grayscale is now: 1" ]]
}

function display-gray-toggle-v0 {
	gray_darwin.c
}

function display-gray-off-v0 {
    #: no longer works
	gray_darwin.c n
}

function display-gray-on-v0 {
    #: no longer works
	gray_darwin.c y
}
##
SCREEN_GRAY_MARKER='SCREEN_GRAY_MARKER'
function display-gray-is {
    sout pgrep "$SCREEN_GRAY_MARKER"
}

function display-gray-toggle {
    if display-gray-is ; then
        display-gray-off
    else
        display-gray-on
    fi
}

function display-gray-off {
    kill-marker "$SCREEN_GRAY_MARKER" || true
}

function display-gray-on {
    display-gray-is && return 0

    awaysh-bnamed "$SCREEN_GRAY_MARKER" screen-gamma-set-dur 99999999 0.4 1
}
##
#: Brightness is split into per-backend helpers plus gateways, because no single
#: API covers every panel:
#:   internal  nriley `brightness`, IOKit. Built-in panels only; it cannot even
#:             read an external one ("unable to get brightness of display 0x2").
#:   ddc       `m1ddc`, DDC/CI over USB-C/DP-Alt-Mode. External panels only,
#:             Apple Silicon only.
#: The public API stays on the 0..1 float scale; the DDC 0..100 luminance scale
#: is an implementation detail of the `-ddc` helpers.
##
brightness_ddc_max="${brightness_ddc_max:-100}"
#: Denominator for the 0..1 <-> luminance conversion. m1ddc can report a panel's
#: own `max luminance`, but that is an extra DDC round trip on every single call
#: and virtually every monitor answers 100. See [agfi:brightness-ddc-max] to
#: check yours, and pin this variable if it differs.

brightness_ddc_retries="${brightness_ddc_retries:-3}"
#: How many times [agfi:brightness-get-ddc] will re-read a luminance that came
#: back out of range. See the comment there.

function brightness-displays-internal {
    : "Every display, as CoreGraphics sees it.
Output (TSV): index, main|-, built-in|external, CGDirectDisplayID (decimal)"
    # @darwinOnly
    ##
    assert isDarwin @MRET

    #: `brightness` exits 0 even when it cannot read a display, and writes that
    #: complaint to stderr; the listing on stdout is still good. Its per-display
    #: value lines carry no ", ID 0x", so the regex skips them.
    local out
    out="$(command brightness -l 2>/dev/null)" @TRET

    local line kind main
    for line in "${(@f)out}" ; do
        [[ "$line" =~ '^display ([0-9]+): (.*), ID 0x([0-9a-fA-F]+)$' ]] || continue

        if [[ "$match[2]" == *built-in* ]] ; then
            kind=built-in
        else
            kind=external
        fi

        if [[ "$match[2]" == *main* ]] ; then
            main=main
        else
            main='-'
        fi

        printf '%s\t%s\t%s\t%s\n' "$match[1]" "$main" "$kind" "$((16#$match[3]))"
    done
}

function brightness-displays-ddc {
    : "DDC/CI-capable displays.
Output (TSV): m1ddc display number, CGDirectDisplayID (decimal), product name"
    # @darwinOnly @appleSiliconOnly
    ##
    assert isAppleSilicon @MRET
    #: Deliberately does NOT auto-install: this runs on every brightness key
    #: repeat and on [agfi:brightness-auto-loop]'s 3s cycle. Use
    #: [agfi:ensure-dep-m1ddc] to install it.
    assert isdefined-cmd m1ddc @MRET

    local out
    out="$(command m1ddc display list detailed 2>/dev/null)" @TRET

    local line n='' name='' id=''
    for line in "${(@f)out}" ; do
        if [[ "$line" =~ '^\[([0-9]+)\] (.*) \(' ]] ; then
            #: A new record starts; flush the previous one.
            [[ -n "$n" && -n "$id" ]] && printf '%s\t%s\t%s\n' "$n" "$id" "$name"

            n="$match[1]" name="$match[2]" id=''
        elif [[ "$line" =~ '^[[:space:]]*-[[:space:]]*Display ID:[[:space:]]+([0-9]+)' ]] ; then
            id="$match[1]"
        fi
    done
    [[ -n "$n" && -n "$id" ]] && printf '%s\t%s\t%s\n' "$n" "$id" "$name"

    return 0
}

function brightness-displays {
    : "Every display with the backend that can drive its brightness.
Output (TSV): index, backend (internal|ddc|none), backend-local id, main|-,
built-in|external, name, CGDirectDisplayID

The two backends number displays differently, so they are joined on the
CGDirectDisplayID: \`brightness -l\` prints it as 'ID 0x2', m1ddc as
'Display ID: 2'."
    # @darwinOnly
    ##
    assert isDarwin @MRET

    local internal
    internal="$(brightness-displays-internal)" @TRET

    #: DDC is optional. Intel, no m1ddc, or no DDC-capable panel all just mean
    #: the external displays end up with backend `none`.
    local ddc
    ddc="$(brightness-displays-ddc 2>/dev/null)" || ddc=''

    local line dline backend local_id name
    local -a f d
    for line in "${(@f)internal}" ; do
        [[ -n "$line" ]] || continue
        f=("${(@ps:\t:)line}")
        #: f: 1 index  2 main|-  3 built-in|external  4 display-id

        backend=none local_id='' name=''
        if [[ "$f[3]" == built-in ]] ; then
            backend=internal local_id="$f[1]" name='Built-in'
        else
            for dline in "${(@f)ddc}" ; do
                [[ -n "$dline" ]] || continue
                d=("${(@ps:\t:)dline}")
                #: d: 1 m1ddc number  2 display-id  3 name

                if [[ "$d[2]" == "$f[4]" ]] ; then
                    backend=ddc local_id="$d[1]" name="$d[3]"
                    break
                fi
            done
            : ${name:='External'}
        fi

        #: The display id is carried through as the last field: it is what
        #: `hs.screen:id()` returns, so [agfi:h-display-black-gamma] can find
        #: the same screen without re-deriving anything.
        printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\n' "$f[1]" "$backend" "$local_id" "$f[2]" "$f[3]" "$name" "$f[4]"
    done
}

function h-brightness-select {
    : "usage: h-brightness-select [<selector>]
Resolves a selector to the matching [agfi:brightness-displays] lines.

  main       (default)  the display macOS considers main
  all                   every display
  internal, built-in    built-in panel(s)
  external, ddc         external panel(s)
  <integer>             index, as listed by [agfi:brightness-displays]
  <anything else>       regex, matched against the display name"
    ##
    local sel="${1:-${brightness_display:-main}}"

    local all
    all="$(brightness-displays)" @TRET

    local line
    local -a f out=()
    for line in "${(@f)all}" ; do
        [[ -n "$line" ]] || continue
        f=("${(@ps:\t:)line}")
        #: f: 1 index  2 backend  3 local-id  4 main|-  5 built-in|external  6 name  7 display-id

        case "$sel" in
            all) out+=("$line") ;;
            main) [[ "$f[4]" == main ]] && out+=("$line") ;;
            internal|built-in) [[ "$f[5]" == built-in ]] && out+=("$line") ;;
            external|ddc) [[ "$f[5]" == external ]] && out+=("$line") ;;
            #: A bare integer is an index; anything else is a name regex.
            <->) [[ "$f[1]" == "$sel" ]] && out+=("$line") ;;
            *) [[ "$f[6]" =~ "$sel" ]] && out+=("$line") ;;
        esac
    done

    if (( ! $#out )) ; then
        ecerr "$0: no display matched selector $(gquote-sq "$sel")"
        return 1
    fi

    printf '%s\n' "$out[@]"
}

function h-brightness-dispatch {
    : "usage: h-brightness-dispatch <selector> <get|set|inc> [<value>]
Runs brightness-<op>-<backend> once per selected display, so each panel is
driven through whichever API can actually reach it."
    ##
    local sel="$1" op="$2" ; shift 2
    assert-args sel op @RET

    local lines
    lines="$(h-brightness-select "$sel")" @TRET

    local line ret=0
    local -a f
    for line in "${(@f)lines}" ; do
        [[ -n "$line" ]] || continue
        f=("${(@ps:\t:)line}")

        if [[ "$f[2]" == none ]] ; then
            ecerr "$0: display $f[1] ($f[6]) has no brightness backend; for an external panel on Apple Silicon, run: ensure-dep-m1ddc"
            ret=1
            continue
        fi

        #: get takes just the id; set and inc take a value first.
        "brightness-${op}-$f[2]" "$@" "$f[3]" || ret=$?
    done

    return $ret
}
##
function brightness-get-internal {
    : "usage: brightness-get-internal [<display-index>]"
    # @darwinOnly
    ##
    local i="${1:-0}"

    if isDarwin ; then
        local out
        out="$(command brightness -l 2>/dev/null)" @TRET

        if [[ "$out" =~ "display ${i}: brightness (\S+)" ]] ; then
            ec "$match[1]"
        else
            ecerr "$0: could not read the brightness of display ${i}"
            return 1
        fi
    else
        @NA
    fi
}

function brightness-set-internal {
    : "usage: brightness-set-internal <0..1> [<display-index>]"
    # @darwinOnly
    ##
    local v="$1" i="${2:-0}"
    assert-args v @RET

    if isDarwin ; then
        command brightness -d "$i" "$v"
    else
        @NA
    fi
}

function brightness-inc-internal {
    : "usage: brightness-inc-internal [<delta>] [<display-index>]"
    ##
    local inc="${1:-0.01}" i="${2:-0}"

    local curr
    curr="$(brightness-get-internal "$i")" @TRET

    local n=$((curr+inc))
    if (( n > 1 )) ; then
        n=1
    elif (( n < 0 )) ; then
        n=0
    fi

    brightness-set-internal "$n" "$i"
}
##
function brightness-ddc-max {
    : "usage: brightness-ddc-max [<m1ddc-display>]
The panel's own maximum luminance. Costs a DDC round trip, which is why the
conversion helpers read \$brightness_ddc_max instead."
    # @appleSiliconOnly
    ##
    local i="${1:-1}"

    assert isAppleSilicon @MRET
    ensure-dep-m1ddc @RET

    command m1ddc display "$i" max luminance
}

function brightness-get-ddc {
    : "usage: brightness-get-ddc [<m1ddc-display>]"
    # @appleSiliconOnly
    ##
    local i="${1:-1}"

    assert isAppleSilicon @MRET
    ensure-dep-m1ddc @RET

    local max="${brightness_ddc_max:-100}"
    local raw
    raw="$(h-m1ddc-get "$i" luminance)" @TRET

    #: %f, to match the format nriley `brightness` prints. Bare zsh float
    #: arithmetic would give '0.34999999999999998' here.
    printf '%f\n' $((raw*1.0/max))
}

function h-m1ddc-get {
    : "usage: h-m1ddc-get <m1ddc-display> <luminance|contrast|...>
One validated DDC reading, as a raw integer."
    ##
    local i="$1" attr="$2"
    assert-args i attr @RET

    #: DDC reads come back corrupt now and then — measured here at roughly 1 in
    #: 13 over a USB-C hub, returning '-7' for a panel pinned at 50. m1ddc still
    #: exits 0 on those, so the exit code tells us nothing and the only usable
    #: signal is the value being out of range. Re-read when it is.
    #:
    #: Writes are not affected, and neither is `chg` (m1ddc does that read
    #: internally): 40 consecutive +1/-1 round trips landed back on exactly 50.
    local max="${brightness_ddc_max:-100}"
    local -i tries="${brightness_ddc_retries:-3}"
    local raw='' n=0
    while (( n < tries )) ; do
        (( n++ ))

        raw="$(command m1ddc display "$i" get "$attr" 2>/dev/null)" || continue

        #: `<->` matches non-negative integers, so a corrupt '-7' fails here.
        if [[ "$raw" == <-> ]] && (( raw <= max )) ; then
            ec "$raw"
            return 0
        fi
    done

    ecerr "$0: display ${i} gave no valid ${attr} in ${tries} tries (last: $(gquote-sq "$raw"))"
    return 1
}

function contrast-get-ddc {
    : "usage: contrast-get-ddc [<m1ddc-display>]
Contrast as 0..1, on the same scale as the brightness functions."
    # @appleSiliconOnly
    ##
    local i="${1:-1}"

    assert isAppleSilicon @MRET
    ensure-dep-m1ddc @RET

    local max="${brightness_ddc_max:-100}"
    local raw
    raw="$(h-m1ddc-get "$i" contrast)" @TRET

    printf '%f\n' $((raw*1.0/max))
}

function contrast-set-ddc {
    : "usage: contrast-set-ddc <0..1> [<m1ddc-display>]"
    # @appleSiliconOnly
    ##
    local v="$1" i="${2:-1}"
    assert-args v @RET

    assert isAppleSilicon @MRET
    ensure-dep-m1ddc @RET

    local max="${brightness_ddc_max:-100}"
    local n
    n="$(printf '%.0f' $((v*max)))"
    if (( n > max )) ; then
        n=$max
    elif (( n < 0 )) ; then
        n=0
    fi

    silent command m1ddc display "$i" set contrast "$n"
}

function brightness-set-ddc {
    : "usage: brightness-set-ddc <0..1> [<m1ddc-display>]"
    # @appleSiliconOnly
    ##
    local v="$1" i="${2:-1}"
    assert-args v @RET

    assert isAppleSilicon @MRET
    ensure-dep-m1ddc @RET

    local max="${brightness_ddc_max:-100}"
    local n
    n="$(printf '%.0f' $((v*max)))"
    if (( n > max )) ; then
        n=$max
    elif (( n < 0 )) ; then
        n=0
    fi

    silent command m1ddc display "$i" set luminance "$n"
}

function brightness-inc-ddc {
    : "usage: brightness-inc-ddc [<delta>] [<m1ddc-display>]
Uses m1ddc's own \`chg\`, so this is one DDC round trip rather than the
get-then-set the internal backend needs. The hyper+F1/F2 key repeat comes
through here, and some panels misbehave under rapid interleaved reads/writes."
    # @appleSiliconOnly
    ##
    local inc="${1:-0.01}" i="${2:-1}"

    assert isAppleSilicon @MRET
    ensure-dep-m1ddc @RET

    local max="${brightness_ddc_max:-100}"
    local n
    n="$(printf '%.0f' $((inc*max)))"

    #: printf rounds a small delta to 0 (or to '-0'); skip the pointless write.
    (( n == 0 )) && return 0

    silent command m1ddc display "$i" chg luminance "$n"
}
##
function brightness-get {
    : "usage: brightness-get [<selector>]
Brightness as 0..1, one line per selected display.
Selectors: see [agfi:h-brightness-select]."
    ##
    local sel="${1:-${brightness_display:-main}}"

    h-brightness-dispatch "$sel" get
}

function brightness-set {
    : "usage: brightness-set <0..1> [<selector>]"
    ##
    local v="$1" sel="${2:-${brightness_display:-main}}"
    assert-args v @RET

    h-brightness-dispatch "$sel" set "$v"
}

function brightness-inc {
    : "usage: brightness-inc [<delta>] [<selector>]"
    ##
    local inc="${1:-0.01}" sel="${2:-${brightness_display:-main}}"

    h-brightness-dispatch "$sel" inc "$inc"
}

function brightness-dec {
    : "usage: brightness-dec [<delta>] [<selector>]"
    ##
    local amount="${1:-0.01}" sel="${2:-${brightness_display:-main}}"

    brightness-inc $((amount*-1)) "$sel"
}
##
#: Blanking a display takes a different route per panel, because "brightness 0"
#: does not mean the same thing on both:
#:   built-in  IOKit brightness 0 really does cut the backlight; the panel goes
#:             black and that is all it takes.
#:   external  DDC luminance 0 is only the *dimmest* backlight setting, not off.
#:             It stays visibly lit, so the image is blacked in software with a
#:             zero gamma table and the DDC levels are floored underneath it.
#: Neither one powers the monitor down — see `display-off` for that.
##
redis-defvar display_black_saved
#: TSV lines: display-id, backend, backend-local id, brightness, contrast,
#: gamma-applied. Doubles as the "is anything blanked" flag for
#: [agfi:display-black-p].

function h-display-black-gamma {
    : "usage: h-display-black-gamma <display-id> on|off
Zeroes one screen's gamma table. Matched on the CGDirectDisplayID, which is
exactly what \`hs.screen:id()\` returns."
    ##
    local id="$1" mode="$2"
    assert-args id mode @RET

    if [[ "$mode" == on ]] ; then
        silent hammerspoon -c "for _, s in ipairs(hs.screen.allScreens()) do if s:id() == ${id} then s:setGamma({red=0,green=0,blue=0},{red=0,green=0,blue=0}) end end"
    else
        #: Global, and deliberately so. Per-screen we could only force gamma
        #: back to identity, which would flatten a real calibration or Night
        #: Shift; `restoreGamma` puts back what macOS actually had. Blanking is
        #: the only thing here that touches gamma, so restoring every screen
        #: cannot lose anything.
        silent hammerspoon -c 'hs.screen.restoreGamma()'
    fi
}

function display-black-on {
    : "usage: display-black-on [<selector>]
Blanks the selected display(s), remembering their levels so
[agfi:display-black-off] can put them back. Selectors: see
[agfi:h-brightness-select]."
    ##
    local sel="${1:-${brightness_display:-main}}"

    local lines
    lines="$(h-brightness-select "$sel")" @TRET

    local line b c g ret=0
    local -a f saved=()
    for line in "${(@f)lines}" ; do
        [[ -n "$line" ]] || continue
        f=("${(@ps:\t:)line}")
        #: f: 1 index  2 backend  3 local-id  4 main|-  5 built-in|external  6 name  7 display-id

        b='-' c='-' g=n
        if [[ "$f[2]" != none ]] ; then
            b="$(brightness-get-$f[2] "$f[3]" 2>/dev/null)" || b='-'
            brightness-set-$f[2] 0 "$f[3]" || ret=$?
        fi

        if [[ "$f[2]" == ddc ]] ; then
            c="$(contrast-get-ddc "$f[3]" 2>/dev/null)" || c='-'
            contrast-set-ddc 0 "$f[3]" || ret=$?
        fi

        #: Only external panels need the software blackout; a built-in one is
        #: already dark from the backlight being off, and leaving its gamma
        #: alone keeps the working display's colour untouched.
        if [[ "$f[5]" == external ]] ; then
            h-display-black-gamma "$f[7]" on || ret=$?
            g=y
        fi

        saved+=("$f[7]"$'\t'"$f[2]"$'\t'"$f[3]"$'\t'"$b"$'\t'"$c"$'\t'"$g")
    done

    (( $#saved )) && display_black_saved_set "${(pj:\n:)saved}"

    return $ret
}

function display-black-off {
    : "usage: display-black-off [<selector>]
Undoes [agfi:display-black-on], restoring gamma and the remembered levels. With
no selector it puts back everything that was blanked; with one, only the
displays it matches. Selectors: see [agfi:h-brightness-select]."
    ##
    local sel="$1"

    #: Unconditional, and before anything else, so running this bare is always
    #: the way out of a screen left black.
    h-display-black-gamma 0 off

    local saved
    saved="$(display_black_saved_get)" || saved=''
    if [[ -z "$saved" ]] ; then
        return 0
    fi

    #: No selector means everything. Otherwise collect the display ids it
    #: resolves to, and put back only those.
    local -a wanted=()
    if [[ -n "$sel" ]] ; then
        local sline
        local -a sf
        for sline in "${(@f)$(h-brightness-select "$sel")}" ; do
            [[ -n "$sline" ]] || continue
            sf=("${(@ps:\t:)sline}")
            wanted+=("$sf[7]")
        done
    fi

    local line ret=0
    local -a f keep=()
    for line in "${(@f)saved}" ; do
        [[ -n "$line" ]] || continue
        f=("${(@ps:\t:)line}")
        #: f: 1 display-id  2 backend  3 local-id  4 brightness  5 contrast  6 gamma-applied

        if (( $#wanted )) && (( ! $wanted[(Ie)$f[1]] )) ; then
            #: Not selected, so it stays blanked — but the gamma restore above
            #: was global, so put its blackout back.
            keep+=("$line")
            [[ "$f[6]" == y ]] && h-display-black-gamma "$f[1]" on
            continue
        fi

        if [[ "$f[4]" != '-' && "$f[2]" != none ]] ; then
            brightness-set-$f[2] "$f[4]" "$f[3]" || ret=$?
        fi

        if [[ "$f[5]" != '-' ]] ; then
            contrast-set-ddc "$f[5]" "$f[3]" || ret=$?
        fi
    done

    if (( $#keep )) ; then
        display_black_saved_set "${(pj:\n:)keep}"
    else
        display_black_saved_del
    fi

    return $ret
}

function display-black-p {
    : "Whether anything is currently blanked."
    ##
    test -n "$(display_black_saved_get)"
}

function display-black-toggle {
    : "usage: display-black-toggle [<selector>]"
    ##
    local sel="${1:-${brightness_display:-main}}"

    if display-black-p ; then
        display-black-off "$sel"
    else
        display-black-on "$sel"
    fi
}
##
#: Selector-suffixed conveniences: display-black-on-all, display-black-toggle-external, ...
#:
#: Only for this family. The brightness getters must NOT get the same treatment:
#: `brightness-get-internal` and `brightness-get-ddc` already exist as backend
#: helpers taking a display index, and `brightness-set-all 0.5` would put the
#: selector where the value goes.
#:
#: h_aliasfn rather than the `aliasfn` alias, since aliases are resolved at parse
#: time and this is built in a loop.
for h_db_fn in display-black-on display-black-off display-black-toggle ; do
    for h_db_sel in main all internal external ; do
        h_aliasfn "${h_db_fn}-${h_db_sel}" "${h_db_fn}" "${h_db_sel}"
    done
done
unset h_db_fn h_db_sel
##
function brightness-screen {
	local mode="${1:-1}"

	local screen="$(gmktemp --suffix .png)"
	@opts silent y s 0 @ screenshot-all "$screen"
	local screen_brightness="$(detect_brightness_mode=$mode detect_brightness.py $screen)"
	command rm $screen
	ec $screen_brightness
}

function brightness-auto {
	local darkest="${1:-0.5}"

	local screen_brightness="$(brightness-screen)"
	dvar screen_brightness

	local to=$(( (1 - screen_brightness)*(1 - darkest) + darkest ))
	if (( to > 1 )) ; then
		to=1
	fi
	dvar to
	brightness-set "${to}"
}

function brightness-auto-loop {
	#: brightness-auto takes ~0.5s
	serr @opts s "${lo_s:-3}" @ loop brightness-auto "${@:-0.3}"
}
@opts-setprefix brightness-auto-loop lo
##
function open_command {
  # forked from OMZ
  local open_cmd

  # define the open command
  case "$OSTYPE" in
    darwin*)  open_cmd='open' ;;
    cygwin*)  open_cmd='cygstart' ;;
    linux*)   open_cmd='xdg-open' ;;
    msys*)    open_cmd='start ""' ;;
    *)        echo "Platform $OSTYPE not supported"
              return 1
              ;;
  esac

  # don't use nohup on OSX
  if [[ "$OSTYPE" == darwin* ]]; then
    $open_cmd "$@" &>/dev/null
  else
    awaysh $open_cmd "$@"
  fi
}
##
function location-get-darwin {
	ensure isDarwin @MRET

	##
  # CoreLocationCLI -json | jq .
  ##
  hammerspoon -c 'printLocation()' |
    rg -v '^-- ' |
    jq .
  #: Sometimes outputs '-- Loading extension: inspect'
  ##
}

function location-get {
  if isDarwin ; then
	  location-get-darwin | jqm '.latitude, .longitude'
  else
    @NA
  fi
}
##
