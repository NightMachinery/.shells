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
	: "Note that headphones-is is expensive and takes ~0.3 seconds"

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
function brightness-get {
	if isDarwin ; then # @darwinonly
		if [[ "$(brightness -l)" =~ 'display 0: brightness (\S+)' ]] ; then
			ec "$match[1]"
		else
			return 1
		fi
	else
		return 1
	fi
}

function brightness-set {
	local i="$1"

	if isDarwin ; then # @darwinonly
		brightness "$i"
	else
		return 1
	fi
}

function brightness-inc {
    local inc="${1:-0.025}"

    local curr
    curr="$(brightness-get)" @TRET

    local n=$((curr+inc))
    if (( n > 1 )) ; then
        n=1
    elif (( n < 0 )) ; then
        n=0
    fi

    brightness-set "$n"
}

function brightness-dec {
    local amount="${1:-0.025}"
    brightness-inc $((amount*-1))
}
##
function brightness-screen {
	local mode="${1:-1}"

	local screen="$(gmktemp --suffix .png)"
	@opts silent y @ screenshot-all "$screen"
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
	brightness $to
}

function brightness-auto-loop {
	# brightness-auto takes ~0.85s
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

	CoreLocationCLI -json | jq .
}

function location-get {
  if isDarwin ; then
	  location-get-darwin | jqm '.latitude, .longitude'
  else
    @NA
  fi
}
##
