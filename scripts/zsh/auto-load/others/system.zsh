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
function volget() {
	osascript -e 'set ovol to output volume of (get volume settings)'
}
function volset() {
	osascript -e "set volume output volume $1"
}
aliasfn setv volset
aliasfn get-volume volget
aliasfn getv volget
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
function display-off() {
	watch -n ${1:-1} brightness 0
	#macOS only probably
}
function resetdns-darwin() {
	sudo dscacheutil -flushcache
}
##
function logout() {
	logout-darwin "$@"
}
function logout-darwin() {
	sudo launchctl bootout user/$(id -u "${1:$(whoami)}")
}
function logout-darwin2() {
	osascript -e 'tell application \"System Events\" to log out'
}
## @darwinonly
function display-gray-is() {
	[[ "$(gray_darwin.c s)" == "Grayscale is now: 1" ]]
}
function display-gray-toggle() {
	gray_darwin.c
}
function display-gray-off() {
	gray_darwin.c n
}
function display-gray-on() {
	gray_darwin.c y
}
##
function brightness-get() {
	if isDarwin ; then # @darwinonly
		if [[ "$(brightness -l)" =~ 'display 0: brightness (\S+)' ]] ; then
			ec "$match[1]"
		else
			return 1
		fi
	fi
}
function brightness-screen() {
	local mode="${1:-1}"

	local screen="$(gmktemp --suffix .png)"
	screencapture -x "$screen" # @darwinonly
	local screen_brightness="$(detect_brightness_mode=$mode detect_brightness.py $screen)"
	command rm $screen
	ec $screen_brightness
}
function brightness-auto() {
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
function brightness-auto-loop() {
	# brightness-auto takes ~0.85s
	serr @opts s "${lo_s:-3}" @ loop brightness-auto "${@:-0.3}"
}
@opts-setprefix brightness-auto-loop lo
##
function open_command() {
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
