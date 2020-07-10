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
get-volume() {
    osascript -e 'set ovol to output volume of (get volume settings)'
}
function setv() {
    osascript -e "set volume output volume $1"
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
##
