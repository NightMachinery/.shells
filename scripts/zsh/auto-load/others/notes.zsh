function rn() {
    local files=()
}
function jrlt() {
	local today="$(date +"%Y.%b.%d") $(datej|tr / -)"
	local dest="$today.md"
	test -e "$dest" || {
	ec "# $today"$'\n\n'"* " >> $dest
	}
	$EDITOR[@] $dest
}
