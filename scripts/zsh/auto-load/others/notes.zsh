export nightNotes="$cellar/notes/"
export nightJournal="$nightNotes/journal"
##
function jt() {
	local today="$(date +"%Y.%b.%d") $(datej|tr / -)"
	local dest="$nightJournal/$today.md"
	test -e "$dest" || {
	ec "# $today"$'\n\n'"* " >> $dest
	}
	! isI || $EDITOR[@] $dest
}
