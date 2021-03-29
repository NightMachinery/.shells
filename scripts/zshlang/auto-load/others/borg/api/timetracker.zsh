function borg-req() {
    curl --fail --silent --location --header "Content-Type: application/json" --data '@-' $borgEndpoint/"$@"
}
##
function borg-tt-mark() {
    local received_at="${borg_tt_mark_at}"

	local out
	out="$(ec "$*" | text2num | jq --raw-input --arg received_at "$received_at" --slurp --null-input --compact-output 'inputs as $i | {"name": $i, "received_at": $received_at}' | borg-req timetracker/mark/)" || return $?
	test -z "$out" && return 1
	ec $out
	[[ "$out" == *("cold shoulder"|"Julia encountered an exception.")* ]] && return 1 || true
}

function borg-tt-last() {
    local count="${1:-6}"

    catsql --table activity --order id- --limit "$count" "$timetracker_db" | gsed -n '4,$p' | sd -f m '^\d+,' '' | sdlit $'\n' $'\n\n' | sdlit , $'\n    '
}
##
function borg-tt-cmdlog() {

}
