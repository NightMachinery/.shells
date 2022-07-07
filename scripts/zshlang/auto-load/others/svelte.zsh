##
function svelte-watch {
    {
        ec _
        fswatch "src"
    } | {
        #: protect our stdin:
        exec {fd_in}<&0
        exec </dev/null
        while read -d $'\n' -r file <&${fd_in} ; do
            if npm run build ; then
                bell-hp3-water-trickle
            else
                bell-fail
            fi

            dateshort
        done
    } always {
        exec {fd_in}<&-
    }

}
##
function soup-test {
    local endpoint="http://127.0.0.1:9102/register/player"

    local stdin="mighty1"
    req="$(printf -- "%s " "$stdin" | jq --raw-input --slurp --null-input --compact-output 'inputs as $i | {"name": $i, "game_id": "98819"}')"
    ec $req | jq .
    printf -- "%s" "$req" | curl --fail --location --header "Content-Type: application/json" --request POST --data '@-' $endpoint
}
##
