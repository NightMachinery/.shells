export CLIPBOARD_RECORD_FILE=~/tmp/.clipboard
function clipboard-record() {
    local file="$CLIPBOARD_RECORD_FILE" sleep=0.5
    assert-args file @RET

    ecdate "$0: Started; file=$(gq $file)"
    local old paste i
    while true ; do
        pbpaste-plus || ectrace
        if [[ "$paste[*]" != "$old" ]] ; then
            for i in $paste[@] ; do
                ecdate "Recorded: "
                ec "$i" | prefixer --skip-empty -a $'\t\t\t\t\t| '
                ec
                ecn $'\0'"$i" >> $file || ectrace
            done
            old="$paste[*]"
        fi
        sleep $sleep
    done
}
function clipboard-fz() {
    local res
    res="$(<$CLIPBOARD_RECORD_FILE fz --read0 --tac --tiebreak=index "$@")" @RET
    assert pbcopy "$res"
    ec "$res"
}
##
