me="Arstar"
export alice='-1001179162919'
export arista='-1001154785017'
export water='-1001293952668'
export ephemeral='-1001404743282'
export tlogs='-1001460496622'
###
function alice() {
    tsend -- "$alice" "$*" && ec "Alicized successfully: $*" || ecerr 'Alicization failed!'
}
noglobfn alice
alias al=alice #NAMECONFLICT: ../Cellar/mono/6.8.0.105/bin/al

function alicedate() {
    local now=("${(s./.)$(datej)}")
    local cyear="$now[1]"
    local cmonth="$now[2]"
    local cday="$now[3]"
    local today=( "$remindayDir/$cyear/$cmonth/$cday"*(.) )

    local text="$(datej) $(date +"%A %B %d")"
    local f
    for f in $today[@] ; do
        if test -e "$today" ; then
            local bak
            bak="$(realpath --relative-to $remindayDir)"
            bak="$cellar/reminders_bak/$bak"
            text="$text"$'\n'"$(<$today)"
            ensure-dir "$bak"
            mv "$today" "$bak"
            rmdir-empty "$remindayDir"
        fi
    done
    tsend -- $alice "$text"
}
