me="Arstar"
export alice='-1001179162919'
export arista='-1001154785017'
export water='-1001293952668'
export ephemeral='-1001404743282'
export tlogs='-1001460496622'
export tlg_notifs='-1001185370891'
export tlg_notifc="$water"
###
function alice() {
    tsend -- "$alice" "$*" && ec "Alicized successfully: $*" || ecerr 'Alicization failed!'
}
noglobfn alice
alias al=alice #NAMECONFLICT: ../Cellar/mono/6.8.0.105/bin/al

function alicedate() {
    local log
    log="$(cellp 2>&1)" || { # to update reminders
        remj "cellp failed with $?: $log"
    }
    tlg-reminday "$alice"
}
