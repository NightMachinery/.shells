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
    tlg-reminday "$alice"
}
