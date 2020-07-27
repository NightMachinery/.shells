me="Arstar"
alice='-1001179162919'
arista='-1001154785017'
water='-1001293952668'
ephemeral='-1001404743282'
###
alice() {
    tsend -- "$alice" "$*" && ec "Alicized $* successfully" || ecerr 'Alicization failed!'
}
noglobfn alice
alias al=alice #NAMECONFLICT: ../Cellar/mono/6.8.0.105/bin/al

alicedate() { tsend -- $alice "$(datej)" }
