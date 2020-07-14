me="Arstar"
alice='-1001179162919'
arista='-1001154785017'
water='-1001293952668'
ephemeral='-1001404743282'
###
alias al=alice #NAMECONFLICT: ../Cellar/mono/6.8.0.105/bin/al
alice() {
    tsend -- "$alice" "$*"
}
alicedate() { tsend -- $alice "$(datej)" }
