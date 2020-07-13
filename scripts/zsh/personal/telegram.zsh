me="Arstar"
alice='-1001179162919'
alias al=alice #NAMECONFLICT: ../Cellar/mono/6.8.0.105/bin/al
alice() {
    tsend -- "$alice" "$*"
}
alicedate() { tsend -- $alice "$(datej)" }
