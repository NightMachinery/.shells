rm-deleteus() {
    ! test -e "$deleteus" && return 0
    re trs "${(@f)$(<"$deleteus")}"
    mv "$deleteus" "$deleteus.pbak"
}
