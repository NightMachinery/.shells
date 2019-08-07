function jsummon() {
    mkdir -p ~/julia_tmp/
    local u=(*)
    mv "$u" ~/julia_tmp/
    realpath ~/julia_tmp/"$u"
}
function junsummon() {
    \rm -r ~/julia_tmp
}
jdlc() {
    silence pushd ~/Downloads/
    ge_ecdbg=y onlc get-dl-link
    silence popd
}
