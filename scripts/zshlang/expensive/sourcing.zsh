## I have disabled asdf for now.
# asdf_dir="$(brew --prefix asdf)" && {
# . $asdf_dir/asdf.sh
# }
##
function nix-source() {
    psource ~/.nix-profile/etc/profile.d/nix.sh
}
##
function rvm-source() {
    if ! isSudo; then
        psource ~/.rvm/scripts/rvm
    fi
}
rvm-source # takes ~0.3s
##
