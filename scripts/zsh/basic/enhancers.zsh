function enh-mkdest() {
    doc enhances commands by creating directories for destination.
    local dest="${@: -1}"
    ecdbg "dest: $dest   bdest: $(bottomdir "$dest")"
    mkdir -p "$(bottomdir "$dest")"
    ruu command "${emd_c:-comment}" "$@"
}
function self-enh() {
    eval "function \\$2() emd_c=$2 $1" '"$@"'
}
re 'self-enh enh-mkdest' mv cp
