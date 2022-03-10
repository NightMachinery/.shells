##
function poetry-env-activate {
    : "@alt =poetry shell=, =poetry run zsh -f="

    reval-ec source "$(poetry env info --path)/bin/activate"
}
##
