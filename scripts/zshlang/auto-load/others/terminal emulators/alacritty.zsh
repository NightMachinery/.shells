##
function alacritty-theme-setup {
    assert pip-install alacritty-colorscheme @RET
    ##
    local REPO=https://github.com/eendroroy/alacritty-theme.git
    local DEST="$HOME/.eendroroy-colorschemes"
    # Get colorschemes
    assert git clone $REPO $DEST @RET
    # Create symlink at default colors location (optional)
    assert ln -s "$DEST/themes" "$HOME/.config/alacritty/colors" @RET
    assert touch "$HOME/.config/alacritty/alacritty.yml" @RET # expects this file to exist
    ##
}
##
# ALACRITTY_LIGHT_COLOR='gotham.yaml'
# ALACRITTY_DARK_COLOR='papercolor_light.yaml'
##
ALACRITTY_LIGHT_COLOR='solarized_light.yaml'
ALACRITTY_DARK_COLOR='solarized_dark.yaml'
##
function alacritty-colorscheme {
    ensure-dep1 alacritty-colorscheme alacritty-theme-setup @RET

    command alacritty-colorscheme "$@"
}

function alacritty-theme-day() {
    alacritty-colorscheme -V apply "$ALACRITTY_LIGHT_COLOR"
}

function alacritty-theme-night() {
    alacritty-colorscheme -V apply "$ALACRITTY_DARK_COLOR"
}

function alacritty-theme-toggle() {
    alacritty-colorscheme -V toggle "$ALACRITTY_LIGHT_COLOR" "$ALACRITTY_DARK_COLOR"
}
##
