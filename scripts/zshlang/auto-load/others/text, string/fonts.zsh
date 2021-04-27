function font-install() {
    assert isDarwin @RET
    # You can simply copy them into /Library/Fonts (for system wide use) or ~/Library/Fonts (for use by current user).
    cp "$@" /Library/Fonts/
}
