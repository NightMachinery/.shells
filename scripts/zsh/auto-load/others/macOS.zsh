alias lock='"/System/Library/CoreServices/Menu Extras/User.menu/Contents/Resources/CGSession" -suspend ; pmset displaysleepnow' # Command+Ctrl+q locks natively; Use lock.as to press them ;)) (Needs assistive access)

function finder-hideicons() {
    defaults write com.apple.finder CreateDesktop false
    killall Finder
}
