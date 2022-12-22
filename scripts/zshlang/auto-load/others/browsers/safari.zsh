##
function safari-current-url {
    osascript -e 'tell application "Safari" to get URL of current tab of window 1'
}

function safari-current-title {
    osascript -e 'tell application "Safari" to get name of current tab of window 1'
}
##
