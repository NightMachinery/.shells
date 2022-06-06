#!/usr/bin/env osascript

tell application "System Preferences" to set current pane to pane "com.apple.preferences.sharing"
delay 1
tell application "System Events" to tell process "System Preferences"
    click checkbox 1 of row 9 of table 1 of scroll area 1 of group 1 of window "Sharing"
    delay 1
    if (exists sheet 1 of window "Sharing") then
        click button "Start" of sheet 1 of window "Sharing"
    end if
end tell
ignoring application responses
    tell application "System Preferences" to quit
end ignoring
