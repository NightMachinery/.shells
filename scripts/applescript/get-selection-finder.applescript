#!/usr/bin/env osascript
tell application "Finder" to set selectedFiles to selection as alias list
if selectedFiles is {} then return
set filePaths to {}
repeat with thisFile in selectedFiles
    set end of filePaths to POSIX path of thisFile
end repeat
set text item delimiters to linefeed
set the clipboard to (filePaths as text)