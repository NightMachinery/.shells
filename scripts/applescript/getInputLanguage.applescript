#!/usr/bin/env osascript

set activeKbdLayout to my getActiveKeyboardLayout()
on getActiveKeyboardLayout()
set plistPath to "~/Library/Preferences/com.apple.HIToolbox.plist"
try
do shell script "defaults read " & plistPath & " dummy"
end try

tell application "System Events"
repeat with pli in property list items of ¬
property list item "AppleSelectedInputSources" of ¬
property list file plistPath
try
return value of property list item "KeyboardLayout Name" of pli
end try
end repeat
end tell
end getActiveKeyboardLayout

if activeKbdLayout is "Khmer" then

return ":cambodia:"

else if activeKbdLayout is "Persian-ISIRI 2901" then

return "🇱🇧"

else if activeKbdLayout is "U.S." then

return "🇺🇸"

else if activeKbdLayout is "Greek" then

return ":greece:"

else if activeKbdLayout is "French" then

return ":fr:"

else if activeKbdLayout is "Thai" then

return ":thailand:"

end if
