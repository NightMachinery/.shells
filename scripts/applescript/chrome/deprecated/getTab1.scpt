#!/usr/bin/env osascript

tell application "BetterTouchTool"
	if is_app_running "Google Chrome" then
		tell application "Google Chrome"
			try
				set windowCount to number of windows
				repeat with x from 1 to windowCount
					set maxSize to 15
					set tabcount to number of tabs in window x
					repeat with y from 1 to tabcount
						set tabName to title of tab 1 of window x
						if length of tabName is greater than maxSize then
							set tabName to text 1 thru (maxSize - 3) of tabName & "..."
						end if
						return tabName
					end repeat
				end repeat
			on error
				return ""
			end try
		end tell
	else
		return ""
	end if
end tell
