###
using namespace System.Management.Automation
using namespace System.Management.Automation.Language
###
echo "profile.ps1 started"
###
Import-Module PSReadLine

Set-PSReadLineOption -EditMode Emacs
##
#: see https://github.com/PowerShell/PSReadLine/blob/master/PSReadLine/SamplePSReadLineProfile.ps1

Set-PSReadLineOption -HistorySearchCursorMovesToEnd
Set-PSReadLineKeyHandler -Key UpArrow -Function HistorySearchBackward
Set-PSReadLineKeyHandler -Key DownArrow -Function HistorySearchForward

Set-PSReadLineKeyHandler -Key Alt+LeftArrow -Function BackwardWord
Set-PSReadLineKeyHandler -Key Alt+RightArrow -Function ForwardWord

# Clipboard interaction is bound by default in Windows mode, but not Emacs mode.
Set-PSReadLineKeyHandler -Key Ctrl+C -Function Copy
Set-PSReadLineKeyHandler -Key Ctrl+v -Function Paste
###
echo "profile.ps1 ended"
