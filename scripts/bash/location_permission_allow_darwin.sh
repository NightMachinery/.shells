#!/usr/bin/env bash
#: @forkedFrom https://gist.github.com/JayBrown/c14642a62e424db3cdc933c9140c8de8
#: adds executables to /var/db/locationd/clients.plist
##

# export PATH=/opt/homebrew/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/usr/local/sbin:/opt/local/bin:/opt/local/sbin:"$HOME"/.local/bin:"$HOME"/.local/sbin

# default locations
locdir="/var/db/locationd"
locloc="$locdir/clients.plist"

# backup location
posixdate=$(date +%s)
locbak="$locdir/clients.plist.$posixdate.bak"

# logging
loggeduser=$(scutil <<< "show State:/Users/ConsoleUser" | awk '/Name :/ && ! /loginwindow/{print $3}')
process="location_permission_allow_darwin"
logloc="$HOME/Library/Logs/local.$loggeduser.$process.log"
exec > >(tee -a "$logloc") 2>&1
currentdate=$(date)
echo "*** $process: $currentdate ***"

# exit function
_quit () {
	sudo -k
	exit "$1"
}

# check for root user
if ! [[ $(id -u) -eq 0 ]] ; then
	echo "ERROR: $process needs to executed as root!" >&2
	_quit 1
fi

# check for dependencies: coreutils (realpath) or python3
coreutilsinstalled=false
if command -v grealpath &>/dev/null ; then
	echo "INFO: coreutils detected"
	coreutilsinstalled=true
else
	echo "INFO: coreutils not detected!"
	if ! command -v python3 &>/dev/null ; then
		echo "ERROR: neither coreutils nor python3 detected on this system!" >&2
		_quit 1
	else
		echo "INFO: python3 detected"
	fi
fi

# check for input
if ! [[ $* ]] ; then
	echo "ERROR: no input!" >&2
	_quit 1
fi

# absolute path function (python3)
_abspath () {
	python3 - "$1" << 'EOF'
import os.path
import sys
for arg in sys.argv[1:]:
    print(os.path.realpath(os.path.abspath(arg)))
EOF
}

# backup clients.plist
if ! cp "$locloc" "$locbak" &>/dev/null ; then
	echo -e "ERROR: unable to backup clients.plist!\nPlease add CLI manually!" >&2
	_quit 1
else
	chown _locationd:_locationd "$locbak" 2>/dev/null # not really necessary (just a backup)
fi

# main routine to add CLIs to clients.plist
errors=false
errorn=0
clin=0
for cli in "$@"
do
	((clin++))
	echo "---"
	# check if input is path or CLI name
	if echo "$cli" | grep "/" &>/dev/null ; then # path
		cliname=$(basename "$cli")
		if ! [[ -f "$cli" ]] ; then
			errors=true
			((errorn++))
			echo -e "ERROR: $cliname not found or not a regular file!\n---"
			continue
		else
			if ! [[ -x "$cli" ]] ; then
				errors=true
				((errorn++))
				echo -e "ERROR: $cliname not executable!\n---"
				continue
			fi
			clipath="$cli"
		fi
	else # CLI namae
		clipath=$(command -v "$cli" 2>/dev/null)
		if ! [[ $clipath ]] ; then
			errors=true
			((errorn++))
			echo -e "ERROR: $cli not in \$PATH!\n---" >&2
			continue
		fi
		cliname="$cli"
	fi
	# determine absolute path
	if $coreutilsinstalled ; then
		cliabspath=$(grealpath "$clipath" 2>/dev/null)
	else
		cliabspath=$(_abspath "$clipath" 2>/dev/null)
	fi
	if [[ $cliabspath == "$clipath" ]] ; then
		echo -e "$cliname detected\n$cliabspath"
	else
		echo -e "$cliname detected\n$clipath -> $cliabspath"
	fi
	# remove from quarantine
	echo "INFO: removing quarantine extended attribute..."
	xattr -d com.apple.quarantine "$cliabspath" 2>/dev/null
	bundleid="com.apple.locationd.executable-$cliabspath"
	echo "Bundle ID: $bundleid"
	# check for code signature
	csig=$(codesign -dv -r- "$cliabspath" 2>&1)
	if echo "$csig" | grep "code object is not signed at all" &>/dev/null ; then # sign ad-hoc for CDHash
		echo "INFO: code-signing executable with ad-hoc signature..."
		codesign --force -s - "$cliabspath" 2>/dev/null
		sleep .5
		csig=$(codesign -dv -r- "$cliabspath" 2>&1)
	fi
	# read signature requirements
	reqspresent=false
	reqs=$(echo "$csig" | grep -e "^designated => " -e "^# designated => " | awk -F" => " '{print $2}')
	if [[ $reqs ]] ; then
		reqspresent=true
	else
		reqs="n/a"
	fi
	# write new dictionary
	echo -e "Requirement: $reqs\nINFO: adding new client entry..."
	if $reqspresent ; then
		if ! defaults write "$locloc" "$bundleid" -dict \
			Authorized -bool true \
			BundleId -string "$bundleid" \
			Executable -string "$cliabspath" \
			Hide -int 0 \
			Registered -string "$cliabspath" \
			Requirement -string "$reqs" \
			Whitelisted -bool true &>/dev/null ; then
			echo "ERROR: while adding new client entry with defaults!"
			errors=true
			((errorn++))
		fi
	else
		if ! defaults write "$locloc" "$bundleid" -dict \
			Authorized -bool true \
			BundleId -string "$bundleid" \
			Executable -string "$cliabspath" \
			Hide -int 0 \
			Registered -string "$cliabspath" \
			Whitelisted -bool true &>/dev/null ; then
			echo "ERROR: while adding new client entry with defaults!"
			errors=true
			((errorn++))
		fi
	fi
	echo "---"
done

# check total errors
if $errors ; then
	if [[ $errorn -eq $clin ]] ; then
		echo "Done: $errorn of $clin possible errors!" >&2
		_quit 1
	fi
fi

# set owner & group for good measure
echo "INFO: setting owner & group to '_locationd'"
if ! chown _locationd:_locationd "$locloc" &>/dev/null ; then
	errors=true
	echo -e "WARNING: unable to change owner & group!\nNOTE: please apply manually if necessary!"
fi

# restart locationd
echo "INFO: restarting locationd..."
killall locationd 2>/dev/null

# exit
if ! $errors ; then
	echo "Done."
	_quit 0
else
	echo "Done: $errorn of $clin possible errors!" >&2
	_quit 1
fi
