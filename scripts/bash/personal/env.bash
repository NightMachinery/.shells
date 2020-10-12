export NNN_USE_EDITOR=1
# export BROWSER="lynx"
##
export HOMEBREW_AUTO_UPDATE_SECS=$((3600*24*7))
export HOMEBREW_NO_AUTO_UPDATE=y # since the other option doesn't seem to work reliably. Use cron to update brew manually.
##
export LC_ALL="en_US.UTF-8" #set locale; use sudo locale-gen to create them. update-locale is supposed to be the way to go about this, but it didn't work for me.
export LPASS_AUTO_SYNC_TIME=$((3600*24*7))
export LPASS_AGENT_TIMEOUT=$((3600*24*7))
export VCSH_GITATTRIBUTES=y
export kindle_email=fifya@kindle.com #PERSONALINFO
isD && export NEDITOR='emacsclient -t' || export NEDITOR='vim' #emacsclient `-t` is essential.
export ALTERNATE_EDITOR="" #Causes Emacs to start a daemon if one is not found.
export SUDO_EDITOR="$NEDITOR"
export VISUAL="$NEDITOR"
export EDITOR="$VISUAL"
##
export JULIA_EDITOR='emacsclient'
# emacsclient opens in an already open client, vim is fast, nvim highlights but is slow
# https://discourse.julialang.org/t/emacsclient-doesnt-work-with-edit/43673
##
export NODEJS_CHECK_SIGNATURES=no #for asdf's nodejs
##
# export WG_CONF=~/.cf-warp/cf-warp.conf
# export WG_CONF=~/Base/keys/karbas/fereydoun1.conf
# export WG_CONF=~/Base/keys/zii/wg0-client-c1.conf
export WG_CONF=~/Base/keys/zii/peer1.conf
##
export DOOMDIR=~/doom.d
##
test -z "$XDG_RUNTIME_DIR" && export XDG_RUNTIME_DIR="/tmp/runtime-sth" # Ubuntu bug?
##
