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

# export kindle_email=fifya@kindle.com #: @personalInfo
# export kindle_email=Azadi4Baran@kindle.com #: @personalInfo
export kindle_email=jkajskjsoqw61azadi4baran@kindle.com #: @personalInfo

isLocal && export NEDITOR='emacs.dash' || export NEDITOR='vim'
export ALTERNATE_EDITOR="" #: Causes Emacs to start a daemon if one is not found.
export SUDO_EDITOR="$NEDITOR"
export VISUAL="$NEDITOR"
export EDITOR="$VISUAL"
##
# export JULIA_EDITOR='emacsclient'
export JULIA_EDITOR='editor-open-adv'
# emacsclient opens in an already open client, vim is fast, nvim highlights but is slow
# [[file:~/Base/_Code/uni/stochastic/common/startup.jl::InteractiveUtils.define_editor("editor-open-adv", wait=false) do cmd, path, line][startup.jl::define_editor]]
##
export NODEJS_CHECK_SIGNATURES=no #for asdf's nodejs
##
if isGrayfur ; then
    export WG_CONF=~/Base/keys/eva/eva_peer13.conf
else
    # export WG_CONF=~/.cf-warp/cf-warp.conf
    # export WG_CONF=~/Base/keys/karbas/fereydoun1.conf
    # export WG_CONF=~/Base/keys/zii/wg0-client-c1.conf
    # export WG_CONF=~/Base/keys/zii/peer1.conf
    export WG_CONF=~/Base/keys/eva/eva_peer1.conf
fi
##
export DOOMDIR=~/doom.d
##
test -z "$XDG_RUNTIME_DIR" && export XDG_RUNTIME_DIR="/tmp/runtime-sth" # Ubuntu bug?
##
