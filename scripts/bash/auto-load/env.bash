export BROWSER="lynx"
eve="23.227.196.103"
export LESS=-Ri #R: maintain the ANSI colour sequences; i: smartcase searches (all lower=ignore case);
export HOMEBREW_AUTO_UPDATE_SECS=259200 #72 hours
export LC_ALL="en_US.UTF-8" #set locale; use sudo locale-gen to create them. update-locale is supposed to be the way to go about this, but it didn't work for me.
music_dir="$HOME/my-music/"
logdir="$HOME/logs/"
PRUNE_SONGD_DAYS="+120"

silence eval "add-path NODE_PATH /home/linuxbrew/.linuxbrew/Cellar/node/(^node_modules*/)#/node_modules"
re 'add-path NODE_PATH' '/usr/local/lib/node_modules/' '/usr/lib/nodejs' '/usr/lib/node_module' '/usr/share/javascript'
