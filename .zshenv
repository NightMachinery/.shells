autoload -U zargs #Necessary for scripts


source "$HOME/scripts/bash/load-first.bash"
typeset -Ug path
function addToPATH {
   # case ":$PATH:" in
   #     *":$1:"*) :;; # already there
   #     *) PATH="$1:$PATH";; # org/r PATH="$PATH:$1"
   # esac
   #path[1,0]="$1"
   #path=("$1" "$path[@]")
   PATH="$1:$PATH"
   typeset -Ug path
}

if ! (( $+commands[brew] )) ; then
test -d ~/.linuxbrew && eval $(~/.linuxbrew/bin/brew shellenv)
test -d /home/linuxbrew/.linuxbrew && eval $(/home/linuxbrew/.linuxbrew/bin/brew shellenv)
fi

addToPATH ~/bin
addToPATH "$HOME/.dotnet/tools"
addToPATH "/Library/TeX/texbin"
addToPATH "$HOME/.cargo/bin"
addToPATH /snap/bin
addToPATH "/usr/local/bin"
addToPATH "$HOME/.local/bin"
addToPATH "/Base/- Code/Resources/"
addToPATH "$HOME/go/bin"
addToPATH "/usr/local/opt/texinfo/bin"
addToPATH "$HOME/kscripts/"
addToPATH "/usr/libexec/"

source <(antibody init)
ecdbg "$path"
psource ~/anaconda/etc/profile.d/conda.sh
# export PYTHONHOME=/Users/evar/anaconda/bin/
# silence conda deactivate
silence conda activate base
PS1="$(echo $PS1 | sed 's/(base) //') "
PS1="$(strip "$PS1" ' +') "

ecdbg "$path"
export corra="198.143.181.104"
alias ccorra="echo -n $corra | pbcopy"
export sgate="198.143.181.179"
alias csgate="echo -n $sgate | pbcopy"
export HISTSIZE=100000
export HISTTIMEFORMAT="%m/%d/%Y %T " #I always tend to configure my machines with an large HISTSIZE value so it keeps a longer history list, as well as HISTTIMEFORMAT with the time stamp value so I can see when was the command ran.
export ALTERNATE_EDITOR="" #Causes Emacs to start a daemon if one is not found.
export SUDO_EDITOR="emacsclient"
export MONO_GAC_PREFIX="/usr/local"
export VISUAL='emacsclient -t'
export EDITOR="$VISUAL"
export LDFLAGS=-L/usr/local/opt/texinfo/lib
export ELM_HOME="/usr/local/bin/"
eval-darwinq 'export JAVA_HOME8=`/usr/libexec/java_home --version 1.8`'
eval-darwinq 'export JAVA_HOME9=`/usr/libexec/java_home --version 9`'
eval-darwinq 'export JAVA_HOME=$JAVA_HOME8'
eval-darwinq 'addToPATH $JAVA_HOME'
export BOOT_CLOJURE_VERSION='1.9.0' 

export RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/src" &>/dev/null
#export PKG_CONFIG_PATH= "/usr/local/opt/zlib/lib/pkgconfig:/usr/local/lib/pkgconfig:/opt/X11/lib/pkgconfig"

addToPATH "/usr/local/opt/curl/bin"
GPG_TTY=$(tty)
export GPG_TTY

unalias run-help &> /dev/null
autoload run-help

psource ~/torch/install/bin/torch-activate

export HH_CONFIG=hicolor
source ~/scripts/bash/load-others.bash

set -o vi
