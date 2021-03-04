if true ; then # ! command -v brew &> /dev/null ; then
    # Sometimes brew itself is in path but its dirs are not.
    test -d ~/.linuxbrew && eval $(~/.linuxbrew/bin/brew shellenv)
    test -d /home/linuxbrew/.linuxbrew && eval $(/home/linuxbrew/.linuxbrew/bin/brew shellenv)
fi

addToPATH /usr/sbin
addToPATH /Applications/SuperCollider.app/Contents/Resources/
addToPATH /Applications/SuperCollider.app/Contents/MacOS/
addToPATH ~/.emacs.d.doom/bin/
addToPATH "$HOME/.dotnet/tools"
addToPATH "/Library/TeX/texbin"
addToPATH "$HOME/.cargo/bin"
isLinux && addToPATH /snap/bin
addToPATH "/usr/local/bin"
addToPATH "/usr/local/sbin"
addToPATH "$HOME/.local/bin"
addToPATH "$HOME/code/resources/"
export GOPATH="$HOME/go"
export GOBIN="$GOPATH/bin"
addToPATH "$HOME/go/bin"
addToPATH "/usr/local/opt/texinfo/bin"
addToPATH "$HOME/kscripts/"
addToPATH "/usr/libexec/"
export GEM_HOME="$HOME/.gem"
addToPATH "$GEM_HOME/bin"
addToPATH "$HOME/.poetry/bin"
addToPATH ~/.nimble/bin
addToPATH /Applications/mpv.app/Contents/MacOS

# psource ~/anaconda/etc/profile.d/conda.sh
# silence conda deactivate #this is necessary try sbing and you'll see
# silence conda activate base
# PS1="$(echo $PS1 | sed 's/(base) //') "
# PS1="$(strip "$PS1" ' +') "
addToPATH ~/anaconda/bin/
addToPATH ~/miniconda3/bin/

addToPATH ~/bin

isDarwin && {
    export MONO_GAC_PREFIX="/usr/local"
    export ELM_HOME="/usr/local/bin/"

    # Use `brew info openjdk` to link java properly.
    export JAVA_HOME15="`/usr/libexec/java_home --version 15`"
    export JAVA_HOME=$JAVA_HOME15
    addToPATH $JAVA_HOME

    addToPATH /usr/local/opt/qt/bin

    export LDFLAGS='-L/usr/local/opt/texinfo/lib -L/usr/local/opt/qt/lib'
    export CPPFLAGS="-I/usr/local/opt/qt/include"

    # needed by pdf-tools of emacs
    export PKG_CONFIG_PATH="/usr/local/opt/qt/lib/pkgconfig:/usr/local/opt/zlib/lib/pkgconfig:/usr/local/lib/pkgconfig:/opt/X11/lib/pkgconfig"


}
export BOOT_CLOJURE_VERSION='1.9.0'



addToPATH "/usr/local/opt/curl/bin"

run-on-each addToPATH "$NIGHTDIR"/**/
addToPATH /myBin/

if isZsh ; then
    typeset -Ug path
fi
