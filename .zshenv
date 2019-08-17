set -o vi
export NIGHT_PERSONAL=y
source "$HOME/scripts/zsh/load-first.zsh"
isNotExpensive || {
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
    export GEM_HOME="$HOME/.gem"
    addToPATH "$GEM_HOME/bin"

    # psource ~/anaconda/etc/profile.d/conda.sh
    # silence conda deactivate #this is necessary try sbing and you'll see
    # silence conda activate base
    # PS1="$(echo $PS1 | sed 's/(base) //') "
    # PS1="$(strip "$PS1" ' +') "
    addToPATH ~/anaconda/bin/
    addToPATH ~/miniconda3/bin/
    export MONO_GAC_PREFIX="/usr/local"
    export ELM_HOME="/usr/local/bin/"
    export LDFLAGS=-L/usr/local/opt/texinfo/lib
    isDarwin && {
        export JAVA_HOME8="`/usr/libexec/java_home --version 1.8`"
        export JAVA_HOME9="`/usr/libexec/java_home --version 9`"
        export JAVA_HOME=$JAVA_HOME8
        addToPATH $JAVA_HOME
    }
    export BOOT_CLOJURE_VERSION='1.9.0'

    #export PKG_CONFIG_PATH= "/usr/local/opt/zlib/lib/pkgconfig:/usr/local/lib/pkgconfig:/opt/X11/lib/pkgconfig"

    addToPATH "/usr/local/opt/curl/bin"
    export HH_CONFIG=hicolor
    ## Ugly Stuff PERSONALINFO
    export corra="198.143.181.104"
    alias ccorra="echo -n $corra | pbcopy"
}
test -n "$NO_AUTOLOAD_BASH" || source "$NIGHTDIR"/bash/load-others.bash
