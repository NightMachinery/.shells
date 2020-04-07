set -o vi
export disable_malice=y
export NIGHT_PERSONAL=y
source "$HOME/scripts/zsh/load-first.zsh"
typeset -g NIGHT_NO_EXPENSIVE
isNotExpensive || {
    if ! (( $+commands[brew] )) ; then
        test -d ~/.linuxbrew && eval $(~/.linuxbrew/bin/brew shellenv)
        test -d /home/linuxbrew/.linuxbrew && eval $(/home/linuxbrew/.linuxbrew/bin/brew shellenv)
    fi

    addToPATH ~/code/node/snips-alice
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
    addToPATH ~/bin

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

    # needed by pdf-tools of emacs
    export PKG_CONFIG_PATH="/usr/local/opt/zlib/lib/pkgconfig:/usr/local/lib/pkgconfig:/opt/X11/lib/pkgconfig"

    addToPATH "/usr/local/opt/curl/bin"
    export HH_CONFIG=hicolor
    ## Ugly Stuff PERSONALINFO
    export corra="198.143.181.104"
    alias ccorra="echo -n $corra | pbcopy"
    #isLinux && export TCLLIBPATH=/usr/lib/x86_64-linux-gnu
    source <(antibody init)
    ANTIBODY_HOME="$(antibody home)"
    # DISABLE_DEFER=y
    # Won't defer if not interactive or disabled explicitly
    { [[ $- == *i* ]] && test -z "$DISABLE_DEFER" } && antibody bundle romkatv/zsh-defer || alias zsh-defer=''
    source-interactive-all() {
        run-on-each source "$NIGHTDIR"/zsh/interactive/**/*(.)
        typeset -g NIGHT_NO_EXPENSIVE
        NIGHT_NO_EXPENSIVE=y
    }
    function rp() {
        test -e "$1" && realpath "$1" || realpath "$(which "$1")"
    }
}
test -n "$NO_AUTOLOAD_BASH" || zsh-defer source "$NIGHTDIR"/bash/load-others.bash
