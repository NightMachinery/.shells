#I am basically using this as shared config between zsh and bash. :D

function addToPATH {
    case ":$PATH:" in
        *":$1:"*) :;; # already there
        *) PATH="$1:$PATH";; # or PATH="$PATH:$1"
    esac
}

addToPATH "$HOME/.cargo/bin"
addToPATH "/usr/local/bin"
#addToPATH "/usr/local/lib"
addToPATH "$HOME/.local/bin"
addToPATH "/Base/- Code/Resources/"
addToPATH "/usr/local/opt/texinfo/bin"

export ALTERNATE_EDITOR="" #Causes Emacs to start a daemon if one is not found.
export LDFLAGS=-L/usr/local/opt/texinfo/lib
export ELM_HOME="/usr/local/bin/"
export JAVA_HOME8=`/usr/libexec/java_home --version 1.8`
export JAVA_HOME9=`/usr/libexec/java_home --version 9`
export JAVA_HOME=$JAVA_HOME8
addToPATH $JAVA_HOME

export RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/src"

alias et="etlas exec eta"
alias et7="~/.etlas/binaries/cdnverify.eta-lang.org/eta-0.7.0.2/binaries/x86_64-osx/eta"
alias pe="pkill -SIGUSR2 Emacs"
alias ls="ls -a"
alias cask="brew cask"
alias bi="brew install"
alias ci="brew cask install"
alias j8='export JAVA_HOME=$JAVA_HOME8; export PATH=$JAVA_HOME/bin:$PATH'
alias j9='export JAVA_HOME=$JAVA_HOME9; export PATH=$JAVA_HOME/bin:$PATH'
alias emacsi="brew install emacs-plus --HEAD --with-24bit-color --with-mailutils --with-x11 --without-spacemacs-icon"

cdf ()
{
    mkdir -p -- "$1" &&
        cd -P -- "$1"
}
. /Users/evar/anaconda/etc/profile.d/conda.sh
conda activate

