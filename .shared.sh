### set max open files, etc
# The "hard limit" (`-H`) could also be set, but only to a value less than the current one, and only to a value not less than the "soft limit" (`-S`).
ulimit -S -n 50000 >&/dev/null || ulimit -S -n 10240 # the first one fails on a non-configured macOS
##
maxproc="$(ulimit -Hu)" # local not allowed on some shells
maxproc=$(( maxproc - 100 )) # give some slack to the OS
if (( maxproc < 10000 )) ; then
    maxproc=10000
fi
ulimit -Su "$maxproc"
unset maxproc
### @duplicateCode/jhsd99wiw3i3hehiajh
if test -z "$NIGHTDIR" ; then
    NIGHTDIR=~/scripts # @hardcoded bash/'zsh -f' have no way of determining this by themselves
    if ! test -d "$NIGHTDIR" ; then
        unset NIGHTDIR
        echo "NIGHTDIR not found"
    fi
fi
##
# export EMACS_SOCKET_NAME=/tmp/sockets/.emacs
export EMACS_SOCKET_NAME="${EMACS_SOCKET_NAME:-${HOME}/tmp/.emacs-servers/server}"
EMACS_ALT1_SOCKET_NAME="${HOME}/tmp/.emacs-servers/server_alt1"
###
if true ; then # ! command -v brew &> /dev/null ; then # it's faster to just not check
    # Sometimes brew itself is in path but its dirs are not. Also, we want to be able to rerun the code to correct the PATH ordering
    if isLinux; then
        if test -d ~/.linuxbrew ; then
            eval $(~/.linuxbrew/bin/brew shellenv)
        fi

        if test -d /home/linuxbrew/.linuxbrew ; then
            eval $(/home/linuxbrew/.linuxbrew/bin/brew shellenv)

            typeset -xg brew_bin_dir=/home/linuxbrew/.linuxbrew/bin
            addToPATH "$brew_bin_dir" # `brew shellenv` doesn't work on root, it seems
        fi
    elif isDarwin ; then
        if isArm ; then
            test -d /opt/homebrew/bin && eval $(/opt/homebrew/bin/brew shellenv)
            # see what this code does with:
            # `env -i zsh -f -c '/opt/homebrew/bin/brew shellenv'`

            typeset -xg brew_bin_dir=/opt/homebrew/bin
            addToPATH "$brew_bin_dir"
            addToPATH /opt/homebrew/sbin
        else
            typeset -xg brew_bin_dir=/usr/local/bin
        fi
    fi
fi

addToPATH /opt/local/bin /opt/local/sbin # macports
addToPATH /usr/sbin
addToPATH /Applications/SuperCollider.app/Contents/Resources
addToPATH /Applications/SuperCollider.app/Contents/MacOS
addToPATH /Applications/MEGAcmd.app/Contents/MacOS
addToPATH ~/.emacs.d.doom/bin
addToPATH "$HOME/.dotnet/tools"
addToPATH "/Library/TeX/texbin"
addToPATH "$HOME/.cargo/bin"
isLinux && addToPATH /snap/bin
addToPATH "/sbin"
addToPATH "/usr/local/bin"
addToPATH "/usr/local/sbin"
addToPATH "$HOME/.local/bin"
addToPATH "$HOME/code/resources"
export GOPATH="$HOME/go"
export GOBIN="$GOPATH/bin"
addToPATH "$HOME/go/bin"
addToPATH "/usr/local/opt/texinfo/bin"
addToPATH "$HOME/kscripts"
addToPATH "/usr/libexec"
export GEM_HOME="$HOME/.gem"
addToPATH "$GEM_HOME/bin"
addToPATH "$HOME/.poetry/bin"
addToPATH ~/.nimble/bin
addToPATH /Applications/mpv.app/Contents/MacOS
addToPATH "${NIGHTDIR}/zshlang/wrappers"

# psource ~/anaconda/etc/profile.d/conda.sh
# silence conda deactivate #this is necessary try sbing and you'll see
# silence conda activate base
# PS1="$(echo $PS1 | sed 's/(base) //') "
# PS1="$(strip "$PS1" ' +') "
addToPATH ~/anaconda/bin
addToPATH ~/miniconda3/bin
addToPATH ~/.cargo/bin/

## perl
# [[id:162e013e-ce1b-405a-9d45-e0e223f56d6d][perl/cpanm.org]]
if false; then
    ##
    # can cause a `ListUtil.c: loadable library and perl binaries are mismatched (got handshake key 0xfa80080, needed 0xf880080)` error
    ##
    addToPATH "${HOME}/perl5/bin"
    PERL5LIB="${HOME}/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"; export PERL5LIB;
    PERL_LOCAL_LIB_ROOT="${HOME}/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"; export PERL_LOCAL_LIB_ROOT;
    PERL_MB_OPT="--install_base \"${HOME}/perl5\""; export PERL_MB_OPT;
    PERL_MM_OPT="INSTALL_BASE=${HOME}/perl5"; export PERL_MM_OPT;
else
    if isDarwin ; then
        # [[id:36836e3f-e409-4867-9ae5-68f16e391efe][perl/cpanm:install path]]
        addToPATH /usr/local/Cellar/perl/5.34.0/bin # @hardcoded
    fi
fi
##
isDarwin && {
    addToPATH /Users/Shared/bin
    export MONO_GAC_PREFIX="/usr/local"
    export ELM_HOME="/usr/local/bin/"

    # Use `brew info openjdk` to link java properly.
    # export JAVA_HOME15="$(/usr/libexec/java_home --version 15)"
    # export JAVA_HOME=$JAVA_HOME15
    export JAVA_HOME="$(/usr/libexec/java_home)"
    addToPATH $JAVA_HOME

    export ANDROID_SDK_ROOT="$HOME/Library/Android/sdk"

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
addToPATH ~/bin # should be last

if isZsh ; then
    typeset -Ug path
fi
