###
alias zsh-defer=''

function psource() {
    if [[ -r "$1" ]]; then # -r: readable file
        source "$1"
    fi
}

function nightsh-load-zshenv() {
    ZSH_PWD_CACHE=~/tmp/.zsh_pwd
    function zsh-pwd-save() {
        if test -z "$HISTFILE" ; then
            return 0
        fi

        print -r -- $PWD > "$ZSH_PWD_CACHE"
    }
    function zsh-pwd-load() {
        local d
        d="$(cat "$ZSH_PWD_CACHE" 2>/dev/null)" || return 0
        if test -d "$d" ; then
            cd -- "$d"
        fi
    }
    if [[ -o interactive ]] ; then
        zsh-pwd-load
    fi
    ##
    test -z "$ZSH_PROFILEME" || zmodload zsh/zprof # use zprof -c to reset counters
    # Does not profile internals of functions well.
    #
    # `ZSH_PROFILEME=y zsh -c 'zprof > ~/tmp/a'`
    #
    # Visualize:
    #  `zprof  | gprof2dot -f perf | dot -Tpng -o output.png`
    #  Did not work because of a unicode error :shrugs:
    ##
    # module_path+=( "/Users/evar/.zinit/mod-bin/zmodules/Src" )
    # zmodload zdharma/zplugin
    # Doesn't work with aliases
    ##
    # export disable_malice=''
    export disable_malice='y'
    export NIGHT_PERSONAL=y
    source "$HOME/scripts/zshlang/load-first.zsh"
    typeset -g NIGHT_NO_EXPENSIVE
    isNotExpensive || {
        source ~/.shared.sh

        export HH_CONFIG=hicolor
        ##
        # export GERBIL_HOME="$(brew --cellar gerbil-scheme)" # @exorbitant
        if isDarwin ; then
            export GERBIL_HOME=/usr/local/Cellar/gerbil-scheme/0.16/
            # not sure if this is even right
        fi
        ##
        # this is a TCL list https://wiki.tcl-lang.org/page/list , uses whitespace as sep
        #isLinux && export TCLLIBPATH=/usr/lib/x86_64-linux-gnu
        isLinux && export TCLLIBPATH="$TCLLIBPATH /home/linuxbrew/.linuxbrew/lib"
        isDarwin && export TCLLIBPATH="/usr/local/lib" # for expect to work
        ##
        source <(antibody init)
        ANTIBODY_HOME="$(antibody home)"

        DISABLE_DEFER=y
        # Won't defer if not interactive or disabled explicitly
        if { [[ -o interactive ]] && test -z "$DISABLE_DEFER" } ; then
            unalias zsh-defer # @warn this is too late to affect the code already loaded
            antibody bundle romkatv/zsh-defer
        fi

        function source-interactive-all() {
            run-on-each source "$NIGHTDIR"/zshlang/interactive/auto-load/**/*(.)
            source "$NIGHTDIR"/zshlang/interactive/completions.zsh # needs to be semi-last
            typeset -g NIGHT_NO_EXPENSIVE
            NIGHT_NO_EXPENSIVE=y
        }

        function realpath2() {
            test -e "$1" && realpath -e "$1" || {
                    (( ${+commands[$1]} )) && realpath -e "${commands[$1]}"
                }
            ##
            # -e, --canonicalize-existing: all components of the path must exist
            ##
        }
        function rp() {
            realpath2 "$@"
        }
    }

    function nightsh-load-bash() {
        zsh-defer source "$NIGHTDIR"/bash/load-others.bash
    }

    function nightsh-basic-p() {
        test -n "$NIGHTSH_NO_AUTOLOAD_BASH"
    }

    if nightsh-basic-p ; then
        ## tests
        # `env-clean NIGHTSH_NO_AUTOLOAD_BASH=y zsh`
        # `time2 env-clean NIGHTSH_NO_AUTOLOAD_BASH=y zsh -c true` takes about 0.37
        ##
        function nsh() {
            nightsh-load-bash
            unfunction "$0"
        }
    else
        nightsh-load-bash
    fi

    if isKitty ; then
        function kitty-fix-path() {
            preexec_functions[$preexec_functions[(i)kitty-fix-path]]=()
            source ~/.shared.sh
        }
        preexec_functions+=kitty-fix-path
    fi
}
###
nightsh_private_first=~/.nightsh_private_first

if test -e "$nightsh_private_first" && ! source "$nightsh_private_first" ; then
    function nsh() {
        nightsh-load-zshenv
        nightsh-load-bash
        unfunction "$0"
    }
else
    nightsh-load-zshenv
fi

function nightsh-load-local-last() {
    if [ -e ~/.localScripts ] ; then
        re psource ~/.localScripts/**/*.zsh(ND)
    fi
    psource "$HOME/.privateShell"
}
nightsh-load-local-last
