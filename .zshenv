###
alias zsh-defer=''
##
function psource() {
    if [[ -r "$1" ]]; then # -r: readable file
        source "$@"
    fi
}

##
function nightsh-load-zshenv {
    ZSH_PWD_CACHE=~/tmp/.zsh_pwd
    function zsh-pwd-save {
        if test -z "$HISTFILE" ; then
            return 0
        fi

        print -r -- $PWD > "$ZSH_PWD_CACHE"
    }
    function zsh-pwd-load {
        local d

        ##
        d="${ZSH_PWD}"
        unset ZSH_PWD
        ##

        if test -z "$d" ; then
            d="$(cat "$ZSH_PWD_CACHE" 2>/dev/null)" || return 0
        fi

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
        typeset -g antibody_enabled=''
        if test -n "$antibody_enabled" ; then
            source <(antibody init)
            typeset -g ANTIBODY_HOME="$(antibody home)"
            typeset -g plugin_dir="$ANTIBODY_HOME"
        fi

        function zinit-start() {
            if [[ ! -f $HOME/.zinit/bin/zinit.zsh ]]; then
                print -P "%F{33}▓▒░ %F{220}Installing %F{33}DHARMA%F{220} Initiative Plugin Manager (%F{33}zdharma/zinit%F{220})…%f"
                command mkdir -p "$HOME/.zinit" && command chmod g-rwX "$HOME/.zinit"
                command git clone https://github.com/zdharma-continuum/zinit "$HOME/.zinit/bin" && \
                    print -P "%F{33}▓▒░ %F{34}Installation successful.%f%b" || \
                    print -P "%F{160}▓▒░ The clone has failed.%f%b"
            fi

            source "$HOME/.zinit/bin/zinit.zsh"
            autoload -Uz _zinit
            (( ${+_comps} )) && _comps[zinit]=_zinit

            typeset -g plugin_dir="$HOME/.zinit/plugins/"
        }

        function zinit-update() {
            local job_count="$1" # @optional

            zinit self-update
            zinit update --parallel $job_count
        }


        typeset -g zinit_enabled='y'
        if test -n "$zinit_enabled" ; then
            zinit-start
        fi

        function source-plugin() {
            if test -n "$source_plugin_skip" ; then
                return 0
            elif test -n "$antibody_enabled" ; then
                antibody bundle "$@"
            elif test -n "$zinit_enabled" ; then
                zinit light "$@"
            fi
        }

        typeset -g DISABLE_DEFER=y
        # Won't defer if not interactive or disabled explicitly
        if { [[ -o interactive ]] && test -z "$DISABLE_DEFER" } ; then
            unalias zsh-defer # @warn this is too late to affect the code already loaded
            source-plugin romkatv/zsh-defer
        fi

        function source-interactive-all() {
            source "$NIGHTDIR"/zshlang/interactive/first.zsh
            run-on-each source "$NIGHTDIR"/zshlang/interactive/auto-load/**/*(.)
            source "$NIGHTDIR"/zshlang/interactive/completions.zsh # needs to be semi-last
            typeset -g NIGHT_NO_EXPENSIVE
            NIGHT_NO_EXPENSIVE=y
        }

        function realpath2 {
            test -e "$1" && grealpath -e -- "$1" || {
                    (( ${+commands[$1]} )) && grealpath -e -- "${commands[$1]}"
                }
            ##
            # -e, --canonicalize-existing: all components of the path must exist
            ##
        }
        function rp {
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
