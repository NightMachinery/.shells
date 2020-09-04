test -z "$ZSH_PROFILEME" || zmodload zsh/zprof # use zprof -c to reset counters
# Does not profile internals of functions well.

set -o vi
export disable_malice=y
export NIGHT_PERSONAL=y
source "$HOME/scripts/zsh/load-first.zsh"
typeset -g NIGHT_NO_EXPENSIVE
isNotExpensive || {
    source ~/.shared.sh

    export HH_CONFIG=hicolor
    #isLinux && export TCLLIBPATH=/usr/lib/x86_64-linux-gnu
    isDarwin && export TCLLIBPATH="/usr/local/lib" # for expect to work
    source <(antibody init)
    ANTIBODY_HOME="$(antibody home)"
    DISABLE_DEFER=y
    # Won't defer if not interactive or disabled explicitly
    { [[ -o interactive ]] && test -z "$DISABLE_DEFER" } && antibody bundle romkatv/zsh-defer || alias zsh-defer=''
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
