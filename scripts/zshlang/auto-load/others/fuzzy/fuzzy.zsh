### SEE ALSO
# spotlight, spt, spot
###
# alias frc='frConfirm=y '
# alias cf='frc fi-d1'
# alias cfr='frc fi-rec'
function fr {
    (( $#@ == 0 )) && return 1
    (( $#@ == 1 )) && set -- "$@" ''
    @opts query "${@[-1]}" @ fi-rec "$(gq "${@[1,-2]}")"
    # hist-add-self
}

function fi-rec() {
    magic mdoc "[frConfirm='' frWidget=''] $0 <cmd> [<fd args> ...]
This function uses eval-memoi." ; mret

    local args=("${@:2}") query="$(fz-createquery $fi_rec_query[@])"
    local cmdhead="$1"
    local dir=.
    dir="$(realpath "$dir")" @TRET

    bella_zsh_disable1

    {
    if test -n "$frWidget" ; then
          zle-print-dots
    fi
    ##
    # reval-onhold wastes ~0.8s, so it's not worth it
    ##
    sels=( "${(@f)$(memoi_skiperr=y memoi_override_duration=0.3 eval-memoi fd "${fd_default[@]}" "${args[@]:-.}" "$dir" | prefixer --skip-empty -r "${dir}/" | fz --cycle --query "$query" | prefixer --skip-empty -a "${dir}/")}" ) @RET


    test -n "$sels" && {
        if test -n "$frWidget" ; then
            LBUFFER="$LBUFFER$(gq "$sels[@]")"
        else
            local cmd=("$cmdhead $(gq "${sels[@]}")")
            if test -n "$frConfirm" ; then
                printz $cmd
            else
                geval $cmd
            fi
        fi
    }
    } always {
        if test -n "$frWidget" ; then
            zle redisplay
        fi
    }
}
aliasfn fr_zle frWidget=y fi-rec
aliasfn fr_zle_deus deusvult=y frWidget=y fi-rec
function fi-d1() fi-rec "$@" --max-depth 1 # freeing up f
##
function ffport() {
    doc "[fpFilter=.*] ffport <port> ..."

    local ports=("$@")
    local filter="${fpFilter:-.*}"

    bella_zsh_disable1

    lsport "$ports[@]" | { local line
                           read -d $'\n' -r line
                           ec "$line"
                           command rg "$filter"
    } | fz --with-nth '1,3,5,8,9..' --header-lines 1 | awk '{print $2}'
}
aliasfn ffportl fpFilter=LISTEN ffport

function ffps() {
    local query="$(fz-createquery "$@")"

    bella_zsh_disable1

    # ps auxww: List all running processes including the full command string
    ps-list | fzp --with-nth '11..' --header-lines 1 "$query" | awk '{print $2}'
}

function ps-list {
    command ps auxww
}

function ffps-c1() {
    local query="$(fz-createquery "$@")"

    bella_zsh_disable1

    ps-children 1 | inargsf command ps -fp | fzp --with-nth '8..' --header-lines 1 "$query" | gawk '{print $2}'
}
##
function ffkill() {
    doc "alt: fkill; [fkEngine=ffps] ffkill ..."
    doc "Tip: fnswap kill 'sudo kill'"

    bella_zsh_disable1

    local engine=("${(@)fkEngine:-ffps}")
    local kill_engine=("${(@)ffkill_ke:-kill-withchildren}")
    ## Abandoned designs
    # local kg="${ffkill_group}" # kill the whole process group. (https://unix.stackexchange.com/a/14853/282382)
    # test -n "$kg" && kg='-'
    # | inargsf revaldbg mapg "${kg}"'${i}'
    ##

    local opts=()
    if [[ "$1" =~ '^-\S+$' ]] ; then
        opts+="$1"
        shift
    fi
    "$engine" "$@"  | inargsf revaldbg "$kill_engine[@]" $opts[@]
}
aliasfnq ffkill-super fnswap kill 'sudo kill' ffkill -9
aliasfn fk ffkill
aliasfn ffkill-c1 fkEngine=ffps-c1 ffkill
aliasfn fks ffkill-super
aliasfnq fk-joker ffkill "'JOKER_MARKER"
##
aliasfn ffportkill fkEngine=ffport ffkill
aliasfn ffportlkill fkEngine=ffportl ffkill
aliasfn killport ffportkill
##
function lsofp() {
    ffps "$@" | inargsf re "lsof -p" | less
    # Old:
    # ppgrep "$@" | fz --header-lines 1 | awk '{print $2}' | inargsf re "lsof -p" | less
}
aliasfn fflsof lsofp
aliasfn plsof lsofp
##
function fftmux() {
    local query="$*"
    local engine=(tmux a -t)
    test -n "$ftE[*]" && engine=("$ftE[@]")

    bella_zsh_disable1

    local sessions
    sessions="$(tmux ls|fz --query "$query")" || return $?
    local i
    for i in "${(f@)sessions}"
    do
        [[ $i =~ '([^:]*):.*' ]] && {
            ecgray "acting on session $match[1]"
            tty-title "${match[1]}"
            reval-ec "${engine[@]}" "$match[1]"
        }
    done
}
alias fft=fftmux

function fftmuxkill() {
    ecgray "$0: using fftmux-session-processes-kill is usually better than this"

    ftE=(tmux kill-session -t) fftmux "$@"
}

function fftmux-name() { ftE=(ec) fftmux "$@" }

function tmux-pane-list {
    : "Usage: <session-name>"

    local out="${tmux_pane_list_o:-#{pane_id}}"
    tmux lsp -s -F"$out" -t "$@"
}

function tmux-pane-list-pid {
    : "Usage: <session-name>"

    @opts o '#{pane_pid' @ tmux-pane-list "$@"
}

function fftmux-pane-list {
    : "Usage: <session-name>"

    ftE='tmux-pane-list' fftmux
}

function fftmux-pane-list-pid {
    : "Usage: <session-name>"

    ftE='tmux-pane-list-pid' fftmux
}

function tmux-pane-restart-nokill {
    : "Usage: <pane-id>"
    : "This command only restarts the tmux pane; The processes might still live beyond their dead pane."

    tmux respawn-pane -kt "$@"
}

function tmux-session-processes-kill {
    local kill_opts=()
    if [[ "$1" == -* ]] ; then
        kill_opts+="$1"
        shift
    fi
    local sessions=("$@")

    local s
    for s in ${(@f)sessions} ; do
        ecgray $'\n'"$0: killing the processes of session $(gquote-sq "$s"):"
        local panes
        pane_pids="$(tmux-pane-list-pid "$s")"

        local p
        for p in ${(@f)pane_pids} ; do
            reval-ec kill-withchildren "$kill_opts" "$p"
        done
    done
}

function tmux-session-restart {
    local kill_opts=()
    if [[ "$1" == -* ]] ; then
        kill_opts+="$1"
        shift
    fi
    local sessions=("$@")

    local s
    for s in ${(@f)sessions} ; do
        ecgray $'\n'"$0: restarting session $(gquote-sq "$s"):"
        local panes
        panes="$(tmux-pane-list "$s")"
        pane_pids="$(tmux-pane-list-pid "$s")"

        local p
        for p in ${(@f)pane_pids} ; do
            reval-ec kill-withchildren "$kill_opts" "$p"
        done
        for p in ${(@f)panes} ; do
            reval-ec tmux-pane-restart-nokill "$p"
        done
    done
}

function fftmux-session-restart {
    local engine=("${fftmux_engine[@]:-tmux-session-restart}") kill_opts=()
    if [[ "$1" == -* ]] ; then
        kill_opts+="$1"
        shift
    fi
    local q="$*"

    bella_zsh_disable1

    local session
    sessions="$(fftmux-name "$q")" @RET
    "$engine[@]" "$kill_opts[@]" "$sessions[@]"
}
alias fftr='fftmux-session-restart'

function fftmux-session-processes-kill {
    fftmux_engine=(tmux-session-processes-kill) fftmux-session-restart "$@"
}
alias fftk='fftmux-session-processes-kill'
##
ffman() {
    # mnf
    ##
    bella_zsh_disable1

    man -k . | fzf_mru_context="$0" fz --prompt='Man> ' | awk '{print $1}' | rgx '\(\d+\)$' '' | gxargs -r man
}
alias ffm=ffman
###
function init-vfiles() {
    : GLOBAL vfiles

    if test -n "$*" || test -z "$vfiles[1]" ; then
        local i dirs=( "${(@0)$(arr0 ~/.julia/config ~/.julia/environments $DOOMDIR $NIGHTDIR $cellar $codedir/nodejs $codedir/lua $codedir/python $codedir/uni $codedir/rust $codedir/golang | filter0 test -e)}" )
        vfiles=( ${(0@)"$(fd -0 --ignore-file ~/.gitignore_global --exclude node_modules --exclude resources --exclude goog --ignore-case --type file --regex "\\.(${(j.|.)text_formats})\$" $dirs[@] )"} ~/.zshrc ~/.zshenv )
        # for i in "$dirs[@]" ; do
        #     vfiles+=( $i/**/*(.D^+isbinary) )
        # done
    fi

    # --ignore-file seems not working
}
aliasfn vinit init-vfiles yes
function v() {
    bella_zsh_disable1

    local q="$* "
    # local q="$(fz-createquery "$@")"

    local emacs_root=~/.emacs.d.doom/.local # normal emacs doesn't have this .local part
    local emacs_recent="$emacs_root/.cache/recentf"
    : GLOBAL vfiles
    init-vfiles

    local files excluded=( ~/.zlua ~/.zsh_history "$HOME/.local/share/nvim/shada/main.shada" "$HOME/Library/Application Support/Code/storage.json" "$HOME/Library/Application Support/Code - Insiders/storage.json" "$HOME/Library/Application Support/Google/Chrome/Local State" )
    files=( "$vfiles[@]" )
    local code="$HOME/Library/Application Support/Code - Insiders/storage.json"

    test -f "$code" && {
        command rg --only-matching --replace '$1' '"file://(.*)"' "$code" |
            while read -d $'\n' -r line; do
                [ -e "$line" ] && {
                    files+="$line"
                    ecdbg "vscode: $line"
                }
            done
    }

    command rg '^>' ~/.viminfo | cut -c3- |
        while read -d $'\n' -r line; do
            line="${line/\~/$HOME}"
            [ -f "$line" ] && files+="$line"
        done
    test -f "$emacs_recent" && {
        command rg --only-matching --replace '$1' '^\s*"(.*)"$' "$emacs_recent" |
            while read -d $'\n' -r line; do
                [ -f "$line" ] && files+="$line"
            done
    }
    # files=( ${(@)files//\~/$HOME} ) # emacs doesn't need this
    # stat doesn't expand ~
    # sort files by modification date
    # %Y     time of last data modification, seconds since Epoch
    # reing gstat is needed if the files get too numerous, but then things will be too slow
    files=( ${(0@)"$(gstat  --printf='%040.18Y:%n\0' "${(@)files:|excluded}" | gsort --reverse --zero-terminated --unique | gcut -z -d':' -f2-)"} ${(@)excluded} ) #Don't quote this there is always a final empty element
    files=( ${(0@)"$(<<<"${(F)files}" fzf_mru_context="$0" fz --print0 --query "$q")"} ) || return 1
    local ve="$ve"
    reval "${veditor[@]}" "${(@)files}"
}
# function vni() { fr "${veditor[@]}" . $NIGHTDIR }
##
# vc: Visual Code
# this opens a new window for me when the first window opened was not by itself (perhaps a macOS bug?)
aliasfn vc code-insiders --reuse-window --add # --add: Add a folder or multiple folders to the last active VS Code instance for a multi-root workspace.
function coder() {
    bella_zsh_disable1

    local p="$(<<<$1 sd "$HOME" /home/${lilf_user})"
    rgeval code-insiders --reuse-window --remote 'ssh-remote+82.102.11.148' "$p"
}
aliasfn vcr coder
##
aliasfn vr veditor=(coder) v # v remote
aliasfn ve-code veditor=(code-insiders --reuse-window)
aliasfn ve-emc veditor=(emc-gateway)
aliasfn vv ve-emc v
###
function vp-ls() {
    arrN ~/Downloads/**/*.pdf ~/Base/_Books/**/*.pdf
}
function vp() {
    # v pdf
    ##
    bella_zsh_disable1

    local q="$* "
    # local q="$(fz-createquery "$@")"

    vp-ls | fzf_mru_context="$0" fz-rtl --query "$q" | sponge | inargsf open
}
##
function fuzzy-choose() {
    bella_zsh_disable1

    : "Usage: arrN <choice> ... | fuzzy-choose query # returns candidates in order"

    fuzzyChoose.js "$*" | jqm '.[] | .[1]'
}
##
function fzinw() {
    bella_zsh_disable1

    doc 'fz in words: allows you to select the part of the output you need from a command. (alt: smenu?)'

    local q="$(fz-createquery "$@")"

    local res
    res="$(iaIFS=$' \t\n\C-@'"'"'(){}"[]' inargss arrN | fzp "$q")" || return 1
    ec-copy "$res"
}
aliasfn ffin-words fzinw
##
