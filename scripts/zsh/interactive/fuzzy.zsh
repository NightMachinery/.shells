### SEE ALSO
# spotlight, spt, spot
###
aliasfn vc code-insiders --reuse-window
function coder() {
    local p="$(<<<$1 sd "$HOME" /home/eva)"
    rgeval code-insiders --reuse-window --remote 'ssh-remote+82.102.11.148' "$p"
}
aliasfn vr veditor=(coder) v
aliasfn vcode veditor=(code-insiders --reuse-window)
aliasfn ve veditor=(emc)
aliasfn vv ve v
##
alias frc='frConfirm=y '
alias cf='frc f'
alias cfr='frc fr'
###
function ffport() {
    doc "[fpFilter=.*] ffport <port> ..."

    local ports=("$@")
    local filter="${fpFilter:-.*}"

    lsport "$ports[@]" | { local line
                           read -d $'\n' -r line
                           ec "$line"
                           command rg "$filter"
    } | fz --with-nth '1,3,5,8,9..' --header-lines 1 | awk '{print $2}'
}
aliasfn ffportl fpFilter=LISTEN ffport
function ffps() {
    # ps auxww: List all running processes including the full command string
    ps auxww | fz --with-nth '11..' --header-lines 1 --query "$*" | awk '{print $2}'
}
function ffkill() {
    doc "alt: fkill; [fkEngine=ffps] ffkill ..."
    local engine="${fkEngine:-ffps}"
    local opts=()
    if [[ "$1" =~ '-\d+' ]] ; then
        opts+="$1"
        shift
    fi
    "$engine" "$@" | inargsf kill $opts[@]
}
aliasfn ffportlkill fkEngine=ffportl ffkill
aliasfn killport ffportlkill
function lsofp() {
    ffps "$@" | inargsf re "lsof -p" | less
    # Old:
    # ppgrep "$@" | fz --header-lines 1 | awk '{print $2}' | inargsf re "lsof -p" | less
}
aliasfn fflsof lsofp
aliasfn plsof lsofp
function fzinw() {
    doc 'fz in words: allows you to select the part of the output you need from a command. (alt: smenu?)'
    iaIFS=$' \t\n\C-@'"'"'(){}"[]' inargss arrN | fz
}
fftmux() {
    local query="$*"
    local engine=(tmux a -t)
    test -n "$ftE[*]" && engine=("$ftE[@]")
    local sessions
    sessions="$(tmux ls|fz --query "$query")" || return $?
    local i
    for i in "${(f@)sessions}"
    do
        [[ $i =~ '([^:]*):.*' ]] && {
            ec "acting on session $match[1]"
            reval "${engine[@]}" "$match[1]"
        }
    done
}
alias fft=fftmux
fftmuxkill() { ftE=(tmux kill-session -t) fftmux }
fr() {
    magic mdoc "frConfirm='' $0 <cmd> [<fd args> ...]" ; mret
    sels=( "${(@f)$(fd "${fd_default[@]}" "${@:2}"|fz --cycle)}" )
    test -n "$sels" && {
        local cmd=("$1 $(gq "${sels[@]}")")
        if test -n "$frConfirm" ; then
            printz $cmd
        else
            eval $cmd
        fi
    }
}
f() fr "$@" --max-depth 1
ffman() {
    # mnf
    man -k . | fz --prompt='Man> ' | awk '{print $1}' | rgx '\(\d+\)$' '' | gxargs -r man
}
alias ffm=ffman
ffaliases() {
    # also see aga
    for k in "${(@k)aliases}"; do
        ec "$k=${aliases[$k]}"
    done | fz --prompt='Aliases> '
}
alias ffa=ffaliases
ffcommands() {
    # cmf (previous name)
    # command finder
    printz "$(agc "${@:-.}" | fz --prompt 'Commands> ')"
}
alias ffc=ffcommands
fffunctions() {
    printz "$(agfunc "${@:-.}" | fz --prompt 'Functions> ')"
}
alias fff=fffunctions
chis() {
    # Forked from fzf's wiki
    # browse chrome history
    local cols sep google_history open
    cols=$(( COLUMNS / 3 ))
    sep='{::}'

    if [ "$(uname)" = "Darwin" ]; then
        google_history="$HOME/Library/Application Support/Google/Chrome/Default/History"
        open=open
    else
        google_history="$HOME/.config/google-chrome/Default/History"
        open=xdg-open
    fi
    cp -f "$google_history" /tmp/h
    local links="$(sqlite3 -separator $sep /tmp/h \
        "select substr(title, 1, $cols), url
     from urls order by last_visit_time desc" |
        awk -F $sep '{printf "%-'$cols's  \x1b[36m%s\x1b[m\n", $1, $2}' |
        ag "${@:-.}" |fz --no-sort --ansi | gsed 's#.*\(https*://\)#\1#')"
    ec "$links"
    ec "$links"|pbcopy
    [[ "${o:-y}" == y ]] && { local i
                              for i in "${(@f)links}"
                              do
                                  $open "$i"
                              done }
    return 0
}
alias ffchrome=chis
function init-vfiles() {
    : GLOBAL vfiles

    if test -n "$*" || test -z "$vfiles[1]" ; then
        local i dirs=( "${(@0)$(arr0 ~/.julia/config ~/.julia/environments $DOOMDIR $NIGHTDIR $cellar $codedir/nodejs $codedir/lua $codedir/python $codedir/uni $codedir/rust | filter0 test -e)}" )
        vfiles=( ${(0@)"$(fd -0 --ignore-file ~/.gitignore_global --exclude node_modules --exclude resources --exclude goog --ignore-case --type file --regex "\\.(${(j.|.)text_formats})\$" $dirs[@] )"} ~/.zshrc ~/.zshenv )
        # for i in "$dirs[@]" ; do
        #     vfiles+=( $i/**/*(.D^+isbinary) )
        # done
    fi

    # --ignore-file seems not working
}
function v() {
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
    files=( ${(0@)"$(<<<"${(F)files}" fz --print0 --query "$*")"} ) || return 1
    local ve="$ve"
    reval "${veditor[@]}" "${(@)files}"
}
function vni() { fr "${veditor[@]}" . $NIGHTDIR }
function rgf() {
    # TODO create a ripgrep parser with json

    # Integration with ripgrep
    local opts=( --delimiter ':|-|\x00' --with-nth 1,3..)
    RG_PREFIX="$functions[rgbase] --null --line-number --no-heading -e"
    local INITIAL_QUERY=( "$@" )

    agC=4 FZF_DEFAULT_COMMAND="$RG_PREFIX $(gq ${INITIAL_QUERY:-.})" \
        fz-empty --reverse --bind "change:reload:$RG_PREFIX {q} || true" ${opts[@]}  --ansi --phony --query "$INITIAL_QUERY"
}
