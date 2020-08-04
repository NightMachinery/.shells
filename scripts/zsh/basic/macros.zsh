##
function _aliasfn() {
    : "ruu might be needed. Example: aliasfn hi ruu someVar=12"
    local name="$1"
    local goesto="$2"
    local body="$@[2,-1]"

    functions[$name]="$body "'"$@"'
    enh-savename "$name" "$goesto"
}
# enh-savename aliasfn _aliasfn # redundant, we will auto-erase noglob ourselves
alias aliasfn='\noglob _aliasfn'
function _aliasfnq() {
    local name="$1"
    local goesto="$2"
    local body=("$@[2,-1]")

    fnswap enh-savename true _aliasfn "$name" "$(gq "${body[@]}")"
    enh-savename "$name" "$goesto"
}
alias aliasfnq='\noglob _aliasfnq'

function _aliasfn-ng() {
    aliasfn "$@"
    noglobfn "$1"
}
alias aliasfn-ng='\noglob _aliasfn-ng'
function _aliasfnq-ng() {
    aliasfnq "$@"
    noglobfn "$1"
}
alias aliasfnq-ng='\noglob _aliasfnq-ng'

function aliasfn-classic() {
    local args=( "$@" )
    [[ "$args[*]" =~ '\s*([^=]+)=(.*[^\s])\s*' ]] || { echo invalid alias: "$args[*]" >&2 ; return 1 }
    run-on-each dvar args match
    aliasfn "$match[1]" "$match[2]"
}
aliasfn alifn aliasfn-classic
function aliassafe() {
    builtin alias "$@"
    aliasfn-classic "$@"
}
##
function createglob() {
    local from="$1"
    local to="$2"
    { test -z "$from" || test -z "$to" } && {
        ecerr "$0: insuffient arguments supplied. (needs 2)"
        return 1
    }
    eval $to'="*.(${(j.|.)'$from'})(.D)"'
}
##
function _@gather() {
    # GLOBALS: OUTPUT: magic_cmd magic_gathered_vars magic_gathered_*
    magic_gathered_vars=() magic_cmd=() # GLOBAL

    ##
    # - `@macrogather [@EOF_UUID] sth sth ... ( array-elem ... ) sth [EOF_UUID]@ ...`
    # - `key )` `key ( ( )` for escaping
    # - optionally `[EOF_UUID]@cmd` to `[EOF_UUID]@ cmd`.

    # test:
    # @gather-reval 1 2 3 hi 4 wow [ "hello world" nii '' 78 bomb ] end [ ] '' boo ] [ [ ] @ eval ' arrN "$magic_gathered_7[@]"'
    # @gather-reval @MERMAID 1 2 3 hi 4 wow [ "hello world" nii '' 78 bomb ] end [ ] '' boo ] [ [ ] MERMAID@ eval ' arrN "$magic_gathered_7[@]"'
    ##

    # local args=("$@")



    local ARRAY_START='[' # parens have parse problems in zsh
    local ARRAY_END=']'
    local VAR_PREFIX='magic_gathered_'
    local MEOF="@"
    if [[ "$1" =~ '^@(.*)' ]] ; then
        MEOF="$match[1]$MEOF"
        shift
    fi
    local key i=1 current_name
    while true ; do
        (( $#@ == 0 )) && {
            ecerr "$0: No arguments remaining, but MEOF not recieved. Aborting."
            return 1
        }
        key="$1"
        shift
        if [[ "$key" == "$MEOF" ]] ; then
            magic_cmd=( "$@" )
            break
        fi
        current_name="${VAR_PREFIX}$i"
        unset $current_name
        i=$(($i+1))
        if [[ "$key" == "$ARRAY_START" ]] ; then
            # ecdbg "ARRAY_START encountered"
            local vals=()
            while true ; do
                (( $#@ == 0 )) && {
                    ecerr "$0: No arguments remaining, but ARRAY_END not recieved. Aborting."
                    return 1
                }
                val="$1"
                shift
                if [[ "$val" == "$ARRAY_END" ]] ; then
                    # ecdbg "ARRAY_END encountered"
                    # ec "$(typeset -p vals)"
                    # eval "$(typeset -p vals)"
                    eval "typeset -ga ${current_name}=( $(gq "$vals[@]") )"
                    break
                else
                    vals+="$val"
                fi
            done
        else
            typeset -g $current_name=$key
        fi
        magic_gathered_vars+="$current_name"
    done
}
# alias @gather='\noglob _@gather'
aliasfn @gather _@gather # we don't need the noglob, so why force it downstream?
function @gather-reval() {
    @gather "$@"
    {
        local cmd=( "$magic_cmd[@]" )
        unset magic_cmd
        reval "$cmd[@]"
    } always {
        unset magic_gathered_vars
    }
}
function _@opts() {
    local prefix="${magic_opts_prefix:-magic}"
    @gather "$@"
    local cmd=( "$magic_cmd[@]" )
    unset magic_cmd
    [[ "$prefix" == magic ]] && {
        prefix="${magic_opts_prefixes[$cmd[1]]:-$cmd[1]}_"
        prefix="${prefix//-/_}" # variables can't have - in their name
    }
    ecdbg "magic opts final prefix: $prefix"
    set -- "$magic_gathered_vars[@]"
    unset magic_gathered_vars
    
    if (( $#@ % 2 != 0 )) ; then
        ecerr "$0: needs an even number of arguments (key-value pairs). Aborting."
        return 1
    fi
    local var varval var2 var2val setcmd varname
    # ecdbg "$0 magic vars: $@"
    while (( $#@ != 0 )) ; do
        # ecdbg "entered opts loop"
        var="$1"
        shift
        varval=( "${(P@)var}" )
        unset "$var"
        varval="$varval[*]"
        [[ "$varval" =~ '^\s*$' ]] && {
            ecerr "$0: empty key supplied. Aborting."
            return 1
        }
        var2="$1"
        shift
        var2val=( "${(P@)var2}" )
        unset "$var2"
        varname="${prefix}${varval}"
        unset "$varname"
        setcmd="typeset -a ${varname}=( $(gq "$var2val[@]") )"
        ecdbg "setcmd: $setcmd"
        eval "$setcmd" || {
            ecerr "$0: Assigning the key '$varval' failed. setcmd: $setcmd"$'\n'"Aborting."
            return 1
        }
    done

    reval "$cmd[@]"
}
aliasfn @opts _@opts
function @opts-setprefix () {
    typeset -A -g magic_opts_prefixes
    # test -n "${magic_opts_prefixes[$1]}" ||
    magic_opts_prefixes[$1]="$2"
}
function opts-test1() {
    # typ path
    typ "opts_test1_path"
    ec "${opts_test1_extension:-${opts_test1_e:-default}}"
    typ opts_test1_animal
    arger "$@"
}
@opts-setprefix opts-test2 lily
function opts-test2() {
    # typ path
    typ "lily_path"
    ec "${lily_extension:-${lily_e:-default}}"
    typ lily_animal
    arger "$@"
}
##
