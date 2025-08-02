##
function extract-head() {
    local i
    for i in "${=@}" ; do
        if ! [[ "$i" =~ '=' ]] ; then
            ec "$i"
            return 0
        fi
    done
    return 1
}

function h_aliasfn_save {
    typeset -Ag aliases_fn
    aliases_fn[$1]="$2"
}


function h_aliasfn() {
    : "ruu might be needed. Example: aliasfn hi ruu someVar=12"
    local name="$1"
    local body="$@[2,-1]"
    local goesto
    goesto="$(extract-head "$body[@]")"

    functions[$name]="$body "'"$@"'
    test -z "$goesto" || {
        ## This is dangerous. Our own _indir completor would mess up, because the words supplied to it no longer contain the requisites.
        # isI && {
        #     local cf="_$goesto"
        #     (( $+functions[$cf] )) && compdef  "$name"
        # }
        ##
        enh-savename "$name" "$goesto"
    }

    h_aliasfn_save "$name" "$body"
}
# enh-savename aliasfn h_aliasfn # redundant, we will auto-erase noglob ourselves
alias aliasfn='\noglob h_aliasfn'

function h_aliasfnq() {
    local name="$1"
    local goesto=""
    local body=("$@[2,-1]") qbody=""
    local i flag=''
    while true ; do
        if [[ "$body[1]" =~ '^([^=]*)=(.*)$' ]] ; then
            qbody="${qbody}${match[1]}=$(gq "$match[2]") "
            shift body
        else
            goesto="$body[1]"
            qbody="${qbody}$(gq "$body[@]")"
            break
        fi
    done

    enh-savename "$name" "$goesto" || true
    h_aliasfn "$name" "$qbody" # do not use `fnswap enh-savename true`, it causes a slowdown
}
alias aliasfnq='\noglob h_aliasfnq'

function h_aliasfn-ng() {
    aliasfn "$@"
    noglobfn "$1"
}
alias aliasfn-ng='\noglob h_aliasfn-ng'

function h_aliasfnq-ng() {
    aliasfnq "$@"
    noglobfn "$1"
}
alias aliasfnq-ng='\noglob h_aliasfnq-ng'

function aliasfn-classic() {
    local args=( "$@" )
    [[ "$args[*]" =~ '\s*([^=]+)=(.*[^\s])\s*' ]] || { echo invalid alias: "$args[*]" >&2 ; return 1 }
    # run-on-each dvar args match
    aliasfn "$match[1]" "$match[2]"
}
aliasfn alifn aliasfn-classic
function aliassafe() {
    # Alt: aliassafe2
    builtin alias "$@"
    aliasfn-classic "$@"
}
function aliassafe2() {
    local head="$1"
    local body=("${@[2,-1]}")
    local bodyquoted="$(gq "$body[@]")"

    builtin alias "$head"="$bodyquoted"
    aliasfnq "$head" "$body[@]"
}
##
function createglob {
    local from="$1"
    local to="$2"
    { test -z "$from" || test -z "$to" } && {
        ecerr "$0: insuffient arguments supplied. (needs 2)"
        return 1
    }
    eval $to'="*.(${(j.|.)'$from'})(.DNn)"'
    # n (NUMERIC_GLOB_SORT):  If numeric filenames are matched by a filename generation pattern,  sort the filenames numerically rather than lexicographically.
    # e.g., =b-3.png= comes before =b-10.png=
}
##
function ensure-var-name {
    local var="$1"

    # variable names can't contain '-' or '.'
    typeset -g ${var}=${(P)var//-/_}
    typeset -g ${var}=${(P)var//./_}
}
##
function h_@gather() {
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
        ensure-var-name current_name
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
# alias @gather='\noglob h_@gather'
aliasfn @gather h_@gather # we don't need the noglob, so why force it downstream?
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

function h_@opts {
    local prefix="${magic_opts_prefix:-magic}"
    @gather "$@"
    local cmd=( "$magic_cmd[@]" )
    unset magic_cmd
    [[ "$prefix" == magic ]] && {
        prefix="${magic_opts_prefixes[$cmd[1]]:-$cmd[1]}_"
        ensure-var-name prefix
    }
    ecdbg "magic opts final prefix: $prefix"
    set -- "$magic_gathered_vars[@]"
    unset magic_gathered_vars
    
    if (( $#@ % 2 != 0 )) ; then
        ectrace "$0: needs an even number of arguments (key-value pairs). Aborting."
        return 1
    fi
    local var varval var2 var2val setcmd varname
    # ecdbg "$0 magic vars: $@"
    while (( $#@ != 0 )) ; do
        # ecdbg "entered opts loop"
        var="${1}"
        ensure-var-name var
        shift
        varval=( "${(P@)var}" )
        unset "$var"
        varval="$varval[*]"
        [[ "$varval" =~ '^\s*$' ]] && {
            ecerr "$0: empty key supplied. Aborting."
            return 1
        }
        var2="${1}"
        ensure-var-name var2
        shift
        var2val=( "${(P@)var2}" )
        unset "$var2"
        varname="${prefix}${varval}"
        varname="${varname}"
        ensure-var-name varname
        unset "$varname"
        ##
        if (( ${#var2val} == 1 )) ; then
            setcmd="local -x ${varname}=$(gq "$var2val[@]")"
            # @warn using `typeset -ag var` will reset the single var; Use ensure-array-var instead.
        else
            setcmd="local -a ${varname}=( $(gq "$var2val[@]") )"
            # can't export arrays: `a=(1 2 3) zsh -fc 'typeset -p a'`
        fi
        ##
        # setcmd="typeset -x -T ${varname} ${varname:u}=( $(gq "$var2val[@]") ) $'\n'" # @docs `man zshbuiltins`
        # @warn using `typeset -ag var` will reset this
        ##
        ecdbg "setcmd: $setcmd"
        eval "$setcmd" || {
            ecerr "$0: Assigning the key '$varval' failed. setcmd: $setcmd"$'\n'"Aborting."
            return 1
        }
    done

    reval "$cmd[@]"

    ## tests:
    # See 'iloop-chis' for a nested usage example
    ##
}
aliasfn @opts h_@opts
function @opts-setprefix () {
    typeset -A -g magic_opts_prefixes
    # test -n "${magic_opts_prefixes[$1]}" ||
    magic_opts_prefixes[$1]="$2"
}
function @opts-setprefixas () {
    typeset -A -g magic_opts_prefixes
    local pre="${magic_opts_prefixes[$2]}"
    test -z "$pre" && {
        # ecerr "$0: ${(q+)2} doesn't have any prefix set."
        # return 1
        ##
        pre="$2"
    }
    magic_opts_prefixes[$1]="$pre"
}
function opts-test1() {
    # typ path
    typeset -p "opts_test1_path"
    ec "${opts_test1_extension:-${opts_test1_e:-default}}"
    typeset -p opts_test1_animal
    arger "$@"
}
@opts-setprefix opts-test2 lily
function opts-test2() {
    # typ path
    typeset -p "lily_path"
    ec "${lily_extension:-${lily_e:-default}}"
    typeset -p lily_animal
    arger "$@"
}
##
