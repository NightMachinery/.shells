### Seal Module
# imports basics fuzzy
# imports string.zsh
# imports binaries sd, fzf, perl

## vars
test -z "$attic_dir" && attic_dir="$cellar/attic/"
test -z "$attic" && attic="$attic_dir/.darkattic"
test -z "$attic_todo" && attic_todo="$attic_dir/.attic_todo"
test -z "$attic_temoji" && attic_temoji="$attic_dir/.temojis"

### aliases
## todo
alias attic_todo='attic="$attic_todo" un_p=y un_no_preview=y'
alias todo='attic_todo seal'
alias todos='attic_todo unseal'
alias todone='attic_todo exor'
alias todo-import='attic_todo seal-import'
alias todo-import-NUL='attic_todo seal-import-NUL'
alias td=todo
alias tds='cat "$attic_todo"'
# alias ts=todos #CONFLICTING_NAME
alias tn=todone
alias tdi=todo-import
## temoji
alias attic_temoji='attic="$attic_temoji" un_p=y un_no_preview=y'
temoji() {
    attic_temoji unseal-split1 "$@"  |tee /dev/tty |pbcopy
}
alias temoji-add='attic_temoji seal'
alias temoji-rm='attic_temoji exor'
alias ej='temoji'
### Extra
unseal-split1() {
    doc Returns the first part of a newline separated record from unseal. Skips empty lines.
    local i flag=0 res=''
    for i in "${(f@)$(unseal "$@")}"
    do
        # re dvar i flag res
        [[ -z "$i" ]] && continue
        flag=$((flag+1))
        (( $flag == 1 )) && {
            res="$res $i"
        }
        (( $flag == 2 )) && flag=0
    done
    ec "$res"
}
## core
seal() {
    doc Use with 'uns' to store and retrieve one-liners
    doc Use exor to remove seals.
    local n=''
    ! test -e "$attic" || n=$''
    print -r -n -- "$n$(in-or-args "$@")"$'\n' >> "$attic"
}
alias uns=unseal
unseal() {
    doc unseal
    doc in: un_fz un_p
    re 'ecdbg un_fz:'  "$un_fz[@]"
    local other_options fz_no_preview
    other_options=()
    test -n "$un_no_preview" || {
        other_options+=(--preview "$FZF_SIMPLE_PREVIEW" --preview-window hidden)
        fz_no_preview=y
        }
    local l="$(<"$attic[@]" RS2NUL | fz $un_fz[@] $other_options[@] --read0 --tac --no-sort -q "${*:-}")"
    test -n "$l" && {
        { [[ "$l" != (@|\#)* ]] && test -z "$un_p" } && printz "$l" || ec "$l"
        ec "$l"|pbcopy
        return 0
    } }
exor() {
    doc exorcize seals :D
    local sels="$(un_p=y un_fz=( --print0 ) uns "$@")"
    test -n "$sels" && {
        local i items
        ecdbg sels: "$sels"
        ecdbg items: "$items[@]"
        for i in "${(@0)sels}"
        do
            test -n "$i" || continue
            ec Exorcizing 🏺
            cat -v <<<"$i"
            # sd --string-mode "$i" '' "$attic" #--flags m 
            FROM="$i" perl -0777 -pi -e 's/(|\A)\Q$ENV{FROM}\E(?<sep>|\Z)/$+{sep}/gm' "$attic"
            # comment "This actually doesn't do the 'g' part completely. It removes some occurences, but not all."
            # comment "because each replacement eats both the NUL before and after the match, so when perl resumes searching for the next match, it won't find the immediately following match because of NUL before it missing. You'd need to use a look-ahead operator that don't eat the NULs but still check they're there."
            # FROM="$i" perl -0lni -e 'print if $_ ne $ENV{FROM}' -- "$attic"
            # FROM="$i" perl ni -e 'print if $_ ne $ENV{FROM}' -- "$attic"
        done
        perl -0777 -pi -e 's/\A//' "$attic"
        # perl -pi -e 's/\0\Z//' "$attic"
    }
}
alias seali=seal-import
seal-import() {
    local inp="$(in-or-args "$@")"
    re seal "${(@f)inp}"
}
seal-import-NUL() {
    local inp="$(in-or-args "$@")"
    # dact arger "${(@0)inp}"
    re seal "${(@0)inp}"
}
