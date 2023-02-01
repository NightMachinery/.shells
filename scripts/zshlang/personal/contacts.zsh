##
function phone-number-ir-to-98 {
    in-or-args "$@" |
        sd '^0' '+98' |
        cat-copy-if-tty
}
##
function contacts-see {
    local files
    files=(
        ~[big_wallet]/'me, self/contacts/main.vcf'
    )

    ##
    # indir "${${files[1]}:h}" sees-ug "$*"
    ##
    indir "${${files[1]}:h}" sees "$@"
    ##
    # cat "$files[@]" \
    #     | ugrep_c="${ugrep_c:-5}" ugbool-context "$@"
    ##
}

function contacts-see-number {
    local query="$*"
    query="$(ecn $query | per2en)" @TRET
    query="$(ecn $query | prefixer -i '' -o '\s*')" @TRET
    query="^(?:item\d*\.)?tel(?:;|:).*${query}"

    revaldbg contacts-see "$query"
}
alias csn='contacts-see-number'
##
