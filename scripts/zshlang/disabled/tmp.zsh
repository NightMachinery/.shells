##
function fz-rtl1() {
    local input="$(cat)" q="$1" opts=("${@[2,-1]}")

    local sels_i="$(echo "$input" |
                     fribidi --nobreak |
                     cat -n |
                     fzf --with-nth 2.. --query "$q" "$opts[@]" |
                     awk '{print $1}')"
    awk 'NR == FNR {nums[$1]; next} FNR in nums' <(ec "$sels_i") <(ec "$input")
}
