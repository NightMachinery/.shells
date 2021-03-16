function rtl-reshaper() {
    rtl_reshaper.py "$@"
}
function reval-rtl() {
    reval "$@" | rtl-reshaper
}
aliasfn rtl reval-rtl
##
function fz-rtl() {
    fz "$@" --preview "$FZF_RTL_PREVIEW"
}
function fz-rtl1() {
    # @warn The reshaped Persian text seemingly uses special chars so we can no longer search it using our normal Persian keyboard.
    local input="$(cat)" q="$1" opts=("${@[2,-1]}")

    local sels_i="$(ec "$input" | rtl-reshaper | cat -n | fz --with-nth 2.. --query "$q" "$opts[@]" | awk '{print $1}')"
    gawk 'NR == FNR {nums[$1]; next} FNR in nums' <(ec "$sels_i") <(ec "$input")
}
##
