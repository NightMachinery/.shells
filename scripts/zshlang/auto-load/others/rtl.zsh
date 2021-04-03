function rtl-reshaper-py() {
  if isRtl ; then
      cat
  else
      rtl_reshaper.py "$@"
  fi
}
function rtl-reshaper() {
    if isRtl ; then
        cat
    else
        rtl_reshaper_rs "$@"
    fi
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

    local sels_i="$(ec "$input" | rtl-reshaper | cat -n | fz --with-nth 2.. --query "$q" "$opts[@]" | gawk '{print $1}')"
    gawk 'NR == FNR {nums[$1]; next} FNR in nums' <(ec "$sels_i") <(ec "$input")
}
##
function biconm() {
  BICON_MODE=y bicon.bin "$@"
}
function bicon-emc() {
  if isBicon ; then
    ecerr "You're already in Bicon mode. Editing RTL text will be 'reversed'. Use a clean session instead."
    {
    ( emc -e "(progn (setq-default bidi-display-reordering nil) (redraw-display))" "$@" )
    } always {
      emacsclient -e "(setq-default bidi-display-reordering t)"
    }
    return $?
  fi
  $proxyenv biconm --reshape-only emacsclient -t "$@"
  reset
}
function bicon-zsh() {
  biconm zsh
  reset
}
##
