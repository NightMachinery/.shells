aliasfnq hb265sharp_strongest  fnswap printz-quoted reval re "hbjson $NIGHTDIR'/configFiles/handbrake/H265 ising (strongest).json'"
##
function hbjson() {
  local preset="$1" ; shift || return 1
  local input="$1" ; shift || return 1
  local args=( "$@" )
  local ext="${hbjson_ext:-mp4}" # @opts

  local presetName="${preset:t:r}"
  local output="${input:r}_${presetName}.$ext"
  # FNSWAP: printz-quoted ; rather bad API?
  printz-quoted HandBrakeCLI --preset-import-file "$preset" --preset "$presetName" "$args[@]" -i "$input" -o "$output"
}
function ffhb() {
  local preset
  preset="$(arrN $NIGHTDIR/configFiles/handbrake/*.json | fz)" || return 1
  printz-quoted hbjson "$preset" "$@"
}