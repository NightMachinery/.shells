##
function hb265 {
  local i="$1" quality="${hb265_quality:-${hb265_q:-18}}"
  local o="${2:-${1:r}_h265.mp4}"
  ensure-array hb_opts
  local opts=("${hb_opts[@]}")

  if isAppleSilicon ; then
    ecgray "$0: hardware encoding not currently used"
  fi

  reval-ec HandBrakeCLI --encoder x265_12bit --quality "$quality" --optimize "${opts[@]}" -i "$1" -o "$o"
  #: --optimize          Optimize MP4 files for HTTP streaming
}

function hb1080 {
  hb_opts=(--maxHeight 1080) hb265 "$@"
}
##
aliasfnq hb265sharp_strongest  fnswap printz-quoted reval re "hbjson $NIGHTDIR'/configFiles/handbrake/H265 ising (strongest).json'"
##
function hbjson {
  local preset="$1" ; shift || return 1
  local input="$1" ; shift || return 1
  local args=( "$@" )
  local ext="${hbjson_ext:-mp4}" # @opts

  local presetName="${preset:t:r}"
  local output="${input:r}_${presetName}.$ext"
  # FNSWAP: printz-quoted ; rather bad API?
  printz-quoted HandBrakeCLI --preset-import-file "$preset" --preset "$presetName" "$args[@]" -i "$input" -o "$output"
}

function ffhb {
  local preset
  preset="$(arrN $NIGHTDIR/configFiles/handbrake/*.json | fz)" || return 1
  printz-quoted hbjson "$preset" "$@"
}
##
