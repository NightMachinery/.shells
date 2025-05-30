### Module Text Processing
### This module specializes in functions that touch the disk.
###
function pre-files {
  doc 'stdin should be null-separated list of files that need replacement; $1 the string to replace, $2 the replacement.'

  local from="$1" to="$2"

  local cmd
  if bool "${agr_regex}" ; then
    if true ; then
      cmd="s/${from}/${to}/g"
    else
      # the quoting rules here are weird, and seem to quote AGR_TO as a literal string.
      cmd='s/$ENV{AGR_FROM}/$ENV{AGR_TO}/g'
    fi
  else
    cmd='s/\Q$ENV{AGR_FROM}\E/$ENV{AGR_TO}/g'
  fi

  AGR_FROM="$from" AGR_TO="$to" gxargs -r0 perl -pi"$pf_i" -e "$cmd" @RET
  # -i:
  # backs up original input files with the supplied extension (leave empty for no backup; needed for in-place replacement.)(do not put whitespace between -i and its arg.)'
  #
  # -r, --no-run-if-empty:
  # If  the  standard input does not contain any nonblanks, do not run the command.  Normally, the command  is  run once  even  if there is no input.  This option is a GNU extension.'
}
@opts-setprefix pre-files agr
##
function f-text-split {
  : "splits a text file into parts having the specified size"

  local i="$1" size="$2" del_p="${f_split_del_p}"
  assert-args i size @RET
  assert test -e "$i" @RET

  if [[ "$size" =~ '^\d+$' ]] ; then
  size+='k' #: use kilobytes by default
  fi

  local o="${3}"
  local opts=()
  local ext="${i:e}"

  if true ; then
  o="${o:-${i}.part}"
  else
    o="${o:-${i:r}_part}"

    if test -n "$ext" ; then
    opts+=( --additional-suffix=".${ext}")
    fi
  fi

  reval-ec gsplit "$opts[@]" -C "$size" --numeric-suffixes "$i" "$o" @RET

  ec-sep-h
  ecgray "Split Files:"
  ll "$o"*(.DN) #: show the split files
  ec-sep-h

  if bool "${del_p}" ; then
    if git-p "$i" ; then
      reval-ecgray git-rm "$i" || true
    fi

    trs "$i"
  fi
}
@opts-setprefix f-text-split f_split
##
