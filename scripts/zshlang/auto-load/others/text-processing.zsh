### Module Text Processing
### This module specializes in functions that touch the disk.
###
clean-dups() {
    sort -u "$1" | sponge "$1"
}
pre-files() {
    doc 'stdin should be null-separated list of files that need replacement; $1 the string to replace, $2 the replacement.'
    comment '-i backs up original input files with the supplied extension (leave empty for no backup; needed for in-place replacement.)(do not put whitespace between -i and its arg.)'
    comment '-r, --no-run-if-empty
              If  the  standard input does not contain any nonblanks,
              do not run the command.  Normally, the command  is  run
              once  even  if there is no input.  This option is a GNU
              extension.'

    local cmd='s/\Q$ENV{AGR_FROM}\E/$ENV{AGR_TO}/g'
    if test -n "$agr_regex" ; then
      cmd='s/$ENV{AGR_FROM}/$ENV{AGR_TO}/g'
    fi
    AGR_FROM="$1" AGR_TO="$2" gxargs -r0 perl -pi"$pf_i" -e "$cmd"
}
@opts-setprefix pre-files agr
##
