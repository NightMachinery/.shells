##
function perl-path-add {
  re 'reval-ec addToPATH' "$(eval-memoi brew --cellar perl)/"*/bin
}

function ansifold-path-fix {
  local ansifold
  ansifold=("$(eval-memoi brew --cellar perl)/"*/bin/ansifold(D))
  (( ${#ansifold} >= 1 )) || {
    ecerr "$0: ansifold not found"
    return 1
  }
  reval-ec lnrp "${ansifold[1]}" ~/bin/ -f
}
##
function perl-repl() {
  : "needs =cpan Data::Printer="
  : "prints the returned value, so don't use print yourself."

  rlwrap -A '' -pgreen -S"iperl> " perl -MData::Printer -wnE 'BEGIN { say "# Use `p @<arrayOrList>` or `p %<hashTable>` to print arrays/lists/hashtables; e.g.: `p %ENV`"; } say eval()//$@'
}
##
