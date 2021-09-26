##
function perl-repl() {
  : "needs =cpan Data::Printer="
  : "prints the returned value, so don't use print yourself."

  rlwrap -A '' -pgreen -S"iperl> " perl -MData::Printer -wnE 'BEGIN { say "# Use `p @<arrayOrList>` or `p %<hashTable>` to print arrays/lists/hashtables; e.g.: `p %ENV`"; } say eval()//$@'
}
##
