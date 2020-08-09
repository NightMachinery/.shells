function table-print() {
  perl -MText::ASCIITable -e '
    $t = Text::ASCIITable->new({drawRowLine => 1});
    while (defined($c = shift @ARGV) and $c ne "--") {
      push @header, $c;
      $cols++
    }
    $t->setCols(@header);
    $rows = @ARGV / $cols;
    for ($i = 0; $i < $rows; $i++) {
      for ($j = 0; $j < $cols; $j++) {
        $cell[$i][$j] = $ARGV[$i * $cols + $j]
      }
    }
    $t->addRow(\@cell);
    print $t' -- "$@"
}
