#!/usr/bin/env perl
# Tests:
# `arrnn {1..9} | tac_grouped.pl`
##
use strict;
use warnings;
use v5.34.0;
# use feature 'refaliasing';
# no warnings 'experimental::refaliasing';
##
chomp(my @l=<STDIN>); #: @input stdin
my $n = 2; #: @input number of grouped records

my $i = $#l + 1;
# print("i_start: $i\n\n");

while ($i > 0) {
    $i -= $n;
    if ($i < 0) {
        $n += $i; #: shrink n so that it equals the number of remaining records
        $i = 0;
    }

    for my $j (0 .. ($n - 1)) { #: range is inclusive
        # print("i=$i; j=$j; ");
        print("$l[$i + $j]\n");
    }
}
