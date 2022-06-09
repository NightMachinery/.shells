#!/usr/bin/env perl
# Tests:
# `arrnn {1..100} | grep-before-only 2 '^4$' '^9$'`
##
use strict;
use warnings;
use v5.34.0;
# use feature 'refaliasing';
# no warnings 'experimental::refaliasing';
##
my $before_count = $ARGV[0] ;
my @patterns = @ARGV[1..$#ARGV] ;
chomp(my @l=<STDIN>) ; #: @input stdin

my $i = $#l + 1; #: length
my $print_remaining = 0;
my $curr;
while ($i > 0) {
    $i -= 1;
    $curr = $l[$i];

    if ($print_remaining > 0) {
        $print_remaining -= 1;
        say $curr;
    }

    for my $pattern (@patterns) {
        if ($curr =~ m/${pattern}/) {
            $print_remaining = $before_count;
            last; #: breaks the loop
        }
    }
}
