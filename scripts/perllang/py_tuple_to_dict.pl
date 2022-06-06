#!/usr/bin/env perl
# Tests:
# `echo 'data, data_predict, manifolds, outliers' | py_tuple_to_dict.pl`
##
use strict;
use warnings;
use v5.34.0;
# use feature 'refaliasing';
# no warnings 'experimental::refaliasing';
##
undef $/; #: unsetting the input record separator
my @l = split(/(?:\s|,)+/, <STDIN>); #: @input stdin
my $dict_name = $ARGV[0] // "result";

for my $kw (@l) {
    say($kw . " = " . $dict_name . "['" . $kw . "']");
}
