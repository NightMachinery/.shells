#!/usr/bin/env perl
##
use strict;
use warnings;
use v5.34.0;
##
srand;

my $line;

rand($.) < 1 && ($line = $_) while <>;
#: See https://www.oreilly.com/library/view/perl-cookbook/1565922433/ch08s07.html .

print $line;
