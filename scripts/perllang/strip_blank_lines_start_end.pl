#!/usr/bin/perl
use strict;
use warnings;

my $leading_blanks = 1;
my @buffer = ();

while (<STDIN>) {
    if (/^\s*$/) {
        # Buffer blank lines unless in leading section
        push @buffer, $_ unless $leading_blanks;
    } else {
        # Print buffered blank lines and current line
        print @buffer, $_;
        # Clear buffer and set leading_blanks to false
        @buffer = ();
        $leading_blanks = 0;
    }
}
# No trailing blank lines will be printed due to buffer being cleared
