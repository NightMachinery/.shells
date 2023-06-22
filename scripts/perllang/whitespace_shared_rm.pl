#!/usr/bin/perl
##
use strict;
use warnings;

#: Read entire input
my @lines = <STDIN>;

#: Convert leading tabs to 4 spaces
foreach (@lines) {
    s/^\t+/ ' ' x (4 * length($&)) /ge;
    #: $&: the matched string
}

#: Determine the smallest shared indentation
my $min_indent;
for (@lines) {
    next unless m/^( *)\S/;
    #: Skip empty lines
    #: Note that this assumes there won't be leading whitespace of a different kind (i.e., lines start either with tabs or spaces and after that there is \S)

    my $indent = length($1);
    $min_indent = $indent if not defined $min_indent or $indent < $min_indent;
}

#: Remove minimum indentation from each line
s/^( {$min_indent})// for @lines;

#: Print out the lines
print @lines;
