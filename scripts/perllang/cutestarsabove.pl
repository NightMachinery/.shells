#!/usr/bin/env perl
use strict;
use warnings;
use v5.34.0;

my $file = $ARGV[0] ;

my $document = do {
    local $/ = undef;
    open my $fh, "<", $file
        or die "could not open $file: $!";
    <$fh>;
};

# say $document;

$_ = $document;

m<
    \*{2}
>x;
my $start = $-[0];
say "$start, $+[0]";

pos = $+[0];
if (m<
    \G(?:.|\n)*?
    ^\*{2}\s+
>xmg) {
my $end = $+[0];
my $length = ($end - $start);

say "$start, $end";
say (substr $document, $start, $length);
} else {
    say "no match"
}
