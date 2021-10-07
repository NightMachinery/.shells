#!/usr/bin/env perl
# usage:
#  see the wrapper [agfi:cutestarsabove]
##
use strict;
use warnings;
use v5.34.0;
##
sub org_escape_brackets {
    my $str = shift;
    $str =~ s/\[/\\[/g;
    $str =~ s/\]/\\]/g;
    return $str;
}

sub org_escape_brackets_replace {
    my $str = shift;
    $str =~ s/\[/{/g;
    $str =~ s/\]/}/g;
    return $str;
}
##
sub block_process {
    my $block = shift;
    my $start = shift;
    my $query = shift;
    my $file_escaped = shift;

    if ($block =~ m/$query/) {
        my $first_match_start = $start + $-[0] + 1;
        my $file_point_head = "file_point:${file_escaped}";

        my $block_highlighted = ($block =~ s/$query/"[[highlight:,,${file_point_head}::".($start + $-[0] + 1)."][".org_escape_brackets_replace($&)."]]"/erg);

        ##
        # $block =~ m/^(\*+)/;
        # my $level_stars = $1;
        # say "${level_stars} [[${file_point_head}::${first_match_start}][jump]]";
        ##
        say $block_highlighted;
    }
}
##
my $query = qr/$ARGV[0]/;

while (<stdin>) {
    chomp;
    my $file = $_ ;
    # @warn feed it absolute paths, or your links will be relative as well

    # print STDERR "FILE: $file\n";

    my $file_escaped = org_escape_brackets($file);

    my $document = do {
        local $/ = undef;
        open my $fh, "<", $file
            or die "could not open $file: $!";
        <$fh>;
    };

    # say $document;

    $_ = $document;

    my $start = 0;
    my $end = 0;
    my $next_start = 0;

    while (m<
    \G(?:.|\n)*?
    (^\*{1,}\s+)
    >xmg) {
        $start = $end;
        $end = $-[1];
        $next_start = $+[1];

        if ($start > 0) {
            my $length = ($end - $start);

            # say "start: $start, end: $end";
            my $block = (substr $document, $start, $length);
            # say $block;

            block_process($block, $start, $query, $file_escaped);
        }

        pos = $next_start;
    }

    # say "last block";
    my $block = (substr $document, $end);
    block_process($block, $end, $query, $file_escaped);
}
