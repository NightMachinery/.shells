#!/usr/bin/env perl
# usage:
#  see the wrapper [agfi:cutestarsabove]
##
use strict;
use warnings;
use v5.34.0;
##
sub org_link_escape {
    my $str = shift;
    $str =~ s/\[/\\[/g;
    $str =~ s/\]/\\]/g;
    return $str;
}

sub org_title_escape {
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

        my $block_highlighted = ($block =~ s/$query/"[[highlight:,,${file_point_head}::".($start + $-[0] + 1)."][".org_title_escape($&)."]]"/erg);

        ##
        # $block =~ m/^(\*+)/;
        # my $level_stars = $1;
        # say "${level_stars} [[${file_point_head}::${first_match_start}][jump]]";
        ##
        # output all blocks as a first-level heading:
        $block_highlighted =~ s/\A(\*+)/"* |".(length $1)."|"/e ;
        # \A     Match only at beginning of string

        say $block_highlighted;
    }
}
##
my $query = $ARGV[0];
if ($query =~ m/\p{Lu}/) { # smartcase
    $query = qr/$query/;
} else {
    $query = qr/$query/i;
}

while (<stdin>) {
    chomp;
    my $file = $_ ;
    # @warn feed it absolute paths, or your links will be relative as well

    # print STDERR "FILE: $file\n";

    my $file_escaped = org_link_escape($file);

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
