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
    my @queries = @{ shift; } ;
    my $file_escaped = shift;

    my $file_point_head = "file_point:${file_escaped}";
    my $block_highlighted = $block ;

    my $echo_block = 0;
    my $match_mode = 'and';

    if ($match_mode eq 'and') {
        $echo_block = 1;
    } else {
        $echo_block = 0;
    }

    my $first_match_start = 0;
    for my $query (@queries) {
        if ($block =~ m/$query/) {
            $first_match_start = $start + $-[0] + 1;

            if ($match_mode eq 'or') {
                $echo_block = 1;
            }

            $block_highlighted =~ s/$query/"[[highlight:,,${file_point_head}::${first_match_start}][".org_title_escape($&)."]]"/eg;
            ## @todo2 how can we get the position the match would have had in the original string?
            #    - one way is to keep a datastructure to mark different regions of the highlighted string as 'original' and 'markup'
            #      + this approach might fail for overlapping matches
            #
            #    + another solution is to get the positions for all the queries, store them, then do the replacements.
            #      - we need to be smart about overlapping matches, and unify them:
            #          searching for ali, ibaba in 'alibaba'
            #            should highlight '[[highighlt:...][alibaba]]'
            #
            ##
            #             $block_highlighted =~ s/$query/"[[highlight:,,${file_point_head}::".($start + $-[0] + 1)."][".org_title_escape($&)."]]"/eg;
        } else {
            if ($match_mode eq 'and') {
                $echo_block = 0;
            }
        }
    }

    if ($echo_block) {
        ## output all blocks as a first-level heading:
        $block_highlighted =~ s/\A(\*+)/"* |".(length $1)."|"/e ;
        # \A     Match only at beginning of string

        say $block_highlighted;
    }
}
##
my @queries = map {
    my $query = $_;
    if ($query =~ m/\p{Lu}/) { # smartcase
        $query = qr/$query/;
    } else {
        # print STDERR "case-insensitive: ${query}\n";

        $query = qr/$query/i;
    }
    $query;
} @ARGV ;

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

            block_process($block, $start, \@queries, $file_escaped);
        }

        pos = $next_start;
    }

    # say "last block";
    my $block = (substr $document, $end);
    block_process($block, $end, \@queries, $file_escaped);
}
