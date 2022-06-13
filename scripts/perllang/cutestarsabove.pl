#!/usr/bin/env perl
# usage:
#  see the wrapper [agfi:cutestarsabove]
##
use strict;
use warnings;
use v5.34.0;
use feature 'refaliasing';
no warnings 'experimental::refaliasing';
use constant false => 0;
use constant true  => 1;

use JSON::PP qw(encode_json);
use Storable qw(dclone);
# use List::Util qw(max);
## @inputs :
my $print_parents = $ENV{"cutestarsabove_print_parents"};
my $print_children = $ENV{"cutestarsabove_print_children"};

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
my @constraints = ('and', @queries);
## @tests
# @constraints = ('and', qr/\@great/i, ['or', qr/black/i, qr/horror/i]);
# @queries = queries_from_constraints(\@constraints);
# say STDERR "queries: @queries";
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
sub say_block {
    my $block = shift;

    $block =~ s/(?:\A\R+|\R+\z)//g;
    say "${block}";
}
##
sub queries_from_constraints {
    \my @constraints = shift;
    # @assumes scalar(@constraints) >= 1;

    my @queries = ();
    foreach my $constraint (@constraints[1..$#constraints]) {
        if (ref($constraint) eq 'ARRAY') {
            push @queries, queries_from_constraints($constraint);
        } else {
            push @queries, $constraint;
        }
    }

    return @queries;
}

sub constraints_satisfied_p {
    \my @constraints = shift;

    return scalar(@constraints) == 0;
}

sub constraints_satisfy1 {
    \my @constraints = shift;
    my $satisfied = shift;

    # say STDERR "START: constraints: @constraints ; satisfied: $satisfied";

    if (not constraints_satisfied_p(\@constraints)) {
        my $constraint_type = shift @constraints;

        if ( $constraint_type eq "and" ) {
            @constraints = grep {
                if (ref($_) eq 'ARRAY') {
                    \my @sub_constraints = constraints_satisfy1($_, $satisfied);

                    not(constraints_satisfied_p(\@sub_constraints)); # prune sub_constraints that are already satisfied
                } else {
                    not ($_ eq $satisfied);
                }
            } @constraints ;
        } elsif ( $constraint_type eq "or" ) {
            my $found = 0;
            foreach my $constraint (@constraints) {
                if (ref($constraint) eq 'ARRAY') {
                    \my @sub_constraints = constraints_satisfy1($constraint, $satisfied);

                    $found = constraints_satisfied_p(\@sub_constraints);
                } else {
                    $found = ($constraint eq $satisfied);
                }

                if ($found) {
                    @constraints = ();
                    last;
                }
            }
        }

        if (not constraints_satisfied_p(\@constraints)) {
            unshift @constraints, $constraint_type;
        }
    }

    # say STDERR "END: constraints: @constraints ; satisfied: $satisfied";

    return \@constraints;
}

sub block_process {
    \my $document = shift ;
    \my $prefix = shift ;
    my $block = shift;
    my $level = shift;
    my $start = shift;
    \my @queries = shift ;
    \my @constraints = dclone shift ;
    my $file_escaped = shift;
    my $in_matching_subtree = shift;
    my $echo_block = shift;
    \my @parent_blocks = shift ;

    my $file_point_head = "file_point:${file_escaped}";
    my $block_highlighted = $block ;


    my $first_match_start = 0;
    my @matches;
    for my $query (@queries) {
        my $match_count = 0;
        while ($block =~ /$query/g) {
            $match_count += 1;

            my @match_pair = ($-[0], $+[0]);
            push @matches, \@match_pair;
        }

        if ($match_count > 0) {
            constraints_satisfy1(\@constraints, $query);
        }
    }

    my $block_satisfies = constraints_satisfied_p(\@constraints);
    $echo_block = $echo_block && ($block_satisfies || ($in_matching_subtree && $print_children));

    if ($echo_block) {
        ##
        if ($print_parents) {
            # say(">->>");
            for my $pb_ref (@parent_blocks) {
                my $pb_block = (substr $document, ${$pb_ref}{'start'}, ${$pb_ref}{'length'});
                # say("->>");

                block_process(\$document, \${$pb_ref}{'prefix'}, $pb_block, ${$pb_ref}{'level'}, ${$pb_ref}{'start'}, \@queries, [], $file_escaped, 1, 1, []);
                #: This prints the block with the matches highlighted.

                # say("<<-");
            }
            @parent_blocks = ();
        }
        # say("<<-<");
        ## DONE how can we get the position the match would have had in the original string?
        #    - one way is to keep a datastructure to mark different regions of the highlighted string as 'original' and 'markup'
        #      + this approach might fail for overlapping matches
        #
        #    + DONE another solution is to get the positions for all the queries, store them, then do the replacements.
        #      - we need to be smart about overlapping matches, and unify them:
        #          searching for ali, ibaba in 'alibaba'
        #            should highlight '[[highighlt:...][alibaba]]'
        #
        ## old way of oding things which worked for a single query (good times):
        #             $block_highlighted =~ s/$query/"[[highlight:,,${file_point_head}::".($start + $-[0] + 1)."][".org_title_escape($&)."]]"/eg;
        ##

        @matches = sort {
            my $res = $a->[0] <=> $b->[0];
            # Binary "<=>" returns -1, 0, or 1 depending on whether the left argument is numerically less than, equal to, or greater than the right argument.

            if ($res == 0) {
                $res = $a->[1] <=> $b->[1]; # @redundant
            }
            $res
        } @matches;

        my $previous_match_pair_ref = undef;
        my @matches_processed;
        foreach my $match_pair_ref (@matches) {
            if (not $previous_match_pair_ref) {
                push @matches_processed, $match_pair_ref;
            } else {
                my $match_start = ${$match_pair_ref}[0];
                my $match_end = ${$match_pair_ref}[1];

                my $previous_match_start = ${$previous_match_pair_ref}[0];
                my $previous_match_end = ${$previous_match_pair_ref}[1];

                if ($match_start < $previous_match_end) {
                    if ($match_end > $previous_match_end) {
                        ${$previous_match_pair_ref}[1] = $match_end;
                    }
                } else {
                    push @matches_processed, $match_pair_ref;
                }
            }

            $previous_match_pair_ref = $match_pair_ref;
        }

        my $offset = 0;
        foreach my $match_pair_ref (@matches_processed) {
            my $match_start = ${$match_pair_ref}[0];
            my $match_end = ${$match_pair_ref}[1];
            my $link_pre = "[[highlight:,,${file_point_head}::".($start + $match_start + 1)."][";
            substr($block_highlighted, $offset + $match_start, 0) = $link_pre;
            my $link_post = "]]";
            $offset += length $link_pre;
            substr($block_highlighted, $offset + $match_end, 0) = $link_post;
            $offset += length $link_post;
        }

        if ($level == 0) {
            say "* |0|"
        } elsif (! $print_parents) {
            if (! $in_matching_subtree) {
                #: output all blocks as a first-level heading
                $block_highlighted =~ s/\A(\*+)/"* |".(length $1)."|"/e ;
                #: \A     Match only at beginning of string
            }
        }

        print $prefix;
        say_block $block_highlighted;
    }

    return $block_satisfies;
}
##
#: top-level =my= => global var
my $document;
my $file;
my $file_escaped;

my $start ;
my $end ;
my $next_start ;

my $level_next_str ;
my $level ;
my $level_next ;
my $level_prev ; #: @notUsed
my $level_matched_root ;
my $in_matching_subtree ;
my @parent_blocks ;
my $prefix = "";

sub block_process_initial {
    $level_prev = $level;
    $level = $level_next;

    # say "level_next: $level_next ; level: $level ; level_prev: $level_prev";

    my $length = ($end - $start);

    # say "start: $start, end: $end";
    my $block = (substr $document, $start, $length);
    # say $block;

    if ($level == $level_prev + 1) { #: @tradeOff We can use =${$parent_blocks[-1]}{'level'}= instead, but then we would have to populate that stack even when =print_parents= is false.
        $prefix = "";
    } else {
        $prefix = "\n";
    }

    if ($print_parents) {
        if ($level <= $level_prev) {
            #: We are not in a child of the previous subtree.
            while ($#parent_blocks >= 0) {
                my $pb_ref = $parent_blocks[-1];
                if (${$pb_ref}{'level'} < $level) {
                    last;
                } else {
                    pop @parent_blocks;
                }
            }
        }
    }

    if ($level_matched_root >= $level) {
        #: We have exited the matched subtree.
        # say "In new tree! level=$level";

        $level_matched_root = $level;
        $in_matching_subtree = false;
    }

    my $block_satisfies = block_process(\$document, \$prefix, $block, $level, $start, \@queries, \@constraints, $file_escaped, $in_matching_subtree, 1, \@parent_blocks);

    if ($level >= 1) {
        if ($block_satisfies) {
            if (! $in_matching_subtree) {
                $level_matched_root = $level;

                # say "set level_matched_root: $level_matched_root"
            }

            $in_matching_subtree = true;
        }

        if ($print_parents && ! $in_matching_subtree) {
            my $block_info = {"level" => $level, "start" => $start, "length" => $length, "prefix" => $prefix};
            push @parent_blocks, $block_info;
        }
    }
}

while (<stdin>) {
    chomp;
    $file = $_ ;
    # @warn feed it absolute paths, or your links will be relative as well

    # print STDERR "FILE: $file\n";

    $file_escaped = org_link_escape($file);

    $document = do {
        local $/ = undef;
        open my $fh, "<", $file
            or die "could not open $file: $!";
        <$fh>;
    };

    # say $document;

    $_ = $document;

    $start = 0;
    $end = 0;
    $next_start = 0;

    $level_next_str = "";
    $level = 0;
    $level_next = 0;
    $level_prev = 0; #: @notUsed
    $level_matched_root = 0+'inf';
    $in_matching_subtree = false;

    while (m<
    \G(?:.|\n)*?
    (^\*{1,})(\s+)
    >xmg) {
        $start = $end;
        $end = $-[1]; #: just before the stars
        $next_start = $+[2]; #: after the stars and the whitespace

        block_process_initial;

        $level_next_str = $1;
        $level_next = length ${level_next_str};

        pos = $next_start;
    }

    # say "last block";
    $start = $end;
    $end = 0+'inf'; #: seems to work fine *shrugs*
    block_process_initial;
}
