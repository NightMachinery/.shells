#!/usr/bin/env perl
#: * @deps
#: ** =cpanm Mojo::DOM=
##
use strict;
use warnings;
use v5.34.0;
use experimental qw(vlb);

use Mojo::DOM;
##
my $input = do { local $/; <STDIN> };
# say $input;

# my $dom = Mojo::DOM->new($input);
my $dom = Mojo::DOM->new->xml(0)->parse($input); #: force HTML semantics

$dom->descendant_nodes->grep(sub { $_->type eq 'text' })
    ->each(sub{
        my $parent = $_->parent;
        my $class = $parent->{class};
        if ($class && $class =~ m/\b(?:todo|done|at_tag)\b/) {
            return
        }
        $_->replace(s/(*plb:^|\s)(\@(?:\w|\/|\d|[][(),.;'\''])+)(*pla:\s|$)/<span class="todo at_tag">$1<\/span>/gr)
           });
#: Basically $_ contains a Mojo::DOM object that stringifies to the text of the element, so s/// operates on that stringification and, naturally, /r causes it to return a string instead of changing the object in-place.

say $dom;
