#!/usr/bin/perl
use strict;
use warnings;
use URI;

sub url_dir_count {
    my $url = shift;

    # Parse the URL using URI module
    my $uri = URI->new($url);

    # Get the path, remove duplicate slashes, and trim trailing slashes
    my $path = $uri->path;
    $path =~ s{//+}{/}g;   # Replace multiple slashes with a single slash
    $path =~ s{/$}{};      # Remove trailing slash

    # Split the path by slashes and count the segments
    my @segments = grep { length($_) > 0 } split('/', $path);
    return scalar(@segments);
}

sub main {
    my $url = shift or die "Usage: $0 URL\n";

    # Check if the URL is well-formed
    if ($url =~ m{^(?:[^:/?#]+://)?[^:/?#]+}) {
        # Count the directories and print
        my $count = url_dir_count($url);
        print "Directory count: $count\n";
    } else {
        warn "Error: Ill-formed URL\n";
        return 1;
    }
}

# Run the script with command line argument
main(@ARGV);
