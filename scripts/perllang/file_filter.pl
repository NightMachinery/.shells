#!/usr/bin/env perl
##
use strict;
use warnings;
use v5.34.0;
use Getopt::Long;

# Command-line options
my $exclude_pattern;
my $dry_run;

# Parse command-line options
GetOptions(
    "e|exclude=s" => \$exclude_pattern,
    "dry-run"     => \$dry_run
    ) or die("Error in command line arguments\n");

# Check if the exclude pattern is provided
unless ($exclude_pattern) {
    die("You must provide an exclude pattern using -e or --exclude.\n");
}

# Read file names from stdin and process each
while (my $file = <STDIN>) {
    chomp $file;

    # Check if the file exists
    unless (-e $file) {
        warn "File '$file' does not exist.\n";
        next;
    }

    # Check if the file is readable and writable
    unless (-r $file && -w $file) {
        warn "File '$file' is not readable and writable.\n";
        next;
    }

    # Temporary file to hold the filtered content
    my $temp_file = "$file.temp";

    # Open the input file
    open(my $in_fh, '<', $file) or die "Could not open file '$file': $!\n";

    # Conditionally open the output file only when not in dry-run mode
    my $out_fh;
    if (!$dry_run) {
        open($out_fh, '>', $temp_file) or die "Could not open file '$temp_file': $!\n";
    }

    # Process the file line by line
    while (my $line = <$in_fh>) {
        if ($line =~ /$exclude_pattern/) {
            if ($dry_run) {
                print "Excluding from $file:\n$line";
            }
        } else {
            print $out_fh $line if !$dry_run;
        }
    }

    # Close the file handles
    close($in_fh);
    close($out_fh) if !$dry_run;

    # Replace the original file with the filtered content
    unless ($dry_run) {
        rename($temp_file, $file) or die "Could not rename '$temp_file' to '$file': $!\n";
    }
}
