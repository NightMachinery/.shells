#!/usr/bin/env -S perl -CS

use strict;
use utf8;
use warnings;
use v5.34.0;
use charnames ':full';
use Getopt::Long;
use Encode;

# Conversion map for Arabic numerals to English
my %arabic_to_english = (
    '۰' => '0',
    '۱' => '1',
    '۲' => '2',
    '۳' => '3',
    '۴' => '4',
    '۵' => '5',
    '۶' => '6',
    '۷' => '7',
    '۸' => '8',
    '۹' => '9'
    );

# Command line options
my $dry_run = 0;
GetOptions("dry-run" => \$dry_run);

while (<STDIN>) {
    chomp;  # remove trailing newline
    my $file = $_;
    my $new_file_name = $file;

    # Convert Arabic numerals to English
    $new_file_name =~ s/([۰۱۲۳۴۵۶۷۸۹])/exists $arabic_to_english{$1} ? $arabic_to_english{$1} : do {
        warn "Unrecognized character: $1, Code: " . ord($1) . ", Unicode name: " . charnames::viacode(ord($1)) . "\n";
        $1
    }/ge;

    if ($new_file_name =~ /جلسه (\d+)/) {
        my $lecture_number = $1;
        $new_file_name = sprintf("L%02d. %s", $lecture_number, $new_file_name);

        # Remove duplicate Lxx. prefixes
        $new_file_name =~ s/(L\d+\. ){2,}/$1/g;

    }

    if ($file eq $new_file_name) {
        # No renaming needed, names are identical
        next;
    }

    if ($dry_run) {
        # Just print the renaming operation without executing it
        say "$file\n->\t$new_file_name'";
    } else {
        # Check if the file with the new name already exists
        if (-e $new_file_name) {
            print "File '$new_file_name' already exists. Skipping.\n";
        } else {
            ##
            # my @cmd = ('gmv', '-iv', $file, $new_file_name);
            ##
            # my $encoded_file = encode('UTF-8', $file);
            # my $encoded_new_file_name = encode('UTF-8', $new_file_name);
            # my @cmd = ('gmv', '-iv', $encoded_file, $encoded_new_file_name);

            # system @cmd;
            ##
            rename($file, $new_file_name) or warn "Failed to rename '$file' to '$new_file_name': $!\n";
        }
    }
}
