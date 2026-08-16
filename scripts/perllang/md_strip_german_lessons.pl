#!/usr/bin/perl
#: Strips every "Learning German ^_^" section from a markdown document,
#: including the mid-document ones that LLM conversation exports carry after
#: each assistant message. Reads STDIN, writes STDOUT.
#: Driven by [agfi:md-strip-german-lessons]; contrast
#: [agfi:md-strip-german-teachings], which only drops the trailing section.
#:
#: $md_german_lesson_re overrides the heading pattern.
use strict;
use warnings;
use utf8;
use open qw/:std :utf8/;

#: The heading is a strict output contract, but we tolerate drift in heading
#: level and in surrounding horizontal whitespace.
#: `||` rather than `//` so an empty override falls back to the default.
my $H = $ENV{md_german_lesson_re}
    || '#{1,6}[ \t]+Learning German \^_\^[ \t]*';

my $HEADING = qr/^$H$/;
my $HR      = qr/^ {0,3}(?:-{3,}|\*{3,}|_{3,})[ \t]*$/;
my $DETAILS = qr/^[ \t]*<details\b/i;
my $BLANK   = qr/^[ \t]*$/;
my $FENCE   = qr/^ {0,3}(`{3,}|~{3,})/;
#: Speaker headings delimit a section regardless of their level: ChatGPT
#: exports use a level-1 German heading whose own subheadings are `##`/`###`,
#: so the level test below never fires there.
my $SPEAKER = qr{^\#{1,6}[ \t]+ (?:\[\d+\][ \t]+)?
                 (?:You|ChatGPT|Claude|Assistant|User|System|Human)\b}xi;

my @lines = <STDIN>;
my @out;
my $i          = 0;
my $fence_char = '';
my $ate_eof    = 0;

while ($i < @lines) {
    my $line = $lines[$i];

    if (my ($fence) = $line =~ $FENCE) {
        my $c = substr($fence, 0, 1);
        if (!$fence_char)        { $fence_char = $c }
        elsif ($c eq $fence_char) { $fence_char = '' }

        push @out, $line;
        $i++;
        next;
    }

    if (!$fence_char && $line =~ $HEADING) {
        my ($hashes) = $line =~ /^(\#{1,6})/;
        my $level = defined $hashes ? length($hashes) : 6;

        #: Find where the section ends. Its own fence state is separate from
        #: the outer one, since the whole span is about to be dropped.
        my $j     = $i + 1;
        my $inner = '';
        my $term  = 'eof';
        while ($j < @lines) {
            my $l = $lines[$j];

            if (my ($fence) = $l =~ $FENCE) {
                my $c = substr($fence, 0, 1);
                if (!$inner)        { $inner = $c }
                elsif ($c eq $inner) { $inner = '' }

                $j++;
                next;
            }

            if (!$inner) {
                if ($l =~ $HR)      { $term = 'hr';      last }
                #: The "Sources" footer belongs to the preceding message even
                #: though the exporter emits it after the lesson. Keep it.
                if ($l =~ $DETAILS) { $term = 'details'; last }
                if ($l =~ $SPEAKER) { $term = 'heading'; last }

                if (my ($h) = $l =~ /^(\#{1,6})[ \t]/) {
                    if (length($h) <= $level) { $term = 'heading'; last }
                }
            }

            $j++;
        }

        #: Every lesson sits between two rules, so exactly one of them goes
        #: with it. Take the trailing rule when there is one; otherwise take
        #: the preceding rule, which reattaches a Sources footer to its
        #: message. A heading terminator means the surrounding rules are
        #: already correct, so neither is touched.
        if ($term eq 'details' || $term eq 'eof') {
            pop @out while @out && $out[-1] =~ $BLANK;
            pop @out if @out && $out[-1] =~ $HR;
        }

        $i = $term eq 'hr' ? $j + 1 : $j;

        #: Leave exactly one blank line at the seam.
        pop @out while @out && $out[-1] =~ $BLANK;
        if (@out) {
            $out[-1] .= "\n" unless $out[-1] =~ /\n\z/;
            push @out, "\n";
        }
        $i++ while $i < @lines && $lines[$i] =~ $BLANK;

        $ate_eof = $i >= @lines;

        next;
    }

    push @out, $line;
    $i++;
}

#: The seam blank above is trailing whitespace when the last section ran to
#: the end of the document.
pop @out while $ate_eof && @out && $out[-1] =~ $BLANK;

print @out;
