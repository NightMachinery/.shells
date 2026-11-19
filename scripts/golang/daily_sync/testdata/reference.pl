#!/usr/bin/env perl
#: The perl conflict engine (ds-conflict-perl) that daily-sync used before the Go port,
#: kept as the reference the corpus goldens were made with. Not run in production.
#: usage: perl reference.pl show|resolve <rebase|merge> <label> <path> <name> [<hunk>=<choice>...]
use strict; use warnings;
my ($mode, $order, $label, $path, $name, @specs) = @ARGV;
open(my $fh, '<', $path) or die "cannot read $path: $!\n";
my @l = <$fh>; close $fh;
my (@out, @h);
my $i = 0;
while ($i < @l) {
    if ($l[$i] =~ /^<{7}(?: |\r?$)/) {
        my %k = (a => [], b => [], c => []); my $sec = 'a'; my $line = $i + 1; my @raw = ($l[$i]); $i++;
        while ($i < @l && $l[$i] !~ /^>{7}(?: |\r?$)/) {
            push @raw, $l[$i];
            if ($l[$i] =~ /^\|{7}(?: |\r?$)/) { $sec = 'b'; $k{based} = 1 }
            elsif ($l[$i] =~ /^={7}\r?$/) { $sec = 'c' }
            else { push @{ $k{$sec} }, $l[$i] }
            $i++;
        }
        push @raw, $l[$i] if $i < @l; $i++;
        my ($up, $loc) = $order eq 'rebase' ? ($k{a}, $k{c}) : ($k{c}, $k{a});
        push @h, { line => $line, up => $up, base => $k{b}, loc => $loc, raw => \@raw, based => $k{based} };
        push @out, \ (my $n = $#h);
    } else { push @out, $l[$i++] }
}
#: Pairs [x, y] of an LCS of $p and $q (by set membership when too big).
sub lcs_pairs {
    my ($p, $q) = @_; my ($n, $m) = (scalar @$p, scalar @$q);
    if ($n * $m > 2_000_000) { my %s = map { $_ => 1 } @$p; return map { [-1, $_] } grep { $s{ $q->[$_] } } 0 .. $m - 1 }
    my @t = map { [ (0) x ($m + 1) ] } 0 .. $n;
    for (my $x = $n - 1; $x >= 0; $x--) { for (my $y = $m - 1; $y >= 0; $y--) {
        $t[$x][$y] = $p->[$x] eq $q->[$y] ? $t[$x+1][$y+1] + 1
                   : $t[$x+1][$y] >= $t[$x][$y+1] ? $t[$x+1][$y] : $t[$x][$y+1] } }
    my @pairs; my ($x, $y) = (0, 0);
    while ($x < $n && $y < $m) {
        if ($p->[$x] eq $q->[$y]) { push @pairs, [$x, $y]; $x++; $y++ }
        elsif ($t[$x+1][$y] >= $t[$x][$y+1]) { $x++ } else { $y++ }
    }
    return @pairs;
}
#: Which lines of $q an LCS with $p matches.
sub matched {
    my ($p, $q) = @_; my @hit = (0) x @$q;
    $hit[ $_->[1] ] = 1 for lcs_pairs($p, $q);
    return @hit;
}
#: A line too common to prove anything alone: blank, short, or bare org markup.
sub weak {
    (my $s = $_[0]) =~ s/^\s+|\s+$//g;
    return length($s) < 4 || $s =~ /^#\+(begin|end)_\w*$/i || $s =~ /^(#\+\w+:?|:\w+:|\*+ ?[_-]?)$/i;
}
#: Like matched, but a run of matched lines counts only when it has a strong
#: line, so an accidental blank or #+begin_verse elsewhere matches nothing.
sub matched_strong {
    my ($p, $q) = @_; my @pairs = lcs_pairs($p, $q); my @hit = (0) x @$q; my $i = 0;
    while ($i < @pairs) {
        my $j = $i;
        $j++ while $j + 1 < @pairs && $pairs[$j][0] >= 0 && $pairs[$j+1][0] == $pairs[$j][0] + 1 && $pairs[$j+1][1] == $pairs[$j][1] + 1;
        if (grep { !weak($q->[ $pairs[$_][1] ]) } $i .. $j) { $hit[ $pairs[$_][1] ] = 1 for $i .. $j }
        $i = $j + 1;
    }
    return @hit;
}
#: Upstream's lines, minus the base lines local deleted or changed.
sub up_kept {
    my ($hk) = @_; my @inl = matched($hk->{loc}, $hk->{base}); my %gone;
    $gone{ $hk->{base}[$_] }++ for grep { !$inl[$_] } 0 .. $#{ $hk->{base} };
    return grep { !($gone{$_} && $gone{$_}--) } @{ $hk->{up} };
}
#: What local added: its lines that neither the base nor upstream already has.
sub local_added {
    my ($hk) = @_; my $loc = $hk->{loc};
    my @inb = matched_strong($hk->{base}, $loc);
    my @new = map { $loc->[$_] } grep { !$inb[$_] } 0 .. $#$loc;
    my @inu = matched_strong($hk->{up}, \@new);
    return map { $new[$_] } grep { !$inu[$_] } 0 .. $#new;
}
#: Two runs of lines, with a blank line between them when the file spaces its entries so.
sub join_blank {
    my ($p, $q) = @_;
    return (@$p, @$q) unless @$p && @$q;
    my $spaced = grep { /^\s*$/ } @$p, @$q;
    my $gap = $spaced && $p->[-1] !~ /^\s*$/ && $q->[0] !~ /^\s*$/;
    my @p = @$p; $p[-1] .= "\n" if $p[-1] !~ /\n$/;
    return (@p, ($gap ? ("\n") : ()), @$q);
}
sub norm { my $s = join '', @{ $_[0] }; $s =~ s/\s+/ /g; $s =~ s/^ | $//g; $s }
sub class {
    my ($hk) = @_;
    return 'SAME-IGNORING-WHITESPACE' if norm($hk->{up}) eq norm($hk->{loc});
    return 'BASE-NOT-SHOWN' unless $hk->{based};
    return 'UPSTREAM-ONLY-WHITESPACE' if norm($hk->{up}) eq norm($hk->{base});
    return 'LOCAL-ONLY-WHITESPACE' if norm($hk->{loc}) eq norm($hk->{base});
    my @bu = matched($hk->{up}, $hk->{base}); my @bl = matched($hk->{loc}, $hk->{base});
    my $du = grep { !$_ } @bu; my $dl = grep { !$_ } @bl;
    return 'BOTH-ONLY-ADDED' unless $du || $dl;
    return 'ONE-EDITED-OTHER-ADDED' unless $du && $dl;
    return 'BOTH-EDITED';
}
sub show {
    my ($lines, $cap) = @_;
    my @s = @$lines; my $more = @s > $cap ? @s - $cap : 0; splice(@s, $cap) if $more;
    print "  | $_" for map { (my $x = $_) =~ s/\r?\n?$/\n/; $x } @s;
    print "  ... $more more lines\n" if $more;
}
my %suggest = ('SAME-IGNORING-WHITESPACE' => 'upstream', 'BOTH-ONLY-ADDED' => 'both', 'ONE-EDITED-OTHER-ADDED' => 'both', 'BOTH-EDITED' => 'decide', 'BASE-NOT-SHOWN' => 'decide',
               'UPSTREAM-ONLY-WHITESPACE' => 'local', 'LOCAL-ONLY-WHITESPACE' => 'upstream');
if ($mode eq 'show') {
    my $n = @h;
    for my $k (0 .. $#h) {
        my $hk = $h[$k]; my $c = class($hk);
        printf "### %s, hunk %d of %d, near line %d: %s (suggested: %d=%s)\n", $name, $k + 1, $n, $hk->{line}, $c, $k + 1, $suggest{$c};
        print "--- upstream (already on the remote):\n"; show($hk->{up}, 40);
        if (@{ $hk->{base} }) { print "--- base (before either side changed it):\n"; show($hk->{base}, 25) }
        elsif ($hk->{based}) { print "--- base: nothing (both sides added text here)\n" }
        else { print "--- base: not shown\n" }
        print "--- local ($label):\n"; show($hk->{loc}, 40);
    }
    exit 0;
}
my %pick; my $all;
for my $s (@specs) {
    if ($s =~ /^(all|\d+)=(upstream|local|both|both-local-first)$/) { if ($1 eq 'all') { $all = $2 } else { $pick{$1} = $2 } }
    else { print "ERROR: bad choice '$s'; use <hunk>=upstream|local|both|both-local-first, or all=<choice>\n"; exit 0 }
}
my @res; my @done;
for my $x (@out) {
    unless (ref $x) { push @res, $x; next }
    my $k = $$x; my $hk = $h[$k]; my $c = $pick{$k + 1} // $all;
    unless (defined $c) { push @res, @{ $hk->{raw} }; next }
    my @r = $c eq 'upstream' ? @{ $hk->{up} } : $c eq 'local' ? @{ $hk->{loc} }
          : $c eq 'both' ? join_blank([up_kept($hk)], [local_added($hk)]) : join_blank([local_added($hk)], [up_kept($hk)]);
    $r[-1] .= "\n" if @r && $r[-1] !~ /\n$/;
    push @res, @r; push @done, [$k + 1, $c, \@r];
}
open(my $o, '>', $path) or die "cannot write $path: $!\n"; print $o @res; close $o;
for my $d (@done) { printf "%s hunk %d -> %s, now reads:\n", $name, $d->[0], $d->[1]; show($d->[2], 15) }
my $left = @h - @done;
print "$name: $left hunk(s) still unresolved\n" if $left;
