##
# Who is connecting to us from outside, and how hard.
#
# Written after finding an unauthenticated SOCKS proxy relaying 1.27TB for
# strangers. The tell was 353 established connections to one port from twelve
# datacenter IPs -- obvious once you look, invisible until you do.
#
# nettop sees connections owned by other users only when run as root. Every
# service on this box runs as $USER, so plain invocation is normally enough;
# add sudo if you suspect otherwise.
##
function port-foreign-inbounds() {
    # Report non-private hosts with established connections to us, with
    # throughput and whois attribution.
    #
    #   port-foreign-inbounds            # every port
    #   port-foreign-inbounds 1081       # just this one
    #   port-foreign-inbounds 1081 -s 10 # longer sampling window
    emulate -L zsh
    local port='' window=3 limit=25

    while (( $# )) ; do
        case "$1" in
            -s|--seconds) window="${2:?}"; shift 2 ;;
            -n|--limit)   limit="${2:?}";  shift 2 ;;
            -h|--help)
                print -r -- "usage: ${0} [PORT] [-s SECONDS] [-n LIMIT]"
                return 0 ;;
            *) port="$1"; shift ;;
        esac
    done

    print -r -- "sampling ${window}s${port:+ on port $port}..." >&2

    # Delta mode: each row carries bytes since the previous sample, so the last
    # row for a given connection is its current rate over the window.
    #
    # The sample goes to a file rather than a pipe because the perl script
    # itself arrives on stdin via the heredoc; they cannot both have it.
    local sample
    sample="$(mktemp -t port-foreign-inbounds)"
    nettop -n -m tcp -x -d -l 2 -s "$window" >"$sample" 2>/dev/null

    # A connection is *inbound* only if our side of it is a listening port.
    # Without this the report is dominated by our own outbound traffic, which
    # looks identical in nettop apart from which end owns the well-known port.
    local listening
    listening="$(netstat -an 2>/dev/null | awk '/LISTEN/ {n=split($4,a,"."); print a[n]}' | sort -un | paste -sd, -)"

    perl -- /dev/stdin "$sample" "$port" "$window" "$limit" "$listening" <<'PERL'
my ($sample_file, $want_port, $window, $limit, $listening) = @ARGV;
$window ||= 1;
my %listening = map { $_ => 1 } grep { length } split /,/, ($listening // '');
open(my $fh, '<', $sample_file) or die "cannot read sample: $!\n";

sub is_private {
    my ($ip) = @_;
    return 1 if $ip =~ /^127\./ || $ip =~ /^10\./ || $ip =~ /^192\.168\./;
    return 1 if $ip =~ /^172\.(1[6-9]|2\d|3[01])\./;
    return 1 if $ip =~ /^169\.254\./;                      # link-local
    return 1 if $ip eq '::1' || $ip =~ /^fe80:/i || $ip =~ /^f[cd][0-9a-f]{2}:/i;
    return 1 if $ip eq '*' || $ip eq '';
    return 0;
}

sub human_rate {
    my ($b) = @_;
    my @u = ('B', 'K', 'M', 'G');
    my $i = 0;
    while ($b >= 1024 && $i < $#u) { $b /= 1024; $i++ }
    return sprintf('%.1f%s/s', $b, $u[$i]);
}

# Keep only the most recent row per connection; in delta mode that is the
# current window's transfer.
my (%conn, %port_of);
while (my $line = <$fh>) {
    next unless $line =~ /<->/;
    my @f = split ' ', $line;
    # time proto local<->remote iface state bytes_in bytes_out ...
    my ($pair, $iface, $state, $bin, $bout) = @f[2, 3, 4, 5, 6];
    next unless defined $state && $state eq 'Established';

    my ($local, $remote) = split /<->/, $pair, 2;
    # Strip the port: IPv6 uses [addr]:port or addr.port, IPv4 addr:port.
    my ($lport) = $local  =~ /[:.](\d+)$/;
    my ($rport) = $remote =~ /[:.](\d+)$/;
    (my $rip = $remote) =~ s/[:.]\d+$//;
    $rip =~ s/^\[|\]$//g;

    next unless defined $lport;
    next unless $listening{$lport};          # inbound only
    next if $want_port && $lport != $want_port;
    next if is_private($rip);

    $port_of{$lport}++;

    $conn{$pair} = { ip => $rip, lport => $lport, rport => $rport, iface => $iface,
                     bin => $bin + 0, bout => $bout + 0 };
}

if (!%conn) {
    print "No foreign inbound connections", ($want_port ? " on port $want_port" : ""), ".\n";
    exit 0;
}

my %host;
for my $c (values %conn) {
    my $h = $host{ $c->{ip} } ||= { n => 0, bin => 0, bout => 0, iface => $c->{iface}, ports => {} };
    $h->{n}++;
    $h->{bin}  += $c->{bin};
    $h->{bout} += $c->{bout};
    $h->{ports}{ $c->{lport} }++;   # which of our ports they are hitting
}

# Busiest first, by connection count then bytes.
my @ranked = sort {
    $host{$b}{n} <=> $host{$a}{n}
      || ($host{$b}{bin} + $host{$b}{bout}) <=> ($host{$a}{bin} + $host{$a}{bout})
} keys %host;

# Team Cymru returns ASN, country and org in one line -- far easier to parse
# than the per-registry whois formats.
sub attribute {
    my ($ip) = @_;
    my $out = `whois -h whois.cymru.com " -v $ip" 2>/dev/null`;
    my @lines = grep { /\|/ && !/^AS\s*\|/ } split /\n/, ($out // '');
    return ('?', '?', 'lookup failed') unless @lines;
    my @f = map { s/^\s+|\s+$//gr } split /\|/, $lines[-1];
    return ($f[3] // '?', $f[0] // '?', $f[6] // '?');
}

my $fmt = "%-32s %-11s %5s %10s %10s  %-3s %-7s %s\n";
printf $fmt, "REMOTE", "OUR PORT", "CONNS", "IN", "OUT", "CC", "ASN", "ORG";

my ($tn, $tin, $tout) = (0, 0, 0);
my $shown = 0;
for my $ip (@ranked) {
    my $h = $host{$ip};
    $tn += $h->{n}; $tin += $h->{bin}; $tout += $h->{bout};
    next if $shown++ >= $limit;
    my ($cc, $asn, $org) = attribute($ip);
    $org = substr($org, 0, 44);
    my $plist = join(",", sort { $a <=> $b } keys %{$h->{ports}});
    printf $fmt, substr($ip, 0, 32), substr($plist, 0, 11), $h->{n},
        human_rate($h->{bin} / $window), human_rate($h->{bout} / $window),
        $cc, $asn, $org;
}
printf "%-40s %6s %11s %11s\n", "(" . (@ranked - $limit) . " more)", '', '', ''
    if @ranked > $limit;

print "\n";
printf $fmt, "TOTAL", "", $tn, human_rate($tin / $window), human_rate($tout / $window), "", "", "";
printf "%d foreign host(s) across %d connection(s)\n", scalar(@ranked), $tn;
print "Rates are over the sample window, so an idle-but-open connection reads 0.\n";
PERL

    command rm -f "$sample"
}
aliasfn pfi port-foreign-inbounds

function port-foreign-inbounds-watch() {
    # Same, on a loop. Handy while tightening a firewall rule and watching
    # whether anything is still getting through.
    emulate -L zsh
    while true ; do
        clear
        print -r -- "$(date '+%H:%M:%S')  foreign inbound connections"
        port-foreign-inbounds "$@"
        sleep 5
    done
}
##
