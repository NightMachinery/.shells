##
# macOS memory reporting.
#
# `htop` on macOS cannot show the memory compressor. It allocates pages to
# Wired/Active/Inactive/Speculative/Free and folds compressed pages into its
# Cache figure, with no meter and no option to break them out (see
# htop-dev/htop#1477). On a machine under compression it therefore
# under-reports real demand by however much the compressor is holding --
# which can easily be 9GB+. These functions read `vm_stat` and `top` instead.
#
# Perl rather than awk throughout: real hashes, a sane sort, and no
# `-v`-style escape-sequence mangling of process command lines.
##
function mem-humanize() {
    perl -e '
        my $b = shift // 0;
        my @u = qw(B K M G T P);
        my $i = 0;
        while ($b >= 1024 && $i < $#u) { $b /= 1024; $i++ }
        printf $i == 0 ? "%dB\n" : "%.1f%s\n", $b, $u[$i];
    ' -- "${1:-0}"
}

function mem-stats_() {
    # Single parser for vm_stat + swap. Emits KEY=VALUE lines with the *raw*
    # vm_stat numbers: page counts stay page counts and operation counters
    # stay counters, because vm_stat mixes the two. Callers multiply page
    # counts by PAGESIZE themselves. Swap values are already in bytes.
    perl <<'PERL'
my $vm = `vm_stat`;
my ($ps) = $vm =~ /page size of (\d+)/;
print "PAGESIZE=$ps\n";

for my $line (split /\n/, $vm) {
    next unless $line =~ /^(.+?):\s+(\d+)\.?\s*$/;
    my ($key, $val) = ($1, $2);
    $key =~ s/[^A-Za-z0-9]+/_/g;
    $key =~ s/^_|_$//g;
    print uc($key), "=$val\n";
}

my %mult = (G => 1073741824, M => 1048576, K => 1024);
my $swap = `sysctl -n vm.swapusage`;
while ($swap =~ /(\w+) = ([\d.]+)([GMK])/g) {
    printf "SWAP_%s=%d\n", uc($1), $2 * $mult{$3};
}
PERL
}

function mem-snapshot() {
    # Emits three space-separated byte counts: compressor free swap_used.
    emulate -L zsh
    local -A m
    local line
    for line in ${(f)"$(mem-stats_)"} ; do
        m[${line%%=*}]=${line#*=}
    done
    print -r -- "$(( m[PAGES_OCCUPIED_BY_COMPRESSOR] * m[PAGESIZE] )) $(( m[PAGES_FREE] * m[PAGESIZE] )) ${m[SWAP_USED]}"
}

function mem-report-sys() {
    emulate -L zsh
    local -A m
    local line
    for line in ${(f)"$(mem-stats_)"} ; do
        m[${line%%=*}]=${line#*=}
    done

    local ps=${m[PAGESIZE]}
    local phys=$(sysctl -n hw.memsize)
    local wired=$(( m[PAGES_WIRED_DOWN] * ps ))
    local active=$(( m[PAGES_ACTIVE] * ps ))
    local inactive=$(( m[PAGES_INACTIVE] * ps ))
    local free=$(( (m[PAGES_FREE] + m[PAGES_SPECULATIVE]) * ps ))
    local cmp_phys=$(( m[PAGES_OCCUPIED_BY_COMPRESSOR] * ps ))
    local cmp_held=$(( m[PAGES_STORED_IN_COMPRESSOR] * ps ))
    local filebacked=$(( m[FILE_BACKED_PAGES] * ps ))

    local pct=$(( 100.0 * cmp_phys / phys ))
    local ratio=0
    (( cmp_phys > 0 )) && ratio=$(( 1.0 * cmp_held / cmp_phys ))

    local verdict
    if (( pct < 10 )) ; then
        verdict='low'
    elif (( pct < 25 )) ; then
        verdict='moderate'
    elif (( pct < 40 )) ; then
        verdict='HIGH'
    else
        verdict='SEVERE'
    fi

    printf '%-14s %s\n' 'physical' "$(mem-humanize $phys)"
    printf '%-14s %s\n' '  wired' "$(mem-humanize $wired)"
    printf '%-14s %s\n' '  active' "$(mem-humanize $active)"
    printf '%-14s %s\n' '  inactive' "$(mem-humanize $inactive)"
    printf '%-14s %s\n' '  cached files' "$(mem-humanize $filebacked)"
    printf '%-14s %s\n' '  free' "$(mem-humanize $free)"
    print
    printf '%-14s %s of RAM, holding %s of pages (%.1fx compression)\n' \
        'compressor' "$(mem-humanize $cmp_phys)" "$(mem-humanize $cmp_held)" "$ratio"
    printf '%-14s %s used of %s\n' \
        'swap' "$(mem-humanize ${m[SWAP_USED]})" "$(mem-humanize ${m[SWAP_TOTAL]})"
    printf '%-14s %s swapouts, %s decompressions (lifetime)\n' \
        '  churn' "${m[SWAPOUTS]}" "${m[DECOMPRESSIONS]}"
    print
    printf '%-14s compressor is %.0f%% of RAM -- %s\n' 'pressure' "$pct" "$verdict"
    printf '%-14s htop under-reports this machine by ~%s\n' \
        'note' "$(mem-humanize $cmp_phys)"
}
aliasfn memsys mem-report-sys

function mem-report() {
    # Per-application memory, grouped by owning .app bundle so that (e.g.)
    # Arc's 40+ helper processes collapse into a single row.
    emulate -L zsh
    local n=${1:-15}

    local -A m
    local line
    for line in ${(f)"$(mem-stats_)"} ; do
        m[${line%%=*}]=${line#*=}
    done
    local ratio=1
    (( m[PAGES_OCCUPIED_BY_COMPRESSOR] > 0 )) && \
        ratio=$(( 1.0 * m[PAGES_STORED_IN_COMPRESSOR] / m[PAGES_OCCUPIED_BY_COMPRESSOR] ))

    perl -- /dev/stdin "$n" "$ratio" <<'PERL'
my ($limit, $ratio) = @ARGV;

my %mult = (B => 1, K => 1024, M => 1048576, G => 1073741824, T => 1099511627776);

sub to_bytes {
    my ($v) = @_;
    return $1 * $mult{$2} if $v =~ /^([\d.]+)([BKMGT])$/;
    return $v + 0;
}

sub human {
    my ($b) = @_;
    my @u = qw(B K M G T);
    my $i = 0;
    while ($b >= 1024 && $i < $#u) { $b /= 1024; $i++ }
    return $i == 0 ? sprintf('%dB', $b) : sprintf('%.1f%s', $b, $u[$i]);
}

# Name the owning application for a full argv string.
sub owning_app {
    my ($args) = @_;
    # Claude Code spreads across claude.exe, bg-spare and bg-pty-host, which
    # share no .app bundle -- fold them into one row.
    return 'claude-code'
        if $args =~ m{claude\.exe|claude \s bg-|\@anthropic-ai/claude-code}x;
    # The first *.app/ component is the owning bundle, so helper processes
    # nested inside it are attributed to the parent app.
    return $1 if $args =~ m{/([^/]+)\.app/};
    my ($exe) = split ' ', $args;
    $exe =~ s{.*/}{};
    return length($exe) ? $exe : '?';
}

my %owner;
for my $line (`ps -Ao pid=,args=`) {
    next unless $line =~ /^\s*(\d+)\s+(.*\S)/;
    $owner{$1} = owning_app($2);
}

# top's MEM is phys_footprint (Activity Monitor's "Memory" column). Its CMPRS
# is the *pre-compression* size, not the RAM those pages occupy: summed across
# all processes it tracks vm_stat's "pages stored in compressor", not "pages
# occupied by compressor". Verified on this machine -- 26.0G of CMPRS against
# 29.0G stored and only 9.4G occupied. Hence the ~IN RAM column, which assumes
# a uniform compression ratio across processes and is therefore an estimate.
my (%app, $started, $total_n, $total_foot, $total_cmprs);
for my $line (`top -l 1 -o mem -n 400 -stats pid,mem,cmprs 2>/dev/null`) {
    if ($line =~ /^PID/) { $started = 1; next }
    next unless $started && $line =~ /^\s*(\d+)\s+(\S+)\s+(\S+)/;
    my $name = $owner{$1} // '?';
    $app{$name}{foot}  += to_bytes($2);
    $app{$name}{cmprs} += to_bytes($3);
    $app{$name}{count}++;
    $total_foot += to_bytes($2);
    $total_cmprs += to_bytes($3);
    $total_n++;
}

my @ranked = sort { $app{$b}{foot} <=> $app{$a}{foot} } keys %app;
my $shown = $limit < @ranked ? $limit : scalar @ranked;
my $fmt = "%-28s %5s %10s %12s %10s\n";

printf $fmt, 'APP', 'PROCS', 'FOOTPRINT', 'CMPRS RAW', '~IN RAM';
for my $name (@ranked[0 .. $shown - 1]) {
    printf $fmt, substr($name, 0, 28), 'x' . $app{$name}{count},
        human($app{$name}{foot}), human($app{$name}{cmprs}),
        human($app{$name}{cmprs} / $ratio);
}
printf $fmt, sprintf('(%d more)', @ranked - $shown), '', '', '', ''
    if @ranked > $shown;

print "\n";
printf $fmt, 'TOTAL', 'x' . $total_n, human($total_foot), human($total_cmprs),
    human($total_cmprs / $ratio);
printf "CMPRS RAW is pre-compression size; ~IN RAM divides by the live %.1fx ratio\n", $ratio;
print "footprints overlap on shared memory, so TOTAL is an upper bound\n";
PERL
}
aliasfn memtop mem-report
aliasfn ram-report mem-report
##
function claude-daemon-reap {
    # Claude Code's daemon pre-warms "bg spare" processes so new sessions start
    # instantly. Every claim spawns a replacement, but the claimed host does not
    # reliably exit when its work settles (anthropics/claude-code#43944), so
    # spares accumulate -- six of them over three days here. Restarting the
    # daemon is the only durable fix without daemon changes: it destroys all
    # spares and lets fresh ones spawn.
    #
    # The daemon idle-exits on its own once nothing holds it open, so the usual
    # answer is to close stale terminals and do nothing. This is the impatient
    # path.
    #
    # Refuses to run while interactive sessions are attached, because
    # `claude daemon stop` terminates background sessions and may take those
    # with it -- including the one you are typing in. Pass -f to override.
    emulate -L zsh
    local force=''
    [[ $1 == -f || $1 == --force ]] && force=y

    local -a interactive
    interactive=( ${(f)"$(ps -Ao pid=,tty=,args= | perl -ne '
        next unless m{claude\.exe|claude bg-};
        my ($pid, $tty) = /^\s*(\d+)\s+(\S+)/;
        print "$pid\n" if $tty ne "??";
    ')"} )
    local spares=$(pgrep -f 'claude bg-spare' | wc -l | tr -d ' ')

    # Background sessions have tty `??' and so are invisible to the count
    # above, yet `claude daemon stop' takes them down for certain: they run
    # under its pty host. Asked of the daemon itself, per config home.
    local -a background
    if (( ${+functions[h-claude-code-bg-list]} )) ; then
        background=( ${(f)"$(h-claude-code-bg-list 2>/dev/null | cut -f2)"} )
    fi

    print -r -- "bg spares:            $spares"
    print -r -- "interactive sessions: ${#interactive} ${interactive:+(${interactive[*]})}"
    print -r -- "background sessions:  ${#background} ${background:+(${background[*]})}"

    if (( ${#interactive} + ${#background} > 0 )) && [[ -z $force ]] ; then
        print -ru2 -- "Refusing: close those sessions first (the daemon will then idle-exit by itself), or pass -f."
        return 1
    fi

    local before=( ${=$(mem-snapshot)} )
    print -r -- "Stopping daemon..."
    claude daemon stop --any
    sleep 3

    local after=( ${=$(mem-snapshot)} )
    printf 'freed: %s of compressor, %s more free RAM\n' \
        "$(mem-humanize $(( before[1] - after[1] )))" \
        "$(mem-humanize $(( after[2] - before[2] )))"
    print -r -- "remaining bg spares:  $(pgrep -f 'claude bg-spare' | wc -l | tr -d ' ')"
}
##
function claude-desktop-restart() {
    # Claude Desktop boots a Linux VM (Apple Virtualization.framework) for
    # local Cowork sessions, from
    #   ~/Library/Application Support/Claude/vm_bundles/claudevm.bundle
    # The guest kernel fills its RAM with page cache and never returns it to
    # the host, so macOS compresses the lot and the cost persists long after
    # the session ends. Quitting the app is the only reliable reclaim.
    #
    # Cowork runs in the cloud by default; the local VM is opt-in. Prefer
    # cloud sessions unless a task genuinely needs local files.
    emulate -L zsh
    local vm_pat='Virtualization.VirtualMachine'
    local app_pat='Claude.app/Contents/MacOS'
    local timeout=${1:-30}

    local before=( ${=$(mem-snapshot)} )
    local vm_pid="$(pgrep -f "$vm_pat" | head -1)"

    if [[ -z "$vm_pid" ]] ; then
        print -r -- "No Cowork VM running -- nothing to reclaim."
    else
        print -r -- "Cowork VM running as pid $vm_pid."
    fi
    printf 'before: compressor %s, free %s, swap %s\n' \
        "$(mem-humanize $before[1])" "$(mem-humanize $before[2])" "$(mem-humanize $before[3])"

    print -r -- "Quitting Claude Desktop..."
    osascript -e 'quit app "Claude"' 2>/dev/null

    # Wait for both the VM *and* the app itself. Relaunching while the quit is
    # still in flight races it: the new instance comes up and the pending quit
    # takes it straight back down.
    local waited=0
    while (( waited < timeout )) && { pgrep -qf "$vm_pat" || pgrep -qf "$app_pat" } ; do
        sleep 1
        (( waited++ ))
    done

    if pgrep -qf "$vm_pat" ; then
        print -ru2 -- "VM still alive after ${timeout}s -- not relaunching. Check for an active Cowork session."
        return 1
    fi
    print -r -- "VM and app exited after ${waited}s."

    print -r -- "Relaunching Claude Desktop..."
    open -a Claude
    sleep 10

    if pgrep -qf "$app_pat" ; then
        print -r -- "Claude Desktop is back up."
    else
        print -ru2 -- "warning: Claude Desktop did not come back up; relaunch it manually."
    fi

    local after=( ${=$(mem-snapshot)} )
    print
    printf 'after:  compressor %s, free %s, swap %s\n' \
        "$(mem-humanize $after[1])" "$(mem-humanize $after[2])" "$(mem-humanize $after[3])"
    printf 'freed:  %s of compressor, %s more free RAM\n' \
        "$(mem-humanize $(( before[1] - after[1] )))" \
        "$(mem-humanize $(( after[2] - before[2] )))"
    print
    print -r -- "Swap stays allocated after a reclaim; it just stops being touched."
}
##
