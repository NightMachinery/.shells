##
function tail-pop {
    local block_size="${tail_pop_block_size:-1048576}"
    local line_count="${tail_pop_line_count:-1}"
    local input

    while (( $# > 0 )) ; do
        case "${1}" in
            -n|--lines)
                shift
                if (( $# == 0 )) ; then
                    ecerr "${0}: missing value for -n"
                    return 1
                fi
                line_count="${1}"
                ;;
            --)
                shift
                break
                ;;
            -*)
                ecerr "${0}: unknown option: ${1}"
                return 1
                ;;
            *)
                break
                ;;
        esac

        shift
    done

    input="${1}"

    if [[ -z "${input}" ]] ; then
        ecerr "${0}: input not supplied"
        return 1
    fi

    if (( $# > 1 )) ; then
        ecerr "${0}: too many arguments"
        return 1
    fi

    if [[ "${line_count}" != <-> ]] ; then
        # <-> is a Zsh numeric glob pattern. It matches an unsigned decimal integer
        ##
        ecerr "${0}: line count must be a non-negative integer"
        return 1
    fi

    if [[ "${block_size}" != <-> ]] || (( block_size <= 0 )) ; then
        ecerr "${0}: tail_pop_block_size must be a positive integer"
        return 1
    fi

    if [[ ! -e "${input}" ]] ; then
        ecerr "${0}: no such file: ${input}"
        return 1
    fi

    if [[ ! -f "${input}" ]] ; then
        ecerr "${0}: not a regular file: ${input}"
        return 1
    fi

    command perl -- - "${input}" "${block_size}" "${line_count}" <<'EOF' @RET
use strict;
use warnings;
use Fcntl qw(:flock SEEK_SET SEEK_END);

my ($path, $block_size_arg, $line_count_arg) = @ARGV;
my $program = 'tail-pop';

sub fail {
    my ($message) = @_;
    print STDERR "${program}: ${message}\n";
    exit 1;
}

sub read_at {
    my ($fh, $offset, $length) = @_;

    defined(sysseek($fh, $offset, SEEK_SET))
        or fail("${path}: seek failed: ${!}");

    my $buf = '';

    while (length($buf) < $length) {
        my $part = '';
        my $wanted = $length - length($buf);
        my $n = sysread($fh, $part, $wanted);

        defined($n)
            or fail("${path}: read failed: ${!}");

        last if $n == 0;

        $buf .= $part;
    }

    length($buf) == $length
        or fail("${path}: short read");

    return $buf;
}

sub write_all {
    my ($buf) = @_;
    my $offset = 0;

    while ($offset < length($buf)) {
        my $n = syswrite(STDOUT, substr($buf, $offset));

        defined($n)
            or fail("write failed: ${!}");

        $n > 0
            or fail("write failed: wrote zero bytes");

        $offset += $n;
    }
}

defined($path) && length($path)
    or fail('input not supplied');

defined($block_size_arg) && $block_size_arg =~ /\A[1-9][0-9]*\z/
    or fail('block size must be a positive integer');

defined($line_count_arg) && $line_count_arg =~ /\A[0-9]+\z/
    or fail('line count must be a non-negative integer');

my $block_size = int($block_size_arg);
my $line_count = int($line_count_arg);

open my $fh, '+<:raw', $path
    or fail("${path}: open failed: ${!}");

flock($fh, LOCK_EX)
    or fail("${path}: lock failed: ${!}");

my $size = sysseek($fh, 0, SEEK_END);

defined($size)
    or fail("${path}: seek failed: ${!}");

exit 0 if $size == 0 || $line_count == 0;

my $last_byte = read_at($fh, $size - 1, 1);
my $pos = $last_byte eq "\n" ? $size - 1 : $size;
my $lines_seen = 0;
my $cut_pos = 0;

while ($pos > 0) {
    my $read_len = $pos < $block_size ? $pos : $block_size;
    $pos -= $read_len;

    my $buf = read_at($fh, $pos, $read_len);
    my $idx = length($buf);

    while ($idx > 0) {
        $idx = rindex($buf, "\n", $idx - 1);
        last if $idx < 0;

        $lines_seen++;

        if ($lines_seen >= $line_count) {
            $cut_pos = $pos + $idx + 1;
            last;
        }
    }

    last if $lines_seen >= $line_count;
}

defined(sysseek($fh, $cut_pos, SEEK_SET))
    or fail("${path}: seek failed: ${!}");

while (1) {
    my $buf = '';
    my $n = sysread($fh, $buf, $block_size);

    defined($n)
        or fail("${path}: read failed: ${!}");

    last if $n == 0;

    write_all($buf);
}

truncate($fh, $cut_pos)
    or fail("${path}: truncate failed: ${!}");

close($fh)
    or fail("${path}: close failed: ${!}");
EOF
}
##
