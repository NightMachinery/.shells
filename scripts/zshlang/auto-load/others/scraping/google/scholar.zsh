###
# Helper function for extracting metrics with xmllint
function h-scholar-metric-xmllint {
    local html="${1}"
    local metric_name="${2}"

    # Create a temporary file to store the HTML
    local tmp_file
    tmp_file="$(mktemp)" @TRET
    assert ec "${html}" > "${tmp_file}" @RET

    # Use xmllint to extract the value
    local value
    value="$(xmllint --html --xpath "//td[@class='gsc_rsb_sc1']/a[contains(text(),'${metric_name}')]/../../td[@class='gsc_rsb_std'][1]/text()" "${tmp_file}" 2>/dev/null)" @TRET

    # Clean up - ignore errors silently since it's just a temp file
    rm -f "${tmp_file}" || true

    if [[ -z "${value}" ]]; then
        ecerr "Failed to extract ${metric_name}"
        return 1
    fi

    # Remove any commas and whitespace from the number
    value="${value//,/}"
    value="${value//[[:space:]]/}"
    ec "${value}"
}

# Helper function for extracting metrics with html-xml-utils
function h-scholar-metric-hxutils {
    local html="${1}"
    local metric_name="${2}"

    local tmp_file
    tmp_file="$(mktemp)" @TRET
    assert ec "${html}" > "${tmp_file}" @RET

    # First normalize the HTML, then select the appropriate cell
    local value
    value="$(hxnormalize -x "${tmp_file}" 2>/dev/null |
            hxselect -c "tr:contains('${metric_name}') td.gsc_rsb_std:first-child" |
            tr -d '[:space:],',)" @TRET

    # Clean up - ignore errors silently
    rm -f "${tmp_file}" || true

    if [[ -z "${value}" ]]; then
        ecerr "Failed to extract ${metric_name}"
        return 1
    fi

    ec "${value}"
}

# Helper function for extracting metrics with Perl and HTML::TreeBuilder
function h-scholar-metric-perl {
    local html="${1}"
    local metric_name="${2}"

    local tmp_file
    tmp_file="$(mktemp)" @TRET
    assert ec "${html}" > "${tmp_file}" @RET

    local value
    value="$(perl - "${tmp_file}" "${metric_name}" <<'EOF'
use strict;
use warnings;
use HTML::TreeBuilder;

my $file_path = shift;
my $metric_name = shift;

my $tree = HTML::TreeBuilder->new;
$tree->parse_file($file_path);

my $result = '';

foreach my $row ($tree->look_down("_tag", "tr")) {
    my @cells = $row->look_down("_tag", "td");
    next unless scalar @cells >= 2;

    my $first_cell = $cells[0];
    my $a_tag = $first_cell->look_down("_tag", "a");
    next unless $a_tag;

    if ($a_tag->as_text =~ /$metric_name/) {
        # Get the value from the second cell (All column)
        $result = $cells[1]->as_text;
        last;
    }
}

$tree->delete;
print $result;
EOF
)" @TRET

    # Clean up - ignore errors silently
    rm -f "${tmp_file}" || true

    if [[ -z "${value}" ]]; then
        ecerr "Failed to extract ${metric_name}"
        return 1
    fi

    # Remove any commas and whitespace from the number
    value="${value//,/}"
    value="${value//[[:space:]]/}"
    ec "${value}"
}

function scholar-citation-count-perl {
    local html="${1:-$(cat)}"
    h-scholar-metric-perl "${html}" "(Citations|Zitate|引用)" @RET
}

function scholar-h-index-perl {
    local html="${1:-$(cat)}"
    h-scholar-metric-perl "${html}" "(h-index|indice h|h 指標|h 指数)" @RET
}

function scholar-i10-index-perl {
    local html="${1:-$(cat)}"
    h-scholar-metric-perl "${html}" "(i10-index|indice i10|i10 指標|i10 指数)" @RET
}

# Gateway functions for easy implementation swapping
aliasfn scholar-citation-count scholar-citation-count-perl
aliasfn scholar-h-index scholar-h-index-perl
aliasfn scholar-i10-index scholar-i10-index-perl
##
function scholar-title-get {
    local html="${1:-$(cat)}"

    local tmp_file
    tmp_file="$(mktemp)" @TRET
    assert ec "${html}" > "${tmp_file}" @RET

    local title
    title="$(perl - "${tmp_file}" <<'EOF'
use strict;
use warnings;
use HTML::TreeBuilder;

my $file_path = shift;

my $tree = HTML::TreeBuilder->new;
$tree->parse_file($file_path);

my $result = '';

# Look for the title element
my $title_div = $tree->look_down("id", "gsc_oci_title");
if ($title_div) {
    my $link = $title_div->look_down("_tag", "a", "class", "gsc_oci_title_link");
    if ($link) {
        $result = $link->as_text;
    }
}

$tree->delete;
print $result;
EOF
)" @TRET

    # Clean up - ignore errors silently
    rm -f "${tmp_file}" || true

    if [[ -z "${title}" ]]; then
        ecerr "Failed to extract title"
        return 1
    fi

    ec "${title}"
}

function scholar-title-current {
    browser-current-html | scholar-title-get
}

function scholar-paper-citations-get {
    local html="${1:-$(cat)}"

    local tmp_file
    tmp_file="$(mktemp)" @TRET
    assert ec "${html}" > "${tmp_file}" @RET

    local citations
    citations="$(perl - "${tmp_file}" <<'EOF'
use strict;
use warnings;
use HTML::TreeBuilder;

my $file_path = shift;

# Check if the file exists and is readable
unless (-e $file_path && -r $file_path) {
    print STDERR "Error: File '$file_path' does not exist or is not readable\n";
    exit 1;
}

my $tree = HTML::TreeBuilder->new;
eval {
    $tree->parse_file($file_path);
};
if ($@) {
    print STDERR "Error parsing HTML: $@\n";
    exit 1;
}

my $result = '';
my $found_citations = 0;

# Look for all elements with class gsc_oci_field
my @field_divs = $tree->look_down("class", "gsc_oci_field");
if (!@field_divs) {
    print STDERR "Error: Could not find any elements with class 'gsc_oci_field'\n";
    $tree->delete;
    exit 1;
}

# Check each field div for the text "Total citations" (case insensitive)
foreach my $field_div (@field_divs) {
    if ($field_div->as_text =~ /total citations/i) {
        my $parent = $field_div->parent;
        if (!$parent) {
            print STDERR "Error: Could not find parent element for citations field\n";
            next;
        }

        my $value_div = $parent->look_down("class", "gsc_oci_value");
        if (!$value_div) {
            print STDERR "Error: Could not find element with class 'gsc_oci_value'\n";
            next;
        }

        my $link = $value_div->look_down("_tag", "a");
        if (!$link) {
            print STDERR "Error: Could not find link element in citation value\n";
            next;
        }

        if ($link->as_text =~ /Cited by (\d+)/i) {
            $result = $1;
            $found_citations = 1;
            last;
        } else {
            print STDERR "Error: Citation text not in expected format\n";
            next;
        }
    }
}

if (!$found_citations) {
    print STDERR "Error: Could not find 'Total citations' field or extract citation count\n";
    $tree->delete;
    exit 1;
}

$tree->delete;
print $result;
EOF
)" @RET

    # Clean up - ignore errors silently
    rm -f "${tmp_file}" || true

    if [[ -z "${citations}" ]]; then
        ecerr "Failed to extract citation count"
        return 1
    fi

    ec "${citations}"
}

function scholar-paper-citations-current {
    browser-current-html | scholar-paper-citations-get
}
##
function scholar-org-link-current {
    local html
    html="$(browser-current-html)" @TRET
    citations="$(scholar-citation-count <<<"${html}")" || citations=""
    h_index="$(scholar-h-index <<<"${html}")" || h_index=""
    i10_index="$(scholar-i10-index <<<"${html}")" || i10_index=""

    org_link="$(org_link_browser_raw_p=y org-link-browser-current)" @TRET

    res=""
    if test -n "${citations}"; then
        if test -n "${res}" ; then
            res+=" "
        fi

        res+="@citations/${citations}"
    fi
    if test -n "${h_index}"; then
        if test -n "${res}" ; then
            res+=" "
        fi
        res+="@hIndex/${h_index}"
    fi
    if test -n "${i10_index}"; then
        if test -n "${res}" ; then
            res+=" "
        fi
        res+="@i10/${i10_index}"
    fi

    if test -n "${res}" ; then
        res+=" "
    fi
    res+="${org_link}"

    ec "${res}"
}
###
