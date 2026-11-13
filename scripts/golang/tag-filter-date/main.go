package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"regexp"
	"strconv"
	"strings"
	"time"
)

var shamsiYearRe = regexp.MustCompile(`1\d{3}`)

// jalaliBreaks are the Jalali years at which the 33-year / astronomical
// leap-year rhythm shifts (Kazimierz M. Borkowski's algorithm, matching
// go-persian-calendar / jalalicli).
var jalaliBreaks = []int{
	-61, 9, 38, 199, 426, 686, 756, 818, 1111, 1181,
	1210, 1635, 2060, 2097, 2192, 2262, 2324, 2394, 2456, 3178,
}

func div(a, b int) int {
	return a / b
}

func mod(a, b int) int {
	return a % b
}

func jalaliCal(jy int) (leap, gy, march int) {
	bl := len(jalaliBreaks)
	gy = jy + 621
	leapJ := -14
	jp := jalaliBreaks[0]

	var jump int
	for i := 1; i < bl; i++ {
		jm := jalaliBreaks[i]
		jump = jm - jp
		if jy < jm {
			break
		}
		leapJ = leapJ + div(jump, 33)*8 + div(mod(jump, 33), 4)
		jp = jm
	}
	n := jy - jp

	leapJ = leapJ + div(n, 33)*8 + div(mod(n, 33)+3, 4)
	if mod(jump, 33) == 4 && jump-n == 4 {
		leapJ++
	}

	leapG := div(gy, 4) - div((div(gy, 100)+1)*3, 4) - 150
	march = 20 + leapJ - leapG

	if jump-n < 6 {
		n = n - jump + div(jump+4, 33)*33
	}
	leap = mod(mod(n+1, 33)-1, 4)
	if leap == -1 {
		leap = 4
	}
	return leap, gy, march
}

func g2d(gy, gm, gd int) int {
	d := div((gy+div(gm-8, 6)+100100)*1461, 4) +
		div(153*mod(gm+9, 12)+2, 5) +
		gd - 34840408
	d = d - div(div(gy+100100+div(gm-8, 6), 100)*3, 4) + 752
	return d
}

func d2j(jdn int) (jy, jm, jd int) {
	// Recover Gregorian year from JDN
	j := 4*jdn + 139361631
	j = j + div(div(4*jdn+183187720, 146097)*3, 4)*4 - 3908
	i := div(mod(j, 1461), 4)*5 + 308
	gm := mod(div(i, 153), 12) + 1
	gy := div(j, 1461) - 100100 + div(8-gm, 6)

	jy = gy - 621
	leap, _, march := jalaliCal(jy)
	jdn1f := g2d(gy, 3, march)
	k := jdn - jdn1f
	if k >= 0 {
		if k <= 185 {
			jm = 1 + div(k, 31)
			jd = mod(k, 31) + 1
			return jy, jm, jd
		}
		k -= 186
	} else {
		jy--
		k += 179
		if leap == 1 {
			k++
		}
	}
	jm = 7 + div(k, 30)
	jd = mod(k, 30) + 1
	return jy, jm, jd
}

// gregorianToJalali converts a Gregorian date (year, month 1..12, day 1..31)
// to its Jalali (Shamsi) year, month (1..12), and day (1..31).
func gregorianToJalali(gy, gm, gd int) (jy, jm, jd int) {
	return d2j(g2d(gy, gm, gd))
}

func currentJalaliDate(now time.Time) (int, int, int) {
	y, m, d := now.Date()
	return gregorianToJalali(y, int(m), d)
}

func buildTagFilterRegex(tags []string) (*regexp.Regexp, error) {
	var tagExpr string
	switch len(tags) {
	case 0:
		tagExpr = ""
	case 1:
		tagExpr = `(?i:` + regexp.QuoteMeta(tags[0]) + `)`
	default:
		quoted := make([]string, len(tags))
		for i, t := range tags {
			quoted[i] = regexp.QuoteMeta(t)
		}
		tagExpr = `(?i:(?:` + strings.Join(quoted, `|`) + `))`
	}

	// Matches the Common Lisp cl-ppcre parse tree in tag-filter-date.lisp:
	//   @<tag>(?:/+([^tnfr /]+))?(?:/+([^tnfr /]+))?(?:/+([^tnfr /]+))?
	// Note: in the Lisp source, (#\t #\n #\f #\r #\  #\/) are literal characters
	// 't', 'n', 'f', 'r', ' ', '/'.
	const childMaybe = `(?:/+([^tnfr /]+))?`
	pattern := `@` + tagExpr + childMaybe + childMaybe + childMaybe
	return regexp.Compile(pattern)
}

// varToInt mirrors Common Lisp (or (ignore-errors (parse-integer s)) def).
func varToInt(s string, def int) int {
	trimmed := strings.TrimSpace(s)
	if trimmed == "" {
		return def
	}
	v, err := strconv.Atoi(trimmed)
	if err != nil {
		return def
	}
	return v
}

func filterTagsByDate(r io.Reader, w io.Writer, tags []string, jYear, jMonth, jDay int) error {
	re, err := buildTagFilterRegex(tags)
	if err != nil {
		return err
	}

	br := bufio.NewReader(r)
	bw := bufio.NewWriter(w)

	for {
		line, err := br.ReadString('\n')
		if err != nil && err != io.EOF {
			return err
		}
		if len(line) > 0 {
			record := strings.TrimSuffix(line, "\n")
			matches := re.FindAllStringSubmatch(record, -1)
			for _, m := range matches {
				yearStr := m[1]
				if !shamsiYearRe.MatchString(yearStr) {
					continue
				}
				year := varToInt(yearStr, 0)
				month := varToInt(m[2], 0)
				day := varToInt(m[3], 0)

				if year < jYear ||
					(year == jYear && (month < jMonth || (month == jMonth && day <= jDay))) {
					if _, werr := fmt.Fprintf(bw, "%s\n", record); werr != nil {
						return werr
					}
				}
			}
		}
		if err == io.EOF {
			break
		}
	}

	return bw.Flush()
}

func main() {
	jYear, jMonth, jDay := currentJalaliDate(time.Now())
	if err := filterTagsByDate(os.Stdin, os.Stdout, os.Args[1:], jYear, jMonth, jDay); err != nil {
		fmt.Fprintf(os.Stderr, "tag-filter-date: %v\n", err)
		os.Exit(1)
	}
}
