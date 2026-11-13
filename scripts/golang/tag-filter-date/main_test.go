package main

import (
	"bytes"
	"strings"
	"testing"
	"time"
)

func TestGregorianToJalali(t *testing.T) {
	cases := []struct {
		gy, gm, gd int
		jy, jm, jd int
	}{
		{1990, 3, 20, 1368, 12, 29},
		{1990, 3, 21, 1369, 1, 1},
		{1992, 3, 20, 1370, 12, 30},
		{1996, 3, 20, 1375, 1, 1},
		{1997, 3, 20, 1375, 12, 30},
		{2000, 3, 20, 1379, 1, 1},
		{2001, 3, 20, 1379, 12, 30},
		{2020, 3, 20, 1399, 1, 1},
		{2021, 3, 20, 1399, 12, 30},
		{2022, 3, 20, 1400, 12, 29},
		{2024, 3, 20, 1403, 1, 1},
		{2025, 3, 20, 1403, 12, 30},
		{2026, 3, 20, 1404, 12, 29},
		{2026, 3, 21, 1405, 1, 1},
		{2026, 11, 13, 1405, 8, 22},
		{2028, 3, 20, 1407, 1, 1},
		{2029, 3, 20, 1408, 1, 1},
		{2030, 3, 20, 1408, 12, 30},
		{2033, 3, 20, 1412, 1, 1},
		{2034, 3, 20, 1412, 12, 30},
	}

	for _, tc := range cases {
		jy, jm, jd := gregorianToJalali(tc.gy, tc.gm, tc.gd)
		if jy != tc.jy || jm != tc.jm || jd != tc.jd {
			t.Errorf("gregorianToJalali(%04d-%02d-%02d) = %04d/%02d/%02d, want %04d/%02d/%02d",
				tc.gy, tc.gm, tc.gd, jy, jm, jd, tc.jy, tc.jm, tc.jd)
		}
	}
}

func TestCurrentJalaliDateUsesLocalDate(t *testing.T) {
	loc := time.FixedZone("TEST", 3*3600+1800)
	ts := time.Date(2026, 11, 13, 23, 30, 0, 0, loc)
	y, m, d := currentJalaliDate(ts)
	if y != 1405 || m != 8 || d != 22 {
		t.Fatalf("got %d/%d/%d, want 1405/8/22", y, m, d)
	}
}

func TestFilterTagsByDateBasicAndBoundaries(t *testing.T) {
	tags := []string{"futurecron", "tofuture", "garbageCollectMe", "GCMe", "toGarbageCollect", "toGC"}
	input := strings.Join([]string{
		"note.org:1: @futurecron/1404/12/29 past year",
		"note.org:2: @FutureCron/1405/07/30 past month",
		"note.org:3: @TOFUTURE/1405/8/21 past day",
		"note.org:4: @gcme/1405/08/22 today exact match",
		"note.org:5: @toGC/1405/08/23 future day",
		"note.org:6: @toGarbageCollect/1405/09/01 future month",
		"note.org:7: @garbageCollectMe/1406/01/01 future year",
		"note.org:8: @futurecron/999/12/30 non-shamsi 3-digit year ignored",
		"note.org:9: @futurecron/2026/01/01 gregorian year ignored",
		"note.org:10: @futurecron no date ignored",
		"note.org:11: @other/1400/01/01 wrong tag ignored",
		"note.org:12: @futurecron/1405 partial date month=0 day=0 matches",
		"note.org:13: @futurecron/1405/08 partial date day=0 matches",
		"note.org:14: @futurecron//1405///08//22 multiple slashes match",
	}, "\n") + "\n"

	var out bytes.Buffer
	if err := filterTagsByDate(strings.NewReader(input), &out, tags, 1405, 8, 22); err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	want := strings.Join([]string{
		"note.org:1: @futurecron/1404/12/29 past year",
		"note.org:2: @FutureCron/1405/07/30 past month",
		"note.org:3: @TOFUTURE/1405/8/21 past day",
		"note.org:4: @gcme/1405/08/22 today exact match",
		"note.org:12: @futurecron/1405 partial date month=0 day=0 matches",
		"note.org:13: @futurecron/1405/08 partial date day=0 matches",
		"note.org:14: @futurecron//1405///08//22 multiple slashes match",
	}, "\n") + "\n"

	if out.String() != want {
		t.Fatalf("mismatch:\ngot:\n%s\nwant:\n%s", out.String(), want)
	}
}

func TestFilterTagsSingleTagAndEmptyTagsAndMultipleMatches(t *testing.T) {
	// Single tag
	var out1 bytes.Buffer
	in1 := "@mytag/1400/1/1 yes\n@mytag/1406/1/1 no\n"
	if err := filterTagsByDate(strings.NewReader(in1), &out1, []string{"mytag"}, 1405, 8, 22); err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if out1.String() != "@mytag/1400/1/1 yes\n" {
		t.Fatalf("single tag got %q", out1.String())
	}

	// Empty tags slice (matches empty tag after @, matching CL nil tag behavior)
	var out0 bytes.Buffer
	in0 := "@/1400/1/1 yes\n@futurecron/1400/1/1 no\n"
	if err := filterTagsByDate(strings.NewReader(in0), &out0, nil, 1405, 8, 22); err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if out0.String() != "@/1400/1/1 yes\n" {
		t.Fatalf("empty tags got %q", out0.String())
	}

	// Regex metacharacters in tag name must be literal
	var outMeta bytes.Buffer
	inMeta := "@a+b.c/1400/1/1 literal\n@axbzc/1400/1/1 should not match\n"
	if err := filterTagsByDate(strings.NewReader(inMeta), &outMeta, []string{"a+b.c"}, 1405, 8, 22); err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if outMeta.String() != "@a+b.c/1400/1/1 literal\n" {
		t.Fatalf("meta tag got %q", outMeta.String())
	}

	// Multiple matches on one line (mirrors do-register-groups):
	// first match has no date (skipped), second match is past date (printed);
	// two past-date matches on one line prints twice.
	var outMulti bytes.Buffer
	inMulti := "@futurecron plain then @futurecron/1400/1/1\n@futurecron/1400/1/1 and @tofuture/1401/2/3\n"
	if err := filterTagsByDate(strings.NewReader(inMulti), &outMulti, []string{"futurecron", "tofuture"}, 1405, 8, 22); err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	wantMulti := "@futurecron plain then @futurecron/1400/1/1\n" +
		"@futurecron/1400/1/1 and @tofuture/1401/2/3\n" +
		"@futurecron/1400/1/1 and @tofuture/1401/2/3\n"
	if outMulti.String() != wantMulti {
		t.Fatalf("multi-match got:\n%s\nwant:\n%s", outMulti.String(), wantMulti)
	}
}

func TestFilterTagsJunkAndCharacterClassEdgeCases(t *testing.T) {
	// Mirrors CL:
	// - child excludes 't','n','f','r',' ','/'
	// - (scan "1\d{3}" year) is unanchored
	// - var-to-int uses default 0 on parse failure
	input := strings.Join([]string{
		"@futurecron/1405/12/30: future month stays excluded even if day=0",
		"@futurecron/1405/08/25t stops at 't' -> day=25 > 22 excluded",
		"@futurecron/1405/08/10n stops at 'n' -> day=10 <= 22 included",
		"@futurecron/1405/08/10] non-digit in day -> day=0 <= 22 included",
		"@futurecron/1405/08/22 without trailing newline",
	}, "\n")

	var out bytes.Buffer
	if err := filterTagsByDate(strings.NewReader(input), &out, []string{"futurecron"}, 1405, 8, 22); err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	want := strings.Join([]string{
		"@futurecron/1405/08/10n stops at 'n' -> day=10 <= 22 included",
		"@futurecron/1405/08/10] non-digit in day -> day=0 <= 22 included",
		"@futurecron/1405/08/22 without trailing newline",
	}, "\n") + "\n"

	if out.String() != want {
		t.Fatalf("mismatch:\ngot:\n%s\nwant:\n%s", out.String(), want)
	}
}
