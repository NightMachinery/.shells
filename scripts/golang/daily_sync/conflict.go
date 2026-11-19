package main

// The conflict engine: `daily_sync conflict show|resolve', which the zsh
// prelude's ds-conflict-print and ds-conflict-resolve run on every machine.
// The local machine builds this program and ships it to the others
// (h-daily-sync-engine), so every machine runs the same version.
//
// It reads a file with zdiff3 (or diff3, or plain) conflict markers, names
// the two sides `upstream' (already on the remote) and `local' (this
// machine's), classifies each hunk, and either lists the hunks with a
// suggestion or rewrites the file with the picks it is given.
//
// It is a byte-for-byte port of the perl engine it replaced (kept as
// testdata/reference.pl), so it follows perl's byte semantics: `\s' is
// ASCII whitespace (space, \t, \n, \v, \f, \r), lengths are in bytes, and a
// line is everything up to and including its "\n". testdata/corpus holds the
// perl engine's outputs, which corpus_test.go checks this one against.

import (
	"errors"
	"fmt"
	"io"
	"os"
	"strconv"
	"strings"
	"syscall"
)

const conflictUsage = `usage: daily_sync conflict show <rebase|merge> <label> <path> <name>
       daily_sync conflict resolve <rebase|merge> <label> <path> <name> [<hunk>=<choice>...]
  <choice>: upstream, local, both (upstream's first) or both-local-first; <hunk>: a number or all.
`

type hunk struct {
	line   int      // 1-based line of the opening marker
	up     []string // the side already on the remote
	base   []string
	loc    []string // this machine's side
	raw    []string // the hunk as it stands, markers included
	based  bool     // a base marker was seen
	index  int      // 0-based, in file order
	result []string // set once resolved
	choice string
}

// part is a run of plain lines or one hunk of the file.
type part struct {
	line string
	h    *hunk // nil for a plain line
}

func runConflict(args []string, stdout, stderr io.Writer) int {
	if len(args) < 1 || (args[0] != "show" && args[0] != "resolve") {
		fmt.Fprint(stderr, conflictUsage)
		return 64
	}
	// Missing arguments read as empty, as they did for the perl engine.
	for len(args) < 5 {
		args = append(args, "")
	}
	mode, order, label, path, name, specs := args[0], args[1], args[2], args[3], args[4], args[5:]

	data, err := readLines(path)
	if err != nil {
		fmt.Fprintf(stderr, "cannot read %s: %s\n", path, strerror(err))
		return exitFor(err)
	}
	parts, hunks := parseConflicts(data, order == "rebase")
	if mode == "show" {
		showHunks(stdout, hunks, label, name)
		return 0
	}

	pick := map[string]string{}
	all := ""
	for _, s := range specs {
		k, c, ok := parseSpec(s)
		if !ok {
			fmt.Fprintf(stdout, "ERROR: bad choice '%s'; use <hunk>=upstream|local|both|both-local-first, or all=<choice>\n", s)
			return 0
		}
		if k == "all" {
			all = c
		} else {
			pick[k] = c
		}
	}
	var res []string
	var done []*hunk
	for _, p := range parts {
		if p.h == nil {
			res = append(res, p.line)
			continue
		}
		h := p.h
		c, ok := pick[strconv.Itoa(h.index+1)]
		if !ok {
			c = all
		}
		if c == "" {
			res = append(res, h.raw...)
			continue
		}
		var r []string
		switch c {
		case "upstream":
			r = clone(h.up)
		case "local":
			r = clone(h.loc)
		case "both":
			r = joinBlank(upKept(h), localAdded(h))
		default:
			r = joinBlank(localAdded(h), upKept(h))
		}
		if n := len(r); n > 0 && !strings.HasSuffix(r[n-1], "\n") {
			r[n-1] += "\n"
		}
		res = append(res, r...)
		h.result, h.choice = r, c
		done = append(done, h)
	}
	if err := writeLines(path, res); err != nil {
		fmt.Fprintf(stderr, "cannot write %s: %s\n", path, strerror(err))
		return exitFor(err)
	}
	for _, h := range done {
		fmt.Fprintf(stdout, "%s hunk %d -> %s, now reads:\n", name, h.index+1, h.choice)
		showLines(stdout, h.result, 15)
	}
	if left := len(hunks) - len(done); left > 0 {
		fmt.Fprintf(stdout, "%s: %d hunk(s) still unresolved\n", name, left)
	}
	return 0
}

// readLines splits the file after each "\n"; the last line may lack one. A
// path that opens but cannot be read (a directory) reads as empty, as perl's
// readline did.
func readLines(path string) ([]string, error) {
	f, err := os.Open(path)
	if err != nil {
		return nil, err
	}
	defer f.Close()
	b, err := io.ReadAll(f)
	if err != nil {
		return nil, nil
	}
	return splitLines(string(b)), nil
}

func splitLines(s string) []string {
	var l []string
	for s != "" {
		i := strings.IndexByte(s, '\n')
		if i < 0 {
			l = append(l, s)
			break
		}
		l = append(l, s[:i+1])
		s = s[i+1:]
	}
	return l
}

// writeLines truncates and rewrites the file in place, as perl's open '>'
// did: the inode, mode and any symlink stay.
func writeLines(path string, lines []string) error {
	f, err := os.OpenFile(path, os.O_WRONLY|os.O_CREATE|os.O_TRUNC, 0o666)
	if err != nil {
		return err
	}
	f.WriteString(strings.Join(lines, ""))
	f.Close()
	return nil
}

// exitFor is perl's die status: the errno, or 255.
func exitFor(err error) int {
	var e syscall.Errno
	if errors.As(err, &e) && e != 0 && int(e) < 256 {
		return int(e)
	}
	return 255
}

// strerror spells the error as C's strerror does ("No such file or directory").
func strerror(err error) string {
	var e syscall.Errno
	if !errors.As(err, &e) {
		return err.Error()
	}
	s := e.Error()
	if s != "" && s[0] >= 'a' && s[0] <= 'z' {
		s = string(s[0]-'a'+'A') + s[1:]
	}
	return s
}

// isMarker says whether l is a 7-character conflict marker of c: the
// marker, then a space and a label, or nothing but an optional "\r".
func isMarker(l string, c byte, label bool) bool {
	if len(l) < 7 {
		return false
	}
	for i := 0; i < 7; i++ {
		if l[i] != c {
			return false
		}
	}
	rest := l[7:]
	if label && strings.HasPrefix(rest, " ") {
		return true
	}
	return rest == "" || rest == "\n" || rest == "\r" || rest == "\r\n"
}

// parseConflicts splits the file into plain lines and hunks. A rebase's
// first side is the remote (upstream); a merge's first side is local.
func parseConflicts(l []string, rebase bool) ([]part, []*hunk) {
	var parts []part
	var hunks []*hunk
	for i := 0; i < len(l); {
		if !isMarker(l[i], '<', true) {
			parts = append(parts, part{line: l[i]})
			i++
			continue
		}
		var a, b, c []string
		sec := &a
		h := &hunk{line: i + 1, raw: []string{l[i]}, index: len(hunks)}
		i++
		for i < len(l) && !isMarker(l[i], '>', true) {
			h.raw = append(h.raw, l[i])
			switch {
			case isMarker(l[i], '|', true):
				sec, h.based = &b, true
			case isMarker(l[i], '=', false):
				sec = &c
			default:
				*sec = append(*sec, l[i])
			}
			i++
		}
		if i < len(l) {
			h.raw = append(h.raw, l[i])
		}
		i++
		h.base = orEmpty(b)
		if rebase {
			h.up, h.loc = orEmpty(a), orEmpty(c)
		} else {
			h.up, h.loc = orEmpty(c), orEmpty(a)
		}
		hunks = append(hunks, h)
		parts = append(parts, part{h: h})
	}
	return parts, hunks
}

func orEmpty(s []string) []string {
	if s == nil {
		return []string{}
	}
	return s
}

func clone(s []string) []string { return append([]string{}, s...) }

// isSpace is perl's \s on bytes.
func isSpace(c byte) bool {
	return c == ' ' || c == '\t' || c == '\n' || c == '\v' || c == '\f' || c == '\r'
}

func isWord(c byte) bool {
	return c == '_' || (c >= '0' && c <= '9') || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
}

func trimSpace(s string) string {
	i, j := 0, len(s)
	for i < j && isSpace(s[i]) {
		i++
	}
	for j > i && isSpace(s[j-1]) {
		j--
	}
	return s[i:j]
}

func isBlank(s string) bool { return trimSpace(s) == "" }

func allWord(s string) bool {
	for i := 0; i < len(s); i++ {
		if !isWord(s[i]) {
			return false
		}
	}
	return true
}

// pair is one matched line of an LCS: x in p (or -1), y in q.
type pair struct{ x, y int }

// lcsPairs is an LCS of p and q, preferring to skip p's line on a tie; when
// the table would be too big, every q line that p has anywhere, with x = -1.
func lcsPairs(p, q []string) []pair {
	n, m := len(p), len(q)
	var pairs []pair
	if n*m > 2_000_000 {
		s := make(map[string]bool, n)
		for _, x := range p {
			s[x] = true
		}
		for y, l := range q {
			if s[l] {
				pairs = append(pairs, pair{-1, y})
			}
		}
		return pairs
	}
	w := m + 1
	t := make([]int32, (n+1)*w)
	for x := n - 1; x >= 0; x-- {
		for y := m - 1; y >= 0; y-- {
			switch {
			case p[x] == q[y]:
				t[x*w+y] = t[(x+1)*w+y+1] + 1
			case t[(x+1)*w+y] >= t[x*w+y+1]:
				t[x*w+y] = t[(x+1)*w+y]
			default:
				t[x*w+y] = t[x*w+y+1]
			}
		}
	}
	for x, y := 0, 0; x < n && y < m; {
		switch {
		case p[x] == q[y]:
			pairs = append(pairs, pair{x, y})
			x++
			y++
		case t[(x+1)*w+y] >= t[x*w+y+1]:
			x++
		default:
			y++
		}
	}
	return pairs
}

// matched marks the lines of q that an LCS with p matches.
func matched(p, q []string) []bool {
	hit := make([]bool, len(q))
	for _, pr := range lcsPairs(p, q) {
		hit[pr.y] = true
	}
	return hit
}

// weak: a line too common to prove anything alone: blank, short, or bare
// org markup (#+begin_verse, #+title:, :drawer:, *, ** -).
func weak(line string) bool {
	s := trimSpace(line)
	if len(s) < 4 {
		return true
	}
	lower := strings.ToLower(s[:min(len(s), 8)])
	for _, pre := range []string{"#+begin_", "#+end_"} {
		if strings.HasPrefix(lower, pre) && allWord(s[len(pre):]) {
			return true
		}
	}
	if strings.HasPrefix(s, "#+") {
		w := strings.TrimSuffix(s[2:], ":")
		if w != "" && allWord(w) {
			return true
		}
	}
	if len(s) >= 3 && s[0] == ':' && s[len(s)-1] == ':' && allWord(s[1:len(s)-1]) {
		return true
	}
	if s[0] == '*' {
		r := strings.TrimLeft(s, "*")
		r = strings.TrimPrefix(r, " ")
		if r == "" || r == "_" || r == "-" {
			return true
		}
	}
	return false
}

// matchedStrong is matched, but a run of matched lines counts only when it
// holds a strong line, so an accidental blank or #+begin_verse elsewhere
// matches nothing.
func matchedStrong(p, q []string) []bool {
	pairs := lcsPairs(p, q)
	hit := make([]bool, len(q))
	for i := 0; i < len(pairs); {
		j := i
		for j+1 < len(pairs) && pairs[j].x >= 0 && pairs[j+1].x == pairs[j].x+1 && pairs[j+1].y == pairs[j].y+1 {
			j++
		}
		strong := false
		for k := i; k <= j; k++ {
			if !weak(q[pairs[k].y]) {
				strong = true
				break
			}
		}
		if strong {
			for k := i; k <= j; k++ {
				hit[pairs[k].y] = true
			}
		}
		i = j + 1
	}
	return hit
}

// upKept is upstream's lines minus the base lines local deleted or changed.
func upKept(h *hunk) []string {
	inl := matched(h.loc, h.base)
	gone := map[string]int{}
	for i, l := range h.base {
		if !inl[i] {
			gone[l]++
		}
	}
	var out []string
	for _, l := range h.up {
		if gone[l] > 0 {
			gone[l]--
			continue
		}
		out = append(out, l)
	}
	return out
}

// localAdded is what local added: its lines that neither the base nor
// upstream already has.
func localAdded(h *hunk) []string {
	inb := matchedStrong(h.base, h.loc)
	var added []string
	for i, l := range h.loc {
		if !inb[i] {
			added = append(added, l)
		}
	}
	inu := matchedStrong(h.up, added)
	var out []string
	for i, l := range added {
		if !inu[i] {
			out = append(out, l)
		}
	}
	return out
}

// joinBlank is two runs of lines, with a blank line between them when the
// file spaces its entries so.
func joinBlank(p, q []string) []string {
	if len(p) == 0 || len(q) == 0 {
		return append(clone(p), q...)
	}
	spaced := false
	for _, l := range append(clone(p), q...) {
		if isBlank(l) {
			spaced = true
			break
		}
	}
	gap := spaced && !isBlank(p[len(p)-1]) && !isBlank(q[0])
	out := clone(p)
	if !strings.HasSuffix(out[len(out)-1], "\n") {
		out[len(out)-1] += "\n"
	}
	if gap {
		out = append(out, "\n")
	}
	return append(out, q...)
}

// norm is the text with every run of whitespace one space, and no space at either end.
func norm(lines []string) string {
	s := strings.Join(lines, "")
	var b strings.Builder
	for i := 0; i < len(s); {
		if isSpace(s[i]) {
			for i < len(s) && isSpace(s[i]) {
				i++
			}
			b.WriteByte(' ')
			continue
		}
		b.WriteByte(s[i])
		i++
	}
	return strings.TrimSuffix(strings.TrimPrefix(b.String(), " "), " ")
}

func class(h *hunk) string {
	if norm(h.up) == norm(h.loc) {
		return "SAME-IGNORING-WHITESPACE"
	}
	if !h.based {
		return "BASE-NOT-SHOWN"
	}
	if norm(h.up) == norm(h.base) {
		return "UPSTREAM-ONLY-WHITESPACE"
	}
	if norm(h.loc) == norm(h.base) {
		return "LOCAL-ONLY-WHITESPACE"
	}
	du, dl := 0, 0
	for _, m := range matched(h.up, h.base) {
		if !m {
			du++
		}
	}
	for _, m := range matched(h.loc, h.base) {
		if !m {
			dl++
		}
	}
	switch {
	case du == 0 && dl == 0:
		return "BOTH-ONLY-ADDED"
	case du == 0 || dl == 0:
		return "ONE-EDITED-OTHER-ADDED"
	}
	return "BOTH-EDITED"
}

var suggest = map[string]string{
	"SAME-IGNORING-WHITESPACE": "upstream", "BOTH-ONLY-ADDED": "both", "ONE-EDITED-OTHER-ADDED": "both",
	"BOTH-EDITED": "decide", "BASE-NOT-SHOWN": "decide",
	"UPSTREAM-ONLY-WHITESPACE": "local", "LOCAL-ONLY-WHITESPACE": "upstream",
}

// showLines prints up to max lines, indented, each ending in exactly one "\n".
func showLines(w io.Writer, lines []string, max int) {
	more := 0
	if len(lines) > max {
		more = len(lines) - max
		lines = lines[:max]
	}
	var b strings.Builder
	for _, l := range lines {
		l = strings.TrimSuffix(l, "\n")
		l = strings.TrimSuffix(l, "\r")
		b.WriteString("  | " + l + "\n")
	}
	if more > 0 {
		fmt.Fprintf(&b, "  ... %d more lines\n", more)
	}
	io.WriteString(w, b.String())
}

func showHunks(w io.Writer, hunks []*hunk, label, name string) {
	n := len(hunks)
	for k, h := range hunks {
		c := class(h)
		fmt.Fprintf(w, "### %s, hunk %d of %d, near line %d: %s (suggested: %d=%s)\n", name, k+1, n, h.line, c, k+1, suggest[c])
		io.WriteString(w, "--- upstream (already on the remote):\n")
		showLines(w, h.up, 40)
		switch {
		case len(h.base) > 0:
			io.WriteString(w, "--- base (before either side changed it):\n")
			showLines(w, h.base, 25)
		case h.based:
			io.WriteString(w, "--- base: nothing (both sides added text here)\n")
		default:
			io.WriteString(w, "--- base: not shown\n")
		}
		fmt.Fprintf(w, "--- local (%s):\n", label)
		showLines(w, h.loc, 40)
	}
}

// parseSpec reads <hunk>=<choice>, the hunk a number (kept as written:
// "01" names no hunk) or all. Like perl's `$', a single trailing "\n" is allowed.
func parseSpec(s string) (string, string, bool) {
	s = strings.TrimSuffix(s, "\n")
	k, c, ok := strings.Cut(s, "=")
	if !ok {
		return "", "", false
	}
	switch c {
	case "upstream", "local", "both", "both-local-first":
	default:
		return "", "", false
	}
	if k == "all" {
		return k, c, true
	}
	if k == "" {
		return "", "", false
	}
	for i := 0; i < len(k); i++ {
		if k[i] < '0' || k[i] > '9' {
			return "", "", false
		}
	}
	return k, c, true
}
