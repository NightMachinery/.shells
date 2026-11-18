package main

import (
	"fmt"
	"regexp"
	"strconv"
	"strings"
)

// option is one choice of a question, as the desk shows it.
type option struct {
	Key   string `json:"key"`
	Label string `json:"label"`
}

// item is one question: a conflicted hunk, or a whole file without markers.
// Body is the hunk's listing (upstream, base and local) as it was asked.
type item struct {
	File    string   `json:"file"`
	Hunk    int      `json:"hunk"`
	Total   int      `json:"total"`
	Whole   bool     `json:"whole"`
	Body    string   `json:"body"`
	Options []option `json:"options,omitempty"`
}

func (it item) name() string {
	if it.Whole {
		return fmt.Sprintf("%s (whole file)", it.File)
	}
	return fmt.Sprintf("%s, hunk %d of %d", it.File, it.Hunk, it.Total)
}

// doubt is a reason not to apply the answer: the agent takes over.
type doubt struct{ msg string }

func (d *doubt) Error() string { return d.msg }

func doubtf(format string, a ...any) *doubt { return &doubt{fmt.Sprintf(format, a...)} }

// The choices ds-conflict-perl understands, by kind of question.
var hunkKeys = map[string]bool{"upstream": true, "local": true, "both": true, "both-local-first": true}
var wholeKeys = map[string]bool{"upstream": true, "local": true}

// parseAnswer reads the desk's answer to items: one line per question,
// "N. <key>: <label>", then optionally a general message. It returns the
// picked key of each item, or a doubt for anything but a plain pick of
// every question: "(no answer)", "Other: ...", a note, a general message,
// or a line that matches no option.
func parseAnswer(text string, items []item) ([]string, *doubt) {
	text = strings.ReplaceAll(text, "\r\n", "\n")
	text = strings.Trim(text, " \t\n")
	if text == "" {
		return nil, doubtf("the answer is empty")
	}
	lines := strings.Split(text, "\n")
	if len(lines) < len(items) {
		return nil, doubtf("the answer has %d line(s) for %d question(s)", len(lines), len(items))
	}
	keys := make([]string, len(items))
	for i, it := range items {
		q := fmt.Sprintf("question %d (%s)", i+1, it.name())
		l := strings.TrimRight(lines[i], " \t")
		num := fmt.Sprintf("%d. ", i+1)
		if !strings.HasPrefix(l, num) {
			return nil, doubtf("line %d of the answer is not the answer to question %d", i+1, i+1)
		}
		rest := l[len(num):]
		switch {
		case rest == "(no answer)":
			return nil, doubtf("%s has no answer", q)
		case strings.HasPrefix(rest, "Other: "):
			return nil, doubtf("%s was answered in your own words", q)
		case strings.Contains(rest, ". Note: "):
			return nil, doubtf("%s has a note", q)
		}
		allowed := hunkKeys
		if it.Whole {
			allowed = wholeKeys
		}
		for _, o := range it.Options {
			if allowed[o.Key] && rest == o.Key+": "+o.Label {
				keys[i] = o.Key
				break
			}
		}
		if keys[i] == "" {
			return nil, doubtf("%s is not answered with one of its options", q)
		}
	}
	if extra := strings.TrimSpace(strings.Join(lines[len(items):], "\n")); extra != "" {
		return nil, doubtf("the answer has a message after the picks")
	}
	return keys, nil
}

// The listing ds-conflict-print writes. h-daily-sync-desk-ask (python, in
// daily-sync.zsh) parses it the same way; keep the two in step.
var (
	headRe    = regexp.MustCompile(`(?m)^### (.+?), hunk (\d+) of (\d+), near line (\d+): (\S+) \(suggested: \d+=(\S+)\)$`)
	nomarkRe  = regexp.MustCompile(`(?m)^### (.+?): no conflict markers`)
	anyHeadRe = regexp.MustCompile(`(?m)^### `)
)

// parseListing returns the questions a listing makes: every hunk, in order,
// then every file without conflict markers.
func parseListing(text string) []item {
	text = strings.ToValidUTF8(text, "�")
	var items []item
	for _, m := range headRe.FindAllStringSubmatchIndex(text, -1) {
		end := len(text)
		if n := anyHeadRe.FindStringIndex(text[m[1]:]); n != nil {
			end = m[1] + n[0]
		}
		body := strings.SplitN(text[m[1]:end], "\nNEXT:", 2)[0]
		k, _ := strconv.Atoi(text[m[4]:m[5]])
		n, _ := strconv.Atoi(text[m[6]:m[7]])
		items = append(items, item{File: text[m[2]:m[3]], Hunk: k, Total: n, Body: strings.Trim(body, "\n")})
	}
	for _, m := range nomarkRe.FindAllStringSubmatch(text, -1) {
		items = append(items, item{File: m[1], Whole: true})
	}
	return items
}

// compareListing checks that a reopened conflict is the one that was asked.
func compareListing(asked, now []item) *doubt {
	for i := 0; i < len(asked) && i < len(now); i++ {
		a, b := asked[i], now[i]
		switch {
		case a.File != b.File || a.Whole != b.Whole || a.Hunk != b.Hunk:
			return doubtf("the conflict changed since the question: question %d was %s, it is now %s", i+1, a.name(), b.name())
		case a.Total != b.Total:
			return doubtf("%s now has %d hunks, not %d as asked", a.File, b.Total, a.Total)
		case a.Body != b.Body:
			return doubtf("%s changed since the question", a.name())
		}
	}
	if len(asked) != len(now) {
		return doubtf("the conflict now has %d question(s), the question had %d", len(now), len(asked))
	}
	return nil
}

// resolveCall is one `daily-sync resolve' of a file: its hunk picks.
type resolveCall struct {
	File  string
	Specs []string
}

// resolveCalls groups the picks by file, in the order the files were asked.
func resolveCalls(items []item, keys []string) []resolveCall {
	var calls []resolveCall
	at := map[string]int{}
	for i, it := range items {
		j, ok := at[it.File]
		if !ok {
			j = len(calls)
			at[it.File] = j
			calls = append(calls, resolveCall{File: it.File})
		}
		spec := fmt.Sprintf("%d=%s", it.Hunk, keys[i])
		if it.Whole {
			spec = "all=" + keys[i]
		}
		calls[j].Specs = append(calls[j].Specs, spec)
	}
	return calls
}
