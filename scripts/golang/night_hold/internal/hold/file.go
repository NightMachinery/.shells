package hold

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
	"time"
)

// The on-disk form is `key: value` lines, deliberately human-readable: the
// first thing anyone does with a hold that will not go away is cat it.
//
// A deadline is also mirrored into the mtime, but nothing reads it any more:
// reload.lua used to, and now asks this binary instead, because only this
// binary knows whether the holder is still alive. It is kept because `ls -l`
// on a stuck hold is the first thing anyone tries, and it costs one syscall.
// An until-live hold has no deadline to mirror and keeps its natural write
// time.

func readHold(path string) (Hold, error) {
	f, err := os.Open(path)
	if err != nil {
		return Hold{}, err
	}
	defer f.Close()

	h := Hold{file: path, Mode: ModeExclusive}
	var sawUntil bool

	sc := bufio.NewScanner(f)
	sc.Buffer(make([]byte, 0, 64*1024), 1024*1024)
	for sc.Scan() {
		key, value, found := strings.Cut(sc.Text(), ":")
		if !found {
			continue
		}
		value = strings.TrimSpace(value)
		switch key {
		case "resource":
			h.Resource = value
		case "holder":
			h.Holder = value
		case "mode":
			if value == ModeShared || value == ModeExclusive {
				h.Mode = value
			}
		case "until":
			n, err := strconv.ParseInt(value, 10, 64)
			if err != nil {
				return Hold{}, fmt.Errorf("unreadable deadline in %s", path)
			}
			// Zero is until-live, not 1970: a hold that ends when its holder
			// does. Distinct from the line being absent, which is a corpse
			// from a half-written acquire.
			if n != 0 {
				h.Until = time.Unix(n, 0)
			}
			sawUntil = true
		case "acquired":
			if n, err := strconv.ParseInt(value, 10, 64); err == nil {
				h.Acquired = time.Unix(n, 0)
			}
		case "pid":
			h.PID, _ = strconv.Atoi(value)
		case "pid-kind":
			h.PIDKind = value
		case "ttl":
			if n, err := strconv.ParseInt(value, 10, 64); err == nil {
				h.TTL = time.Duration(n) * time.Second
			}
		case "host":
			h.Host = value
		case "ttl-fallback":
			h.TTLFallback = value
		case "reason":
			h.Reason = value
		case "match":
			h.Matches = append(h.Matches, value)
		case "path-match":
			h.PathMatches = append(h.PathMatches, value)
		}
	}
	if err := sc.Err(); err != nil {
		return Hold{}, err
	}
	if !sawUntil {
		return Hold{}, fmt.Errorf("no deadline in %s", path)
	}
	if h.Acquired.IsZero() && h.HasDeadline() {
		h.Acquired = h.Until
	}
	return h, nil
}

func writeHold(h Hold) error {
	var b strings.Builder
	fmt.Fprintf(&b, "resource: %s\n", h.Resource)
	fmt.Fprintf(&b, "holder:   %s\n", h.Holder)
	fmt.Fprintf(&b, "mode:     %s\n", h.Mode)
	fmt.Fprintf(&b, "until:    %d\n", untilField(h))
	fmt.Fprintf(&b, "acquired: %d\n", h.Acquired.Unix())
	fmt.Fprintf(&b, "pid:      %d\n", h.PID)
	fmt.Fprintf(&b, "pid-kind: %s\n", h.PIDKind)
	fmt.Fprintf(&b, "ttl:      %d\n", int64(h.TTL.Seconds()))
	fmt.Fprintf(&b, "host:     %s\n", h.Host)
	if h.TTLFallback != "" {
		fmt.Fprintf(&b, "ttl-fallback: %s\n", h.TTLFallback)
	}
	fmt.Fprintf(&b, "reason:   %s\n", h.Reason)
	for _, m := range h.Matches {
		fmt.Fprintf(&b, "match:    %s\n", m)
	}
	for _, m := range h.PathMatches {
		fmt.Fprintf(&b, "path-match: %s\n", m)
	}

	if err := os.WriteFile(h.file, []byte(b.String()), 0o644); err != nil {
		return err
	}
	if !h.HasDeadline() {
		return nil
	}
	// Last, so it is never ahead of the contents.
	return os.Chtimes(h.file, h.Until, h.Until)
}

// untilField is the deadline as the file carries it: seconds, or 0 for a hold
// that has none.
func untilField(h Hold) int64 {
	if !h.HasDeadline() {
		return 0
	}
	return h.Until.Unix()
}

// FormatDuration prints a span the way seconds-fmt-short does, so the binary
// and the zsh it replaced say the same thing.
func FormatDuration(d time.Duration) string {
	if d < 0 {
		d = 0
	}
	s := int(d.Seconds())
	return fmt.Sprintf("%dh:%dm:%ds", s/3600, (s%3600)/60, s%60)
}

// ParseDuration accepts 90s, 30m, 2h, 3d or a bare seconds count, matching
// dur2sec.
func ParseDuration(s string) (time.Duration, error) {
	if s == "" {
		return 0, fmt.Errorf("empty duration")
	}
	unit := time.Second
	switch s[len(s)-1] {
	case 's':
		s = s[:len(s)-1]
	case 'm':
		unit, s = time.Minute, s[:len(s)-1]
	case 'h':
		unit, s = time.Hour, s[:len(s)-1]
	case 'd':
		unit, s = 24*time.Hour, s[:len(s)-1]
	}
	n, err := strconv.Atoi(s)
	if err != nil || n < 0 {
		return 0, fmt.Errorf("bad duration: %s", s)
	}
	return time.Duration(n) * unit, nil
}
