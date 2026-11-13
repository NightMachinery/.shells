package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"regexp"
	"sort"
	"strconv"
	"strings"
)

var ircLogLineRe = regexp.MustCompile(
	`^([^:]*/+)?([^/]+)/+(\d{4})-(\d{2})-(\d{2})\.log:(\d+):\[(\d{2}):(\d{2}):(\d{2})\]\s+<([^>]+)>\s+(.*)$`,
)

type ircMessage struct {
	prefix     string
	chat       string
	year       int
	month      int
	day        int
	lineNumber int
	hour       int
	minute     int
	second     int
	sender     string
	text       string
}

func parseSelfUsernames(env string) []string {
	if env == "" {
		return nil
	}
	parts := strings.Split(env, "|")
	var out []string
	for _, p := range parts {
		if p != "" {
			out = append(out, p)
		}
	}
	return out
}

func isSelfSender(sender string, selfUsernames []string) bool {
	for _, u := range selfUsernames {
		if strings.EqualFold(sender, u) {
			return true
		}
	}
	return false
}

func parseIntOrDefault(s string, def int) int {
	if s == "" {
		return def
	}
	v, err := strconv.Atoi(s)
	if err != nil {
		return def
	}
	return v
}

func sortIRCLogs(r io.Reader, w io.Writer, selfEnv string) error {
	selfUsernames := parseSelfUsernames(selfEnv)
	br := bufio.NewReader(r)

	var messages []ircMessage
	for {
		line, err := br.ReadString('\n')
		if err != nil && err != io.EOF {
			return err
		}
		if len(line) > 0 {
			line = strings.TrimSuffix(line, "\n")
			if m := ircLogLineRe.FindStringSubmatch(line); m != nil {
				sender := m[10]
				if !isSelfSender(sender, selfUsernames) {
					messages = append(messages, ircMessage{
						prefix:     m[1],
						chat:       m[2],
						year:       parseIntOrDefault(m[3], 1),
						month:      parseIntOrDefault(m[4], 1),
						day:        parseIntOrDefault(m[5], 1),
						lineNumber: parseIntOrDefault(m[6], 1),
						hour:       parseIntOrDefault(m[7], 1),
						minute:     parseIntOrDefault(m[8], 1),
						second:     parseIntOrDefault(m[9], 1),
						sender:     sender,
						text:       m[11],
					})
				}
			}
		}
		if err == io.EOF {
			break
		}
	}

	sort.SliceStable(messages, func(i, j int) bool {
		a, b := messages[i], messages[j]
		if a.year != b.year {
			return a.year > b.year
		}
		if a.month != b.month {
			return a.month > b.month
		}
		if a.day != b.day {
			return a.day > b.day
		}
		if a.hour != b.hour {
			return a.hour > b.hour
		}
		if a.minute != b.minute {
			return a.minute > b.minute
		}
		return a.second > b.second
	})

	bw := bufio.NewWriter(w)
	for _, m := range messages {
		if _, err := fmt.Fprintf(
			bw,
			"%s%s/%04d-%02d-%02d.log:%d:[%02d:%02d] <%s> %s\n",
			m.prefix, m.chat, m.year, m.month, m.day, m.lineNumber, m.hour, m.minute, m.sender, m.text,
		); err != nil {
			return err
		}
	}
	return bw.Flush()
}

func main() {
	if err := sortIRCLogs(os.Stdin, os.Stdout, os.Getenv("IRC_USERNAMES")); err != nil {
		fmt.Fprintf(os.Stderr, "irc-log-sort: %v\n", err)
		os.Exit(1)
	}
}
