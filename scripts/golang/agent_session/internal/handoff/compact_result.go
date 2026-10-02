package handoff

import (
	"bufio"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"os"
	"strings"
)

// ValidateCompactResult checks Claude's full stream, including the successful
// terminal result and exact session identity. A process exit status alone does
// not establish that /compact ran on the selected session.
func ValidateCompactResult(path, expected string, progress io.Writer) error {
	if expected == "" {
		return errors.New("expected session id is required")
	}
	f, err := os.Open(path)
	if err != nil {
		return err
	}
	defer f.Close()
	r := bufio.NewReader(f)
	boundary, success, noop := false, false, false
	var resultText string
	resultCount := 0
	for line := 1; ; line++ {
		data, readErr := r.ReadBytes('\n')
		if len(strings.TrimSpace(string(data))) > 0 {
			var rec struct {
				Type, Subtype string
				SessionID     string `json:"session_id"`
				IsError       bool   `json:"is_error"`
				Result        string
			}
			if err = json.Unmarshal(data, &rec); err != nil {
				return fmt.Errorf("compact stream line %d: %w", line, err)
			}
			if success {
				return errors.New("compact stream has data after terminal result")
			}
			if rec.SessionID != "" && rec.SessionID != expected {
				return fmt.Errorf("compact stream session %q does not match %q", rec.SessionID, expected)
			}
			if rec.Type == "system" && rec.Subtype == "compact_boundary" {
				if rec.SessionID != expected {
					return errors.New("compact boundary has no matching session id")
				}
				boundary = true
			}
			if rec.Type == "result" {
				resultCount++
				if rec.SessionID != expected {
					return errors.New("compact result has no matching session id")
				}
				if rec.Subtype != "success" || rec.IsError {
					return fmt.Errorf("Claude compact result failed (%s): %s", rec.Subtype, rec.Result)
				}
				success = true
				resultText = rec.Result
				// The Claude SDK slash-command docs explicitly describe this no-op for
				// a session containing only a prompt. No arbitrary prose is accepted.
				noop = strings.TrimSpace(rec.Result) == "Not enough messages to compact."
			}
		}
		if readErr == io.EOF {
			break
		}
		if readErr != nil {
			return readErr
		}
	}
	if !success || resultCount != 1 {
		return errors.New("compact stream requires exactly one successful terminal result")
	}
	if !boundary && !noop {
		if len(resultText) > 1024 {
			resultText = resultText[:1024] + "…"
		}
		return fmt.Errorf("compact stream has no compact_boundary or recognized insufficient-history no-op; Claude result: %s", resultText)
	}
	if !boundary && noop && progress != nil {
		fmt.Fprintln(progress, "Claude /compact skipped: insufficient conversation history")
	}
	return nil
}
