// parallel_sections runs several labelled commands at once and prints their
// output in the order they were given rather than the order they finished.
//
// It exists because shell fan-out for this is all downside: backgrounded
// subshells cannot return anything, so each one has to be routed through a
// temporary file, and reassembling those in order, keeping stderr attributable,
// and getting one exit status out of the set is a lot of bookkeeping to get
// subtly wrong.
//
// The spec is JSON on stdin, so a caller never has to quote a command line
// into a string:
//
//	{"mode": "text",
//	 "sections": [{"label": "default", "argv": ["some-cmd", "--flag"]},
//	              {"label": "work",    "argv": ["some-cmd", "--other"]}]}
//
// In "text" mode the sections' stdout is concatenated in order, separated by a
// blank line. In "json" mode each section's stdout is parsed as one JSON
// document and the lot is emitted as an array -- two bare objects in a row are
// not JSON, so a caller splicing reports together needs the array.
//
// Anything a section writes to stderr is forwarded with its label prefixed, so
// concurrent output stays attributable. A section that fails does not stop the
// others; the exit status is 1 if any of them failed, 0 otherwise.
package main

import (
	"bufio"
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"os"
	"os/exec"
	"sync"
)

type section struct {
	Label string   `json:"label"`
	Argv  []string `json:"argv"`
}

type spec struct {
	Mode     string    `json:"mode"`
	Sections []section `json:"sections"`
}

type result struct {
	stdout []byte
	stderr []byte
	err    error
}

func main() {
	failed, err := run()
	if err != nil {
		fmt.Fprintf(os.Stderr, "parallel_sections: %v\n", err)
		os.Exit(2)
	}
	if failed {
		os.Exit(1)
	}
}

func run() (bool, error) {
	raw, err := io.ReadAll(os.Stdin)
	if err != nil {
		return false, fmt.Errorf("reading the spec from stdin: %w", err)
	}

	var sp spec
	if err := json.Unmarshal(raw, &sp); err != nil {
		return false, fmt.Errorf("parsing the spec: %w", err)
	}

	switch sp.Mode {
	case "", "text", "json":
	default:
		return false, fmt.Errorf("unknown mode %q (want \"text\" or \"json\")", sp.Mode)
	}
	if len(sp.Sections) == 0 {
		return false, fmt.Errorf("the spec has no sections")
	}

	// Results are written by index, never appended, so the output order is the
	// spec order no matter which section finishes first.
	results := make([]result, len(sp.Sections))
	var wg sync.WaitGroup
	for i, sec := range sp.Sections {
		if len(sec.Argv) == 0 {
			results[i] = result{err: fmt.Errorf("empty argv")}
			continue
		}

		wg.Add(1)
		go func(i int, sec section) {
			defer wg.Done()
			results[i] = runSection(sec)
		}(i, sec)
	}
	wg.Wait()

	failed := false
	var texts [][]byte
	jsons := []json.RawMessage{}

	for i, sec := range sp.Sections {
		res := results[i]

		if len(res.stderr) > 0 {
			// Forwarded even when the section succeeded: a warning about
			// serving stale data is exactly the kind of thing worth seeing.
			writeLabeled(os.Stderr, sec.Label, res.stderr)
		}
		if res.err != nil {
			// One dead section must not cost the others their output.
			failed = true
			fmt.Fprintf(os.Stderr, "parallel_sections: section %s failed: %v\n", sec.Label, res.err)
			continue
		}

		trimmed := bytes.TrimSpace(res.stdout)
		if len(trimmed) == 0 {
			continue
		}

		if sp.Mode == "json" {
			if !json.Valid(trimmed) {
				failed = true
				fmt.Fprintf(os.Stderr, "parallel_sections: section %s did not produce valid JSON\n", sec.Label)
				continue
			}
			jsons = append(jsons, json.RawMessage(trimmed))
		} else {
			texts = append(texts, res.stdout)
		}
	}

	out := bufio.NewWriter(os.Stdout)
	defer out.Flush()

	if sp.Mode == "json" {
		encoded, err := json.MarshalIndent(jsons, "", "  ")
		if err != nil {
			return failed, fmt.Errorf("assembling the JSON output: %w", err)
		}
		out.Write(encoded)
		out.WriteByte('\n')

		return failed, nil
	}

	for i, text := range texts {
		if i > 0 {
			// Each section's output already ends in its own newline, so one
			// more makes the blank line between them.
			out.WriteByte('\n')
		}
		out.Write(text)
	}

	return failed, nil
}

func runSection(sec section) result {
	var stdout, stderr bytes.Buffer

	cmd := exec.Command(sec.Argv[0], sec.Argv[1:]...)
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr
	err := cmd.Run()

	return result{stdout: stdout.Bytes(), stderr: stderr.Bytes(), err: err}
}

// writeLabeled prefixes every line with the section label, so that output from
// sections running at the same time stays attributable.
func writeLabeled(w io.Writer, label string, data []byte) {
	scanner := bufio.NewScanner(bytes.NewReader(data))
	scanner.Buffer(make([]byte, 0, 64*1024), 8*1024*1024)
	for scanner.Scan() {
		fmt.Fprintf(w, "[%s] %s\n", label, scanner.Text())
	}
}
