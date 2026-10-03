// llm_complete owns completion computation. Text travels on stdin, never argv.
package main

import (
	"encoding/json"
	"fmt"
	"io"
	"os"
)

type DabbrevRequest struct {
	Screen  Screen   `json:"screen"`
	Corpora []string `json:"corpora"`
	State   Cycle    `json:"state"`
}

func decode(r io.Reader, v any) error {
	d := json.NewDecoder(io.LimitReader(r, 16<<20))
	if err := d.Decode(v); err != nil {
		return err
	}
	var extra any
	if err := d.Decode(&extra); err != io.EOF {
		return fmt.Errorf("expected one JSON request")
	}
	return nil
}
func run(args []string, in io.Reader, out, errs io.Writer) int {
	if len(args) == 0 {
		fmt.Fprintln(errs, "llm_complete: usage: llm_complete dabbrev")
		return 2
	}
	switch args[0] {
	case "terminal":
		var r TerminalRequest
		if err := decode(in, &r); err != nil {
			fmt.Fprintln(errs, "llm_complete: invalid terminal request")
			return 1
		}
		err := terminalDabbrev(r)
		if err != nil {
			notify(r, err.Error())
			return 1
		}
		return 0
	case "dabbrev":
		var r DabbrevRequest
		if err := decode(in, &r); err != nil {
			fmt.Fprintln(errs, "llm_complete: invalid request")
			return 1
		}
		e, err := expand(r.Screen, r.Corpora, r.State)
		if err != nil {
			fmt.Fprintln(errs, "llm_complete:", err)
			return 1
		}
		json.NewEncoder(out).Encode(e)
		return 0
	default:
		fmt.Fprintln(errs, "llm_complete: unknown subcommand")
		return 2
	}
}
func main() { os.Exit(run(os.Args[1:], os.Stdin, os.Stdout, os.Stderr)) }
