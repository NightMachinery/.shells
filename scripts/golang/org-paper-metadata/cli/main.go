// Package cli exposes the two binaries backed by the same paper package.
package cli

import (
	"bytes"
	"context"
	"encoding/json"
	"flag"
	"fmt"
	"io"
	"os"
	"os/signal"
	"syscall"
	"time"

	"org-paper-metadata/paper"
)

func Main() { os.Exit(mainCode(os.Args[1:])) }

func client(o paper.Options) *paper.Client {
	c := paper.NewClient(o)
	c.Warnf = func(format string, args ...any) { fmt.Fprintf(os.Stderr, "org-paper-metadata: "+format+"\n", args...) }
	return c
}
func mainCode(args []string) int {
	o := paper.EnvOptions()
	flags := flag.NewFlagSet("org-paper-metadata", flag.ContinueOnError)
	bibtex := bibTeXFlags(flags)
	flags.BoolVar(&o.BibTeXBrowser, "bibtex-browser", o.BibTeXBrowser, "allow full-html2 browser recovery for DBLP exports (default: disabled)")
	flags.StringVar(&o.BibTeXSource, "bibtex-source", o.BibTeXSource, "auto (DBLP, DOI, Semantic Scholar), dblp, doi, or semantic-scholar")
	format := flags.String("format", "org", "org or json (JSONL for multiple URLs)")
	resolve := flags.Bool("resolve", false, "resolve the Semantic Scholar page without fetching metadata")
	parse := flags.String("parse", "", "parse stdin: page-data, json, html, fetch-json, or markdown")
	legacy := flags.Bool("from-json", false, "read normalized or Graph API JSON from stdin")
	timeout := flags.Duration("timeout", 3*time.Minute, "timeout per paper")
	flags.StringVar(&o.Scraper, "scraper", o.Scraper, "auto, page-data, api, deepseek, or full-html2")
	flags.Usage = func() {
		fmt.Fprintln(flags.Output(), "usage: org-paper-metadata [options] <paper-url> ...")
		flags.PrintDefaults()
	}
	if err := flags.Parse(args); err != nil {
		if err == flag.ErrHelp {
			return 0
		}
		return 2
	}
	if flags.NArg() == 0 || *timeout <= 0 || (*format != "org" && *format != "json") {
		flags.Usage()
		return 2
	}
	if err := o.Validate(); err != nil {
		fmt.Fprintln(os.Stderr, err)
		return 2
	}
	if *legacy {
		*parse = "json"
	}
	if *parse != "" && flags.NArg() != 1 {
		fmt.Fprintln(os.Stderr, "stdin parsing requires exactly one paper URL")
		return 2
	}
	ctx, stop := signal.NotifyContext(context.Background(), os.Interrupt, syscall.SIGTERM)
	defer stop()
	o.FetchBibTeX = *bibtex
	c := client(o)
	var output bytes.Buffer
	for _, input := range flags.Args() {
		var m paper.Metadata
		var err error
		if *resolve {
			requestCtx, cancel := context.WithTimeout(ctx, *timeout)
			var page string
			page, err = c.Resolve(requestCtx, input)
			cancel()
			if err == nil {
				fmt.Fprintln(&output, page)
			}
		} else if *parse != "" {
			var raw []byte
			raw, err = io.ReadAll(io.LimitReader(os.Stdin, (16<<20)+1))
			if len(raw) > 16<<20 {
				err = fmt.Errorf("stdin exceeds 16 MiB")
			}
			if err == nil {
				switch *parse {
				case "page-data":
					m, err = paper.ParsePageData(raw, input)
				case "json":
					m, err = paper.ParseJSON(raw, input)
				case "html":
					m, err = paper.ParseHTML(raw, input)
				case "fetch-json":
					m, err = paper.ParseFetch(raw, input)
				case "markdown":
					m, err = paper.ParseMarkdown(string(raw), input)
				default:
					err = fmt.Errorf("unknown stdin format: %s", *parse)
				}
			}
			if err == nil && m.First("metadataFetchedAt") == "" {
				m.Stamp("semantic-scholar-"+*parse, time.Now())
			}
		} else {
			requestCtx, cancel := context.WithTimeout(ctx, *timeout)
			m, err = c.Fetch(requestCtx, input)
			cancel()
		}
		if err == nil && !*resolve {
			if *format == "json" {
				err = json.NewEncoder(&output).Encode(m)
			} else {
				var org string
				org, err = paper.RenderOrgWithOptions(m, paper.OrgOptions{NoBibTeX: !*bibtex})
				output.WriteString(org)
			}
		}
		if err != nil {
			fmt.Fprintln(os.Stderr, "org-paper-metadata:", err)
			return 1
		}
	}
	_, err := output.WriteTo(os.Stdout)
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		return 1
	}
	return 0
}
