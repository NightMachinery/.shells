package cli

import (
	"context"
	"flag"
	"fmt"
	"org-paper-metadata/paper"
	"org-paper-metadata/update"
	"os"
	"os/signal"
	"syscall"
	"time"
)

func UpdaterMain() { os.Exit(updaterCode(os.Args[1:])) }

func updaterCode(args []string) int {
	o := paper.EnvOptions()
	var u update.Options
	flags := flag.NewFlagSet("org-paper-metadata-updater", flag.ContinueOnError)
	bibtex := bibTeXFlags(flags)
	flags.BoolVar(&u.Write, "write", false, "write changes atomically (default: dry run)")
	dry := flags.Bool("dry-run", false, "explicitly select dry run; incompatible with --write")
	flags.BoolVar(&u.Diff, "diff", false, "print a patch instead of per-heading status")
	flags.BoolVar(&u.Force, "force", false, "refresh entries even when fetched within max-age")
	flags.DurationVar(&u.MaxAge, "max-age", 7*24*time.Hour, "skip entries fetched more recently than this")
	flags.DurationVar(&u.Interval, "interval", time.Second, "minimum delay between uncached paper requests")
	flags.DurationVar(&u.Timeout, "timeout", time.Minute, "timeout per paper, including fallbacks")
	flags.StringVar(&o.Scraper, "scraper", o.Scraper, "auto, page-data, api, deepseek, or full-html2")
	flags.Usage = func() {
		fmt.Fprintln(flags.Output(), "usage: org-paper-metadata-updater [options] <directory-root>")
		flags.PrintDefaults()
	}
	if err := flags.Parse(args); err != nil {
		if err == flag.ErrHelp {
			return 0
		}
		return 2
	}
	if flags.NArg() != 1 || (*dry && u.Write) {
		flags.Usage()
		return 2
	}
	if err := o.Validate(); err != nil {
		fmt.Fprintln(os.Stderr, err)
		return 2
	}
	u.NoBibTeX = !*bibtex
	ctx, stop := signal.NotifyContext(context.Background(), os.Interrupt, syscall.SIGTERM)
	defer stop()
	_, err := update.Run(ctx, flags.Arg(0), u, client(o).Fetch, time.Now(), os.Stdout, os.Stderr)
	if err != nil {
		fmt.Fprintln(os.Stderr, "org-paper-metadata-updater:", err)
		return 1
	}
	return 0
}
