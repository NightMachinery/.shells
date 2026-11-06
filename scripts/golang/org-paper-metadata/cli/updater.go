package cli

import (
	"context"
	"errors"
	"flag"
	"fmt"
	"org-paper-metadata/paper"
	"org-paper-metadata/update"
	"os"
	"os/signal"
	"strings"
	"syscall"
	"time"
)

func UpdaterMain() { os.Exit(updaterCode(os.Args[1:])) }

func updaterCode(args []string) int {
	o := paper.EnvOptions()
	var u update.Options
	flags := flag.NewFlagSet("org-paper-metadata-updater", flag.ContinueOnError)
	bibtex := bibTeXFlags(flags)
	flags.BoolVar(&o.EnrichDBLP, "dblp-metadata", o.EnrichDBLP, "enrich publication metadata through DBLP SPARQL (default: enabled)")
	flags.BoolVar(&o.BibTeXBrowser, "bibtex-browser", o.BibTeXBrowser, "allow full-html2 browser recovery for DBLP exports (default: disabled)")
	flags.StringVar(&o.BibTeXSource, "bibtex-source", o.BibTeXSource, "auto (DBLP, DOI, Semantic Scholar), dblp, doi, or semantic-scholar")
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
	o.FetchBibTeX = *bibtex
	if u.Write {
		lock, code := acquireUpdaterLock(args)
		if lock == nil {
			return code
		}
		defer lock.Release()
	}
	ctx, stop := signal.NotifyContext(context.Background(), os.Interrupt, syscall.SIGTERM)
	defer stop()
	_, err := update.Run(ctx, flags.Arg(0), u, client(o).Fetch, time.Now(), os.Stdout, os.Stderr)
	if err != nil {
		fmt.Fprintln(os.Stderr, "org-paper-metadata-updater:", err)
		return 1
	}
	return 0
}

// exitLocked is EX_TEMPFAIL: nothing was attempted, so a later retry is safe.
const exitLocked = 75

// acquireUpdaterLock keeps two --write runs off the same machine at once. A
// dry run takes no lock, since it writes nothing.
func acquireUpdaterLock(args []string) (*update.WriteLock, int) {
	path, err := update.DefaultLockPath()
	if err == nil {
		holder := fmt.Sprintf("pid %d since %s: org-paper-metadata-updater %s",
			os.Getpid(), time.Now().Format(time.RFC3339), strings.Join(args, " "))
		var lock *update.WriteLock
		if lock, err = update.AcquireWriteLock(path, holder); err == nil {
			return lock, 0
		}
	}
	fmt.Fprintln(os.Stderr, "org-paper-metadata-updater:", err)
	var locked *update.LockedError
	if errors.As(err, &locked) {
		return nil, exitLocked
	}
	return nil, 1
}
