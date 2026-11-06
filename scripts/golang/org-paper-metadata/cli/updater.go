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
	var lockWait time.Duration
	flags.Var(lockWaitFlag{&lockWait}, "lock-wait", "with --write, how long to wait for another run's write lock: 0 refuses at once (exit 75), -1 waits indefinitely, or a duration such as 30m")
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
	ctx, stop := signal.NotifyContext(context.Background(), os.Interrupt, syscall.SIGTERM)
	defer stop()
	if u.Write {
		lock, code := acquireUpdaterLock(ctx, args, lockWait)
		if lock == nil {
			return code
		}
		defer lock.Release()
	}
	_, err := update.Run(ctx, flags.Arg(0), u, client(o).Fetch, time.Now(), os.Stdout, os.Stderr)
	if err != nil {
		fmt.Fprintln(os.Stderr, "org-paper-metadata-updater:", err)
		return 1
	}
	return 0
}

// exitLocked is EX_TEMPFAIL: nothing was attempted, so a later retry is safe.
const exitLocked = 75

// lockWaitPoll is how often a waiting run retries the lock. A wait still ends
// on time, since the last delay is cut to what is left, and Ctrl-C ends it at once.
var lockWaitPoll = time.Minute

// acquireUpdaterLock keeps two --write runs off the same machine at once. A
// dry run takes no lock, since it writes nothing.
func acquireUpdaterLock(ctx context.Context, args []string, wait time.Duration) (*update.WriteLock, int) {
	path, err := update.DefaultLockPath()
	if err == nil {
		holder := func() string {
			return fmt.Sprintf("pid %d since %s: org-paper-metadata-updater %s",
				os.Getpid(), time.Now().Format(time.RFC3339), strings.Join(args, " "))
		}
		waiting := func(e *update.LockedError) {
			fmt.Fprintf(os.Stderr, "org-paper-metadata-updater: waiting for the write lock %s (%s)\n", e.Path, e.Holder)
		}
		var lock *update.WriteLock
		if lock, err = update.WaitWriteLock(ctx, path, holder, wait, lockWaitPoll, waiting); err == nil {
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

// lockWaitFlag is a duration that also accepts -1, meaning no limit.
type lockWaitFlag struct{ d *time.Duration }

func (f lockWaitFlag) String() string {
	switch {
	case f.d == nil || *f.d == 0:
		return "0"
	case *f.d < 0:
		return "-1"
	}
	return f.d.String()
}

func (f lockWaitFlag) Set(s string) error {
	if s == "-1" {
		*f.d = -1
		return nil
	}
	d, err := time.ParseDuration(s)
	if err != nil || d < 0 {
		return fmt.Errorf("want 0, -1 (no limit), or a duration such as 30m")
	}
	*f.d = d
	return nil
}
