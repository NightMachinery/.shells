// Self-expiring advisory holds over a named resource -- a repository, a GPU, a
// service -- so one agent can do something atomic without a parallel session
// walking into it, and so the Hammerspoon auto-reloader can be held off by
// several at once. One binary behind the zsh `hold-*` and `hs-reload-*`
// commands and behind Claude Code's PreToolUse guard.
//
// See scripts/docs/holds.md. Stdlib only, like agent_session.
package main

import (
	"errors"
	"flag"
	"fmt"
	"os"
	"time"

	"night_hold/internal/hold"
)

func main() {
	if len(os.Args) < 2 {
		usage(1)
	}

	var err error
	switch os.Args[1] {
	case "-h", "--help", "help":
		usage(0)
	case "acquire":
		err = cmdAcquire(os.Args[2:])
	case "release":
		err = cmdRelease(os.Args[2:])
	case "renew":
		err = cmdRenew(os.Args[2:])
	case "check":
		err = cmdCheck(os.Args[2:])
	case "status":
		err = cmdStatus(os.Args[2:])
	case "holders":
		err = cmdHolders(os.Args[2:])
	case "guard":
		cmdGuard()
		return
	default:
		fmt.Fprintf(os.Stderr, "night_hold: unknown subcommand %q\n", os.Args[1])
		usage(1)
	}

	if err != nil {
		fmt.Fprintf(os.Stderr, "night_hold: %v\n", err)
		os.Exit(1)
	}
}

func usage(code int) {
	fmt.Fprint(os.Stderr, `night_hold <subcommand>

  acquire <resource> [--ttl D] [--reason S] [--match S]... [--shared]
                     [--wait D] [--holder ID]
  release <resource> [--holder ID]
  renew   <resource> [--ttl D] [--holder ID]
  check   <resource> [--shared] [--holder ID]   exit 0 when acquire would succeed
  status  [<resource>]
  holders <resource>                            live holder ids, one per line
  guard                                         PreToolUse hook payload on stdin

A resource is any string. repo:, path:, dir: and file: are resolved to an
absolute path first, so repo:~/scripts and repo:/Users/evar/scripts are one
resource. Everything else passes through, so gpu:0 means what its callers agree.
`)
	os.Exit(code)
}

type matchList []string

func (m *matchList) String() string     { return "" }
func (m *matchList) Set(s string) error { *m = append(*m, s); return nil }

func flagsFor(name string) *flag.FlagSet {
	fs := flag.NewFlagSet(name, flag.ContinueOnError)
	fs.SetOutput(os.Stderr)
	return fs
}

// parsePositional parses flags that may appear on either side of the single
// positional argument.
//
// flag.Parse stops at the first non-flag argument, so `acquire <resource> --ttl
// 10m` parsed nothing at all and every flag silently took its default -- the
// hold was taken for the wrong duration, by the wrong holder, in the wrong
// mode. Re-parsing what is left after each positional is the standard way
// round it.
func parsePositional(fs *flag.FlagSet, args []string) (string, error) {
	var positional string
	seen := false
	for {
		if err := fs.Parse(args); err != nil {
			return "", err
		}
		if fs.NArg() == 0 {
			return positional, nil
		}
		if seen {
			return "", fmt.Errorf("unexpected argument %q", fs.Arg(0))
		}
		positional, seen = fs.Arg(0), true
		args = fs.Args()[1:]
	}
}

func requirePositional(fs *flag.FlagSet, args []string, what string) (string, error) {
	v, err := parsePositional(fs, args)
	if err != nil {
		return "", err
	}
	if v == "" {
		return "", fmt.Errorf("missing %s", what)
	}
	return v, nil
}

func ttlOf(s string) (time.Duration, error) {
	if s == "" {
		return 0, nil
	}
	return hold.ParseDuration(s)
}

func cmdAcquire(args []string) error {
	fs := flagsFor("acquire")
	ttl := fs.String("ttl", "", "how long to hold it (90s, 30m, 2h, 3d)")
	reason := fs.String("reason", "", "why")
	holder := fs.String("holder", "", "override the holder id")
	shared := fs.Bool("shared", false, "let others hold it at the same time")
	wait := fs.String("wait", "", "keep retrying for this long instead of failing when it is held")
	var matches matchList
	fs.Var(&matches, "match", "extra literal that names this resource (repeatable)")
	resource, err := requirePositional(fs, args, "resource")
	if err != nil {
		return err
	}
	d, err := ttlOf(*ttl)
	if err != nil {
		return err
	}

	opts := hold.AcquireOpts{
		Resource: resource, Holder: *holder, TTL: d,
		Reason: *reason, Matches: matches, Shared: *shared,
	}

	store := hold.New()
	var h hold.Hold
	if *wait != "" {
		budget, werr := hold.ParseDuration(*wait)
		if werr != nil {
			return werr
		}
		h, err = store.AcquireWait(opts, budget, 0, func(by hold.Hold) {
			fmt.Fprintf(os.Stderr, "night_hold: %s is held by %s; waiting up to %s\n",
				by.Resource, by.Holder, hold.FormatDuration(budget))
		})
	} else {
		h, err = store.Acquire(opts)
	}
	if err != nil {
		var held hold.ErrHeld
		if errors.As(err, &held) {
			fmt.Fprintf(os.Stderr, "night_hold: %s is held by %s for another %s\n",
				held.By.Resource, held.By.Holder, hold.FormatDuration(held.By.Left(time.Now())))
			fmt.Fprintf(os.Stderr, "  reason: %s\n", held.By.Reason)
			os.Exit(1)
		}
		return err
	}

	fmt.Fprintf(os.Stderr, "night_hold: holding %s for %s (%s); release with hold-release\n",
		h.Resource, hold.FormatDuration(h.Left(time.Now())), h.Reason)
	return nil
}

func cmdRelease(args []string) error {
	fs := flagsFor("release")
	holder := fs.String("holder", "", "override the holder id")
	resource, err := requirePositional(fs, args, "resource")
	if err != nil {
		return err
	}

	h, err := hold.New().Release(resource, *holder, time.Time{})
	if errors.Is(err, hold.ErrNotHeld) {
		fmt.Fprintf(os.Stderr, "night_hold: %s was not held\n", hold.Canonical(resource))
		return nil
	}
	var foreign hold.ErrForeign
	if errors.As(err, &foreign) {
		// Releasing someone else's is not ours to guess at: with concurrent
		// agents it would open the resource under whoever is still working.
		fmt.Fprintf(os.Stderr, "night_hold: %s is held by %s, not by you\n",
			foreign.By.Resource, foreign.By.Holder)
		fmt.Fprintf(os.Stderr, "  to take it anyway: night_hold release %s --holder %s\n",
			resource, foreign.By.Holder)
		os.Exit(1)
	}
	if err != nil {
		return err
	}
	fmt.Fprintf(os.Stderr, "night_hold: released %s\n", h.Resource)
	return nil
}

func cmdRenew(args []string) error {
	fs := flagsFor("renew")
	ttl := fs.String("ttl", "", "how long from now")
	holder := fs.String("holder", "", "override the holder id")
	resource, err := requirePositional(fs, args, "resource")
	if err != nil {
		return err
	}
	d, err := ttlOf(*ttl)
	if err != nil {
		return err
	}

	h, err := hold.New().Renew(resource, *holder, d, time.Time{})
	if errors.Is(err, hold.ErrNotHeld) {
		return fmt.Errorf("%s is not held by you; use acquire", hold.Canonical(resource))
	}
	if err != nil {
		return err
	}
	fmt.Fprintf(os.Stderr, "night_hold: holding %s for %s (%s); release with hold-release\n",
		h.Resource, hold.FormatDuration(h.Left(time.Now())), h.Reason)
	return nil
}

func cmdCheck(args []string) error {
	fs := flagsFor("check")
	holder := fs.String("holder", "", "override the holder id")
	shared := fs.Bool("shared", false, "ask whether a shared acquire would succeed")
	resource, err := requirePositional(fs, args, "resource")
	if err != nil {
		return err
	}

	ok, err := hold.New().Check(resource, *holder, *shared, time.Time{})
	if err != nil {
		return err
	}
	if !ok {
		os.Exit(1)
	}
	return nil
}

func cmdStatus(args []string) error {
	fs := flagsFor("status")
	resource, err := parsePositional(fs, args)
	if err != nil {
		return err
	}

	s := hold.New()
	now := time.Now()
	var holds []hold.Hold
	if resource != "" {
		holds, err = s.Live(hold.Canonical(resource), now, true)
	} else {
		holds, err = s.All(now, true)
	}
	if err != nil {
		return err
	}
	if len(holds) == 0 {
		fmt.Println("holds: none")
		return nil
	}
	for _, h := range holds {
		shared := ""
		if h.Mode == hold.ModeShared {
			shared = " [shared]"
		}
		fmt.Printf("%s held by %s%s, %s left, %s\n",
			h.Resource, h.Holder, shared, hold.FormatDuration(h.Left(now)), h.Reason)
	}
	return nil
}

func cmdHolders(args []string) error {
	fs := flagsFor("holders")
	resource, err := requirePositional(fs, args, "resource")
	if err != nil {
		return err
	}

	holds, err := hold.New().Live(hold.Canonical(resource), time.Now(), true)
	if err != nil {
		return err
	}
	for _, h := range holds {
		fmt.Println(h.Holder)
	}
	return nil
}

// cmdGuard never returns an error to main: every unexpected condition is an
// allow. Exit 2 blocks the tool call, and the message is taken from the JSON
// when it parses, so both are printed.
func cmdGuard() {
	d := hold.New().Guard(os.Stdin, time.Now())
	if !d.Deny {
		os.Exit(0)
	}
	fmt.Println(hold.DenyJSON(d.Reason))
	fmt.Fprintln(os.Stderr, d.Reason)
	os.Exit(2)
}
