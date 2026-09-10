// Renders coding-agent session transcripts as markdown or org, and lists,
// names, previews and locates them for the zsh pickers and the kitty hotkey.
// One binary, one adapter per agent: `agent_session <agent> <subcommand>`.
// Called from [agfi:h-agent-session-render], [agfi:h-agent-session-select-fz]
// and friends.
//
// Stdlib only, so `go build` needs no network and works on hosts without a
// module cache.

package main

import (
	"bufio"
	"errors"
	"flag"
	"fmt"
	"os"
	"runtime"
	"strings"
	"sync"

	"agent_session/internal/agy"
	"agent_session/internal/claude"
	"agent_session/internal/codex"
	"agent_session/internal/session"
	"agent_session/internal/turns"
)

// The agents this binary knows, in the order `agents` lists them.
var agentNames = []string{"claude", "codex", "agy"}

var adapters = map[string]session.Adapter{
	"claude": claude.Adapter{},
	"codex":  codex.Adapter{},
	"agy":    agy.Adapter{},
}

func main() {
	args := os.Args[1:]
	if len(args) == 0 {
		usage()
	}

	switch args[0] {
	case "-h", "--help", "help":
		usage()
	case "agents":
		for _, a := range agentNames {
			fmt.Println(a)
		}
		return
	case "live-all":
		if err := cmdLiveAll(args[1:]); err != nil {
			session.Fatal(err.Error())
		}
		return
	}

	ad, ok := adapters[args[0]]
	if !ok {
		fmt.Fprintf(os.Stderr, "agent_session: unknown agent: %s\n", args[0])
		usage()
	}
	if len(args) < 2 {
		usage()
	}

	agent, sub, argv := args[0], args[1], args[2:]
	var err error
	switch sub {
	case "render":
		err = cmdRender(ad, argv)
	case "list":
		err = cmdList(ad, argv)
	case "name":
		err = cmdName(ad, argv)
	case "meta":
		err = cmdMeta(ad, argv)
	case "preview":
		err = cmdPreview(ad, argv)
	case "live":
		err = cmdLive(ad, argv)
	case "-h", "--help", "help":
		usage()
	default:
		fmt.Fprintf(os.Stderr, "agent_session: unknown subcommand: %s\n", sub)
		usage()
	}

	if errors.Is(err, session.ErrUnsupported) {
		// Its own exit status, so a caller can tell "this agent has no such
		// thing" from "it failed".
		fmt.Fprintf(os.Stderr, "agent_session: %s does not support %s\n", agent, sub)
		os.Exit(3)
	}
	if err != nil {
		session.Fatal(sub + ": " + err.Error())
	}
}

func usage() {
	fmt.Fprint(os.Stderr, `usage: agent_session <agent> <subcommand> [flags] args...
       agent_session agents                          #: the known agents, one per line
       agent_session live-all <agent>=<root>...      #: every agent's live sessions, concurrently

  agent_session <agent> render [flags] <transcript>   #: transcript -> markdown/org on stdout
  agent_session <agent> list   [flags] <root>...      #: TSV of sessions, one per transcript
  agent_session <agent> name           <transcript>   #: session name, empty if unnamed
  agent_session <agent> meta           <transcript>   #: id <TAB> name <TAB> cwd
  agent_session <agent> preview [flags] <transcript>  #: fzf preview body for a session
  agent_session <agent> live           <root>...      #: TSV of live sessions (pid, id, name, cwd, transcript, tmux, status)

agents: claude (Claude Code; roots are <config-home>/projects directories)
        codex  (Codex CLI; roots are <CODEX_HOME>/sessions directories)
        agy    (Antigravity; roots are antigravity-cli/brain directories)

render flags:
  -format md|org|org-pandoc   output syntax (default md). org-pandoc pipes the
                              markdown through pandoc, in parallel chunks
  -max-block-lines N          elide code blocks longer than N lines (0 = never)
  -diff                       render Edit as a unified diff (default true)
  -subagents                  inline the transcripts of spawned subagents (default true)
  -jobs N                     worker count (default: CPU count)
  -pandoc PATH                pandoc binary for org-pandoc (default "pandoc")

preview flags:
  -bytes N              how much of the transcript's tail to read (default 409600)
  -color                emit ANSI colour (default true; NO_COLOR also disables it)

list flags:
  -cwd DIR              only sessions that ran in DIR
  -snippet-len N        max snippet width (default 120)
  -name-len N           max session-name width (default 40)
  -subagents            also list subagent transcripts
  -only <transcript>    list exactly this transcript rather than walking the
                        roots; repeatable, for a caller that knows its paths
  -last-by any|user     which record dates a session: any of them (default), or
                        only the last message the user typed
  -jobs N               worker count (default: CPU count)

Several roots may be given: they are merged and sorted together. With more than
one, each relative path is prefixed by its root's label -- the first path
component in which the roots differ, e.g. .claude / .claude-work -- since the
same project appears under each. A single root is listed exactly as before.

list emits: epoch <TAB> path <TAB> local time <TAB> name <TAB> relative path
<TAB> snippet. The name is empty for a session that has none; its id is in the
relative path either way. Rows are newest first.
`)
	os.Exit(2)
}

func cmdRender(ad session.Adapter, argv []string) error {
	fs := flag.NewFlagSet("render", flag.ExitOnError)
	format := fs.String("format", "md", "output syntax: md, org or org-pandoc")
	maxBlock := fs.Int("max-block-lines", 0, "elide code blocks longer than N lines (0 = never)")
	diff := fs.Bool("diff", true, "render Edit tool calls as a unified diff")
	jobs := fs.Int("jobs", runtime.NumCPU(), "worker count")
	pandocBin := fs.String("pandoc", "pandoc", "pandoc binary, for -format=org-pandoc")
	subagentsP := fs.Bool("subagents", true, "inline the transcripts of spawned subagents")
	fs.Parse(session.GuardPathArgs(fs, argv))

	input := fs.Arg(0)
	if input == "" {
		return errors.New("no input file given")
	}

	doc, err := ad.Document(input, session.DocOpts{Subagents: *subagentsP})
	if err != nil {
		return err
	}
	out, err := turns.Render(doc, turns.Options{
		Format:   *format,
		MaxBlock: *maxBlock,
		Diff:     *diff,
		Jobs:     *jobs,
		Pandoc:   *pandocBin,
	})
	if err != nil {
		return err
	}

	w := bufio.NewWriter(os.Stdout)
	defer w.Flush()
	_, err = w.WriteString(out)
	return err
}

// A repeatable path flag: `-only <transcript>`, once per transcript.
type pathList []string

func (p *pathList) String() string { return strings.Join(*p, " ") }

func (p *pathList) Set(v string) error {
	if v == "" {
		return errors.New("empty path")
	}
	*p = append(*p, v)
	return nil
}

func cmdList(ad session.Adapter, argv []string) error {
	fs := flag.NewFlagSet("list", flag.ExitOnError)
	cwd := fs.String("cwd", "", "only sessions that ran in this directory")
	snippetLen := fs.Int("snippet-len", 120, "max snippet width, in runes")
	nameLen := fs.Int("name-len", 40, "max session-name width, in runes")
	subagentsP := fs.Bool("subagents", false, "also list subagent transcripts")
	jobs := fs.Int("jobs", runtime.NumCPU(), "worker count")
	lastBy := fs.String("last-by", session.LastByAny, "which record dates a session: any or user")
	var only pathList
	fs.Var(&only, "only", "list exactly this transcript instead of walking the roots; repeatable")
	fs.Parse(session.GuardPathArgs(fs, argv))

	switch *lastBy {
	case session.LastByAny, session.LastByUser:
	default:
		return fmt.Errorf("-last-by: want %s or %s, got %q", session.LastByAny, session.LastByUser, *lastBy)
	}

	infos, err := ad.List(fs.Args(), session.ListOpts{
		Cwd:        *cwd,
		Subagents:  *subagentsP,
		SnippetLen: *snippetLen,
		NameLen:    *nameLen,
		Jobs:       *jobs,
		Only:       only,
		LastBy:     *lastBy,
	})
	if err != nil {
		return err
	}
	session.Sort(infos)

	w := bufio.NewWriter(os.Stdout)
	defer w.Flush()
	for _, s := range infos {
		// A snippet is transcript text, so it can carry the same stray bytes
		// a rendered document can; a row of it goes into an fzf line.
		w.WriteString(turns.ScrubText(s.Row()) + "\n")
	}
	return nil
}

func cmdName(ad session.Adapter, argv []string) error {
	if len(argv) == 0 {
		return errors.New("no input file given")
	}
	name, err := ad.Name(argv[0])
	if err != nil {
		return err
	}
	if name != "" {
		fmt.Println(name)
	}
	return nil
}

func cmdMeta(ad session.Adapter, argv []string) error {
	if len(argv) == 0 {
		return errors.New("no input file given")
	}
	m, err := ad.Meta(argv[0])
	if err != nil {
		return err
	}
	fmt.Println(m.Row())
	return nil
}

func cmdPreview(ad session.Adapter, argv []string) error {
	fs := flag.NewFlagSet("preview", flag.ExitOnError)
	window := fs.Int64("bytes", 400<<10,
		"how much of the transcript's tail to read; 0 or less reads all of it")
	colorP := fs.Bool("color", true, "emit ANSI colour")
	fs.Parse(session.GuardPathArgs(fs, argv))

	if fs.NArg() == 0 {
		return errors.New("no session file given")
	}

	// NO_COLOR is the cross-tool convention, and costs one lookup to honour.
	// Its presence is what counts, whatever the value.
	_, noColor := os.LookupEnv("NO_COLOR")

	out, err := ad.Preview(fs.Arg(0), session.PreviewOpts{Bytes: *window, Color: *colorP && !noColor})
	if err != nil {
		return err
	}
	os.Stdout.WriteString(turns.ScrubText(out))
	return nil
}

// live-all lists the live sessions of several agents at once, from specs of
// the form `<agent>=<root>` -- repeated for an agent with several roots.
//
// One process rather than one per agent, and the adapters run concurrently,
// because the cost here is external commands: `claude agents --json` takes
// 190ms, tracing the Codex locks 40ms, the process table 115ms. Run in
// sequence from zsh that was near a second before anything appeared on
// screen; run together, sharing one process table and one `tmux list-panes`
// (see [proc.ListShared]), it is about a quarter of that.
//
// An agent that fails contributes no rows rather than failing the call, which
// is what the zsh loop did before: a host without Codex installed is not an
// error. Rows stay grouped by agent, in the order the specs named them, each
// group in its own adapter's order, so the output is what concatenating the
// separate calls produced.
func cmdLiveAll(args []string) error {
	if len(args) == 0 {
		return errors.New("live-all: no <agent>=<root> given")
	}

	roots := map[string][]string{}
	var order []string
	for _, spec := range args {
		agent, root, ok := strings.Cut(spec, "=")
		if !ok || agent == "" || root == "" {
			return fmt.Errorf("live-all: not an <agent>=<root> spec: %s", spec)
		}
		if _, known := adapters[agent]; !known {
			return fmt.Errorf("live-all: unknown agent: %s", agent)
		}
		if _, seen := roots[agent]; !seen {
			order = append(order, agent)
		}
		roots[agent] = append(roots[agent], root)
	}

	rows := make([][]session.Live, len(order))
	var wg sync.WaitGroup
	for i, agent := range order {
		wg.Add(1)
		go func(i int, agent string) {
			defer wg.Done()
			out, err := adapters[agent].Live(roots[agent])
			if err != nil {
				return
			}
			rows[i] = out
		}(i, agent)
	}
	wg.Wait()

	w := bufio.NewWriter(os.Stdout)
	defer w.Flush()
	for _, group := range rows {
		for _, r := range group {
			w.WriteString(r.Row() + "\n")
		}
	}
	return nil
}

func cmdLive(ad session.Adapter, argv []string) error {
	rows, err := ad.Live(argv)
	if err != nil {
		return err
	}
	w := bufio.NewWriter(os.Stdout)
	defer w.Flush()
	for _, r := range rows {
		w.WriteString(r.Row() + "\n")
	}
	return nil
}
