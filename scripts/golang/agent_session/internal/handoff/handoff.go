package handoff

import (
	"bufio"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"
	"time"

	"agent_session/internal/claude"
)

type Options struct {
	Mode, Cwd, Transcript, Codex string
	Timeout                      time.Duration
	Args                         []string
	Progress                     io.Writer
}

// ConfigArguments validates the deliberately narrow app-server option subset.
// Configuration stays on the command line so Codex parses TOML, layered config,
// and dotted overrides itself, rather than duplicating that parser here.
func ConfigArguments(args []string) ([]string, error) {
	out := make([]string, 0, len(args))
	for i := 0; i < len(args); i++ {
		arg := args[i]
		key, value, inline := strings.Cut(arg, "=")
		if key != "--model" && key != "--config" {
			return nil, fmt.Errorf("unsupported app-server option %q; only --model and --config are accepted", key)
		}
		if !inline {
			i++
			if i >= len(args) {
				return nil, fmt.Errorf("%s needs a value", key)
			}
			value = args[i]
		}
		if value == "" {
			return nil, fmt.Errorf("%s needs a nonempty value", key)
		}
		// app-server accepts --config; model is a config override, not a server flag.
		if key == "--model" {
			b, _ := json.Marshal(value)
			out = append(out, "--config", "model="+string(b))
		} else {
			out = append(out, "--config", value)
		}
	}
	return out, nil
}

func Run(ctx context.Context, o Options) (id string, err error) {
	if o.Mode != "native" && o.Mode != "compact" {
		return "", errors.New("mode must be native or compact")
	}
	if o.Cwd == "" {
		return "", errors.New("cwd is required")
	}
	o.Cwd, err = filepath.Abs(o.Cwd)
	if err != nil {
		return "", err
	}
	stat, err := os.Stat(o.Cwd)
	if err != nil {
		return "", err
	}
	if !stat.IsDir() {
		return "", errors.New("cwd is not a directory")
	}
	o.Transcript, err = filepath.Abs(o.Transcript)
	if err != nil {
		return "", err
	}
	stat, err = os.Stat(o.Transcript)
	if err != nil {
		return "", err
	}
	if !stat.Mode().IsRegular() {
		return "", errors.New("transcript is not a regular file")
	}
	argv, err := ConfigArguments(o.Args)
	if err != nil {
		return "", err
	}
	if o.Timeout <= 0 {
		return "", errors.New("timeout must be positive")
	}
	if o.Codex == "" {
		o.Codex = "codex"
	}
	if o.Progress == nil {
		o.Progress = io.Discard
	}
	ctx, cancel := context.WithTimeout(ctx, o.Timeout)
	defer cancel()
	var items []historyItem
	if o.Mode == "native" {
		_, err = claude.HandoffDocument(o.Transcript)
		if err != nil {
			return "", err
		}
	}
	if o.Mode == "compact" {
		items, err = history(o.Transcript)
		if err != nil {
			return "", err
		}
	}
	c, err := start(ctx, o.Codex, argv, o.Progress)
	if err != nil {
		return "", err
	}
	defer c.close()
	if err = c.call("initialize", map[string]any{"clientInfo": map[string]string{"name": "agent_session_handoff", "version": "1"}, "capabilities": map[string]bool{"experimentalApi": true}}, nil); err != nil {
		return "", err
	}
	if err = c.send(map[string]any{"method": "initialized", "params": map[string]any{}}); err != nil {
		return "", err
	}
	if o.Mode == "native" {
		return native(c, o)
	}
	id, err = compact(c, o, items)
	if err == nil {
		c.close()
		err = verifyPersistent(ctx, o, argv, id)
	}
	if err != nil && id != "" {
		return "", fmt.Errorf("%w; destination thread %s exists, recover with codex resume %s", err, id, id)
	}
	return id, err
}

func readThread(c *client, id string) error {
	if !validID(id) {
		return fmt.Errorf("invalid destination thread id %q", id)
	}
	var result struct {
		Thread struct {
			ID string `json:"id"`
		} `json:"thread"`
	}
	if err := c.call("thread/read", map[string]any{"threadId": id, "includeTurns": false}, &result); err != nil {
		return err
	}
	if result.Thread.ID != id {
		return fmt.Errorf("thread/read returned %q for exact destination %q", result.Thread.ID, id)
	}
	return nil
}
func validID(s string) bool {
	if len(s) != 36 {
		return false
	}
	for i, r := range s {
		if i == 8 || i == 13 || i == 18 || i == 23 {
			if r != '-' {
				return false
			}
			continue
		}
		if !(r >= '0' && r <= '9' || r >= 'a' && r <= 'f') {
			return false
		}
	}
	return true
}
func native(c *client, o Options) (string, error) {
	title, err := (claude.Adapter{}).Name(o.Transcript)
	if err != nil {
		return "", err
	}
	var reply struct {
		ImportID string `json:"importId"`
	}
	params := map[string]any{"migrationSource": "claude-code", "migrationItems": []any{map[string]any{"itemType": "SESSIONS", "cwd": o.Cwd, "description": "Import selected Claude conversation only", "details": map[string]any{"sessions": []any{map[string]any{"path": o.Transcript, "cwd": o.Cwd, "title": title}}}}}}
	fmt.Fprintln(o.Progress, "Importing selected Claude session through Codex")
	if err = c.call("externalAgentConfig/import", params, &reply); err != nil {
		return "", err
	}
	if reply.ImportID == "" {
		return "", errors.New("import response has no importId")
	}
	for {
		m, err := c.event()
		if err != nil {
			return "", err
		}
		if m.Method != "externalAgentConfig/import/completed" {
			continue
		}
		var done struct {
			ImportID string `json:"importId"`
			Results  []struct {
				ItemType  string                                      `json:"itemType"`
				Successes []struct{ ItemType, Source, Target string } `json:"successes"`
				Failures  []struct{ Source, Message string }          `json:"failures"`
			} `json:"itemTypeResults"`
		}
		if err = json.Unmarshal(m.Params, &done); err != nil {
			return "", err
		}
		if done.ImportID != reply.ImportID {
			continue
		}
		var targets []string
		for _, r := range done.Results {
			if r.ItemType != "SESSIONS" {
				continue
			}
			for _, f := range r.Failures {
				if f.Source == "" || f.Source == o.Transcript {
					return "", fmt.Errorf("session import failed: %s", f.Message)
				}
			}
			for _, s := range r.Successes {
				if s.ItemType == "SESSIONS" && s.Source == o.Transcript {
					targets = append(targets, s.Target)
				}
			}
		}
		if len(targets) != 1 {
			return "", fmt.Errorf("import completed without exactly one success mapped to selected transcript (got %d)", len(targets))
		}
		id, err := targetID(targets[0])
		if err != nil {
			return "", err
		}
		if err = readThread(c, id); err != nil {
			return "", fmt.Errorf("import target is not locally resumable: %w; target %q", err, targets[0])
		}
		return id, nil
	}
}

// Import may return a thread id or a local rollout path. A path is resolved
// only from its own session_meta record, never from a glob or newest rollout.
func targetID(target string) (string, error) {
	if validID(target) {
		return target, nil
	}
	if !filepath.IsAbs(target) || filepath.Ext(target) != ".jsonl" {
		return "", fmt.Errorf("import returned unsupported target %q", target)
	}
	st, err := os.Stat(target)
	if err != nil {
		return "", fmt.Errorf("cannot inspect import target %q: %w", target, err)
	}
	if !st.Mode().IsRegular() {
		return "", fmt.Errorf("import target %q is not a regular rollout file", target)
	}
	f, err := os.Open(target)
	if err != nil {
		return "", fmt.Errorf("cannot read import target %q: %w", target, err)
	}
	defer f.Close()
	r := bufio.NewReader(io.LimitReader(f, (1<<20)+1))
	for line := 0; line < 100; line++ {
		data, err := r.ReadBytes('\n')
		if len(data) > 1<<20 {
			return "", errors.New("import target metadata exceeds safety limit")
		}
		var rec struct {
			Type    string
			Payload struct{ ID string }
		}
		if len(data) > 0 {
			if e := json.Unmarshal(data, &rec); e != nil {
				return "", fmt.Errorf("invalid import target metadata: %w", e)
			}
			if rec.Type == "session_meta" && validID(rec.Payload.ID) {
				return rec.Payload.ID, nil
			}
		}
		if err == io.EOF {
			break
		}
		if err != nil {
			return "", err
		}
	}
	return "", errors.New("import target has no exact session_meta thread id")
}

func compact(c *client, o Options, items []historyItem) (id string, err error) {
	var config struct {
		Config struct {
			Model string `json:"model"`
		} `json:"config"`
	}
	if err = c.call("config/read", map[string]any{"cwd": o.Cwd, "includeLayers": false}, &config); err != nil {
		return "", err
	}
	model := config.Config.Model
	if model == "" {
		var models struct {
			Data []struct {
				Model     string `json:"model"`
				IsDefault bool   `json:"isDefault"`
			} `json:"data"`
		}
		if err = c.call("model/list", map[string]any{}, &models); err != nil {
			return "", err
		}
		for _, m := range models.Data {
			if m.IsDefault {
				if model != "" {
					return "", errors.New("model/list returned multiple defaults")
				}
				model = m.Model
			}
		}
	}
	switch model {
	case "gpt-6-astra", "gpt-6.1-sol", "gpt-6-sol", "gpt-6-luna":
	default:
		return "", fmt.Errorf("compact handoff requires a supported 1.05m model; configured model %q is unsupported, select gpt-6-astra, gpt-6.1-sol, gpt-6-sol or gpt-6-luna", model)
	}
	var started struct {
		Model  string `json:"model"`
		Thread struct {
			ID string `json:"id"`
		} `json:"thread"`
	}
	params := map[string]any{"cwd": o.Cwd, "model": model, "config": map[string]any{"model_context_window": 1050000}, "ephemeral": false, "allowProviderModelFallback": false, "historyMode": "legacy"}
	if err = c.call("thread/start", params, &started); err != nil {
		return "", err
	}
	id = started.Thread.ID
	if !validID(id) {
		return id, errors.New("thread/start returned invalid thread id")
	}
	fmt.Fprintf(o.Progress, "Created destination thread %s; injecting %d historical items\n", id, len(items))
	if started.Model != model {
		return id, fmt.Errorf("thread/start selected model %q instead of %q", started.Model, model)
	}
	if err = c.call("thread/inject_items", map[string]any{"threadId": id, "items": items}, nil); err != nil {
		return id, err
	}
	// Drop notifications from thread creation/injection so only a subsequently
	// started compaction turn can satisfy completion. Events during the RPC are
	// retained, including those sent before compact/start's response.
	c.queue = nil
	if err = c.call("thread/compact/start", map[string]any{"threadId": id}, nil); err != nil {
		return id, err
	}
	compactions := map[string]bool{}
	completions := map[string]bool{}
	var turnID string
	for {
		m, e := c.event()
		if e != nil {
			return id, e
		}
		var event struct {
			ThreadID string `json:"threadId"`
			TurnID   string `json:"turnId"`
			Item     struct {
				Type string `json:"type"`
			} `json:"item"`
			Turn struct {
				ID, Status string
				Error      json.RawMessage
			} `json:"turn"`
		}
		if e = json.Unmarshal(m.Params, &event); e != nil {
			return id, e
		}
		if event.ThreadID != id {
			continue
		}
		switch m.Method {
		case "turn/started":
			if turnID != "" && turnID != event.Turn.ID {
				return id, errors.New("unexpected additional turn during compaction")
			}
			turnID = event.Turn.ID
		case "item/completed":
			if event.Item.Type == "contextCompaction" {
				compactions[event.TurnID] = true
			}
		case "turn/completed":
			if event.Turn.Status != "completed" || len(event.Turn.Error) > 0 && string(event.Turn.Error) != "null" {
				return id, fmt.Errorf("compaction turn %s ended %s: %s", event.Turn.ID, event.Turn.Status, event.Turn.Error)
			}
			completions[event.Turn.ID] = true
		case "error":
			return id, fmt.Errorf("app-server reported compaction error: %s", m.Params)
		}
		for t := range compactions {
			if t != "" && completions[t] && (turnID == "" || turnID == t) {
				if e = readThread(c, id); e != nil {
					return id, e
				}
				return id, nil
			}
		}
	}
}

// Reopening a second app-server proves this is a durable compacted thread,
// rather than merely injected history in the first process's memory.
func verifyPersistent(ctx context.Context, o Options, argv []string, id string) error {
	c, err := start(ctx, o.Codex, argv, o.Progress)
	if err != nil {
		return err
	}
	defer c.close()
	if err = c.call("initialize", map[string]any{"clientInfo": map[string]string{"name": "agent_session_handoff", "version": "1"}, "capabilities": map[string]bool{"experimentalApi": true}}, nil); err != nil {
		return err
	}
	if err = c.send(map[string]any{"method": "initialized", "params": map[string]any{}}); err != nil {
		return err
	}
	var resumed struct {
		Thread struct {
			ID    string
			Turns []struct{ Items []struct{ Type string } }
		} `json:"thread"`
	}
	if err = c.call("thread/resume", map[string]any{"threadId": id, "cwd": o.Cwd, "config": map[string]any{"model_context_window": 1050000}, "excludeTurns": false}, &resumed); err != nil {
		return fmt.Errorf("destination persistence verification: %w", err)
	}
	if resumed.Thread.ID != id {
		return fmt.Errorf("resumed destination id %q differs from %q", resumed.Thread.ID, id)
	}
	for _, turn := range resumed.Thread.Turns {
		for _, item := range turn.Items {
			if item.Type == "contextCompaction" {
				return nil
			}
		}
	}
	return errors.New("resumed destination has no persisted contextCompaction item")
}
