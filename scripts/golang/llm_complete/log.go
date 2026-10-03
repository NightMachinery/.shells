package main

import (
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"syscall"
	"time"
)

func loggingEnabled(c Config, r FIMRequest) bool {
	if r.Log != nil {
		return *r.Log
	}
	if value, ok := c.Logging.Sources[r.Source]; ok {
		return value
	}
	if r.Source == "kitty" || r.Source == "tmux" {
		if c.AgentFIM.Log != nil {
			return *c.AgentFIM.Log
		}
		return true
	}
	return false
}
func logDir() string {
	if s := os.Getenv("LLM_COMPLETE_LOG_DIR"); s != "" {
		return s
	}
	h, _ := os.UserHomeDir()
	return filepath.Join(h, "logs/llm_complete")
}
func writeFIMLog(c Config, r FIMRequest, body map[string]any, started time.Time, result, msg string, code int) error {
	if !loggingEnabled(c, r) {
		return nil
	}
	limit, files := c.Logging.MaxBytes, c.Logging.Files
	if limit == 0 {
		limit = 1 << 20
	}
	if files == 0 {
		files = 4
	}
	if limit < 1024 || files < 1 || files > 100 {
		return errors.New("invalid logging limits")
	}
	params := map[string]any{}
	for k, v := range body {
		if k != "prompt" && k != "suffix" {
			params[k] = v
		}
	}
	_, _, timeout, strip, _ := resolve(c, r)
	params["timeout_seconds"] = timeout.Seconds()
	params["strip_space"] = strip
	b, _ := json.MarshalIndent(params, "", "  ")
	provider := r.Provider
	if provider == "" {
		provider = c.DefaultProvider
	}
	record := fmt.Sprintf("Time: %s\nSource: %s\nTarget: %s\nProvider: %s\nParameters:\n%s\nPrefix (%d bytes), cursor, suffix (%d bytes), verbatim:\n%s⟦CURSOR⟧%s\nDuration: %s\nOutcome: exit %d %s\nCompletion (%d bytes), verbatim:\n%s\n\n", started.UTC().Format(time.RFC3339Nano), sanitise(r.Source), sanitise(r.Target), sanitise(provider), b, len(r.Prefix), len(r.Suffix), r.Prefix, r.Suffix, time.Since(started), code, msg, len(result), result)
	// API keys and headers have no place in this log, even if a server echoes one.
	if key := os.Getenv(c.Providers[provider].KeyVar); key != "" {
		record = strings.ReplaceAll(record, key, "[redacted API key]")
	}
	if int64(len(record)) > limit {
		return errors.New("request exceeds log size limit")
	}
	dir := logDir()
	if err := privateDir(dir); err != nil {
		return err
	}
	lock, err := os.OpenFile(filepath.Join(dir, ".lock"), os.O_CREATE|os.O_RDWR, 0600)
	if err != nil {
		return err
	}
	defer lock.Close()
	if err = syscall.Flock(int(lock.Fd()), syscall.LOCK_EX); err != nil {
		return err
	}
	defer syscall.Flock(int(lock.Fd()), syscall.LOCK_UN)
	name := filepath.Join(dir, "completion.log")
	if st, err := os.Lstat(name); err == nil {
		if !st.Mode().IsRegular() {
			return errors.New("log is not a regular file")
		}
		if st.Size()+int64(len(record)) > limit {
			for i := files - 1; i >= 1; i-- {
				old := name
				if i > 1 {
					old = fmt.Sprintf("%s.%d", name, i-1)
				}
				dest := fmt.Sprintf("%s.%d", name, i)
				if err := os.Rename(old, dest); err != nil && !os.IsNotExist(err) {
					return err
				}
			}
			if files == 1 {
				if err := os.Remove(name); err != nil {
					return err
				}
			}
		}
	} else if !os.IsNotExist(err) {
		return err
	}
	f, err := os.OpenFile(name, os.O_CREATE|os.O_APPEND|os.O_WRONLY, 0600)
	if err != nil {
		return err
	}
	defer f.Close()
	if err = f.Chmod(0600); err != nil {
		return err
	}
	_, err = f.WriteString(record)
	return err
}
