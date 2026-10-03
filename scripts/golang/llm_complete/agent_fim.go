package main

import (
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strconv"
	"syscall"
	"time"
)

type AgentContextRequest struct {
	Screen    Screen `json:"screen"`
	LastReply string `json:"last_reply"`
	Source    string `json:"source"`
	Target    string `json:"target"`
	Provider  string `json:"provider,omitempty"`
	Parameters
}

func budget(value *int, defaultValue int) (int, error) {
	if value == nil {
		return defaultValue, nil
	}
	if *value < 0 {
		return 0, errors.New("negative agent context budget")
	}
	return *value, nil
}
func agentContext(c Config, a AgentContextRequest) (FIMRequest, error) {
	r := FIMRequest{Provider: a.Provider, Source: a.Source, Target: a.Target, Parameters: a.Parameters}
	in, err := extract(a.Screen)
	if err != nil {
		return r, err
	}
	p, err := budget(c.AgentFIM.PrefixChars, 2000)
	if err != nil {
		return r, err
	}
	s, err := budget(c.AgentFIM.SuffixChars, 1000)
	if err != nil {
		return r, err
	}
	reply, err := budget(c.AgentFIM.ReplyChars, 1500)
	if err != nil {
		return r, err
	}
	context := a.LastReply
	if context == "" {
		context = in.Above
	}
	context = lastChars(context, reply)
	r.Prefix = lastChars(in.Prefix, p)
	r.Suffix = firstChars(in.Suffix, s)
	if context != "" {
		r.Prefix = "Previous assistant reply:\n" + context + "\n\nCurrent prompt:\n" + r.Prefix
	}
	return r, nil
}

// Lock only the brief snapshot/insertion transaction. Network waits never hold it.
func targetLock(r TerminalRequest) (func(), error) {
	if err := privateDir(stateDir()); err != nil {
		return nil, err
	}
	f, err := os.OpenFile(targetFile(r)+".lock", os.O_CREATE|os.O_RDWR, 0600)
	if err != nil {
		return nil, err
	}
	if err = syscall.Flock(int(f.Fd()), syscall.LOCK_EX|syscall.LOCK_NB); err != nil {
		f.Close()
		return nil, errors.New("completion insertion already in progress")
	}
	return func() { syscall.Flock(int(f.Fd()), syscall.LOCK_UN); f.Close() }, nil
}
func ticket(r TerminalRequest) string { return targetFile(r) + ".request" }
func terminalFIM(r TerminalRequest) error {
	c, err := readConfig()
	if err != nil {
		return err
	}
	// Reject vim commands and dialogs before any request or transcript lookup.
	if _, err = extract(r.Screen); err != nil {
		return err
	}
	unlock, err := targetLock(r)
	if err != nil {
		return err
	}
	token := strconv.FormatInt(time.Now().UnixNano(), 10) + "-" + strconv.Itoa(os.Getpid())
	err = writePrivate(ticket(r), token)
	unlock()
	if err != nil {
		return err
	}
	context := loadContext(r)
	req, err := agentContext(c, AgentContextRequest{Screen: r.Screen, LastReply: context.LastReply, Source: r.Source, Target: r.Target})
	if err != nil {
		return err
	}
	_, _, timeout, _, err := resolve(c, req)
	if err != nil {
		return err
	}
	data, _ := json.Marshal(req)
	text, err := child(timeout+5*time.Second, string(data), filepath.Join(rootDir(), "bin/llm-complete.zsh"), "fim")
	if err != nil {
		return err
	}
	text = sanitise(text)
	if text == "" {
		return errors.New("empty completion")
	}
	unlock, err = targetLock(r)
	if err != nil {
		return err
	}
	defer unlock()
	var currentToken string
	b, err := os.ReadFile(ticket(r))
	if err != nil || json.Unmarshal(b, &currentToken) != nil || token != currentToken {
		return errors.New("superseded FIM request, discarded completion")
	}
	current, err := capture(r)
	if err != nil {
		return err
	}
	if !sameInput(r.Screen, current) {
		return errors.New("prompt changed, discarded completion")
	}
	if err = insert(r, text, 0); err != nil {
		return err
	}
	_ = os.Remove(targetFile(r))
	return nil
}
func staleTicket(r TerminalRequest) error {
	// A dabbrev transaction supersedes an earlier FIM even if the input later
	// returns to the same bytes. Never let an old response win that ABA race.
	return writePrivate(ticket(r), fmt.Sprintf("dabbrev-%d", time.Now().UnixNano()))
}
