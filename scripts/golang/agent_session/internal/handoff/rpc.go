// Package handoff migrates a selected Claude transcript through Codex's
// app-server. It never chooses a destination by list ordering or timestamp.
package handoff

import (
	"context"
	"encoding/json"
	"fmt"
	"io"
	"os/exec"
	"sync"
	"time"
)

type message struct {
	ID     json.RawMessage `json:"id,omitempty"`
	Method string          `json:"method,omitempty"`
	Params json.RawMessage `json:"params,omitempty"`
	Result json.RawMessage `json:"result,omitempty"`
	Error  *rpcError       `json:"error,omitempty"`
}
type rpcError struct {
	Code    int    `json:"code"`
	Message string `json:"message"`
}
type received struct {
	msg message
	err error
}
type client struct {
	ctx       context.Context
	cancel    context.CancelFunc
	cmd       *exec.Cmd
	in        io.WriteCloser
	incoming  chan received
	queue     []message
	next      int
	closeOnce sync.Once
}

func start(ctx context.Context, binary string, args []string, stderr io.Writer) (*client, error) {
	ctx, cancel := context.WithCancel(ctx)
	argv := append([]string{"app-server", "--listen", "stdio://"}, args...)
	cmd := exec.CommandContext(ctx, binary, argv...)
	isolateProcess(cmd)
	cmd.Stderr = stderr
	cmd.WaitDelay = 2 * time.Second
	in, err := cmd.StdinPipe()
	if err != nil {
		cancel()
		return nil, err
	}
	out, err := cmd.StdoutPipe()
	if err != nil {
		cancel()
		in.Close()
		return nil, err
	}
	if err = cmd.Start(); err != nil {
		cancel()
		in.Close()
		out.Close()
		return nil, err
	}
	c := &client{ctx: ctx, cancel: cancel, cmd: cmd, in: in, incoming: make(chan received)}
	go func() {
		dec := json.NewDecoder(out)
		for {
			var m message
			err := dec.Decode(&m)
			select {
			case c.incoming <- received{m, err}:
			case <-ctx.Done():
				return
			}
			if err != nil {
				return
			}
		}
	}()
	return c, nil
}
func (c *client) close() { c.closeOnce.Do(func() { c.cancel(); c.in.Close(); _ = c.cmd.Wait() }) }
func (c *client) send(v any) error {
	b, err := json.Marshal(v)
	if err != nil {
		return err
	}
	b = append(b, '\n')
	done := make(chan error, 1)
	go func() { _, err := c.in.Write(b); done <- err }()
	select {
	case err := <-done:
		return err
	case <-c.ctx.Done():
		return c.ctx.Err()
	}
}
func (c *client) receive() (message, error) {
	select {
	case <-c.ctx.Done():
		return message{}, c.ctx.Err()
	case r := <-c.incoming:
		if r.err != nil {
			return message{}, fmt.Errorf("app-server output ended: %w", r.err)
		}
		if r.msg.Method != "" && len(r.msg.ID) > 0 {
			// Never approve tool execution, edits or external elicitation during handoff.
			_ = c.send(map[string]any{"id": r.msg.ID, "error": rpcError{-32601, "session handoff does not accept server requests"}})
			return message{}, fmt.Errorf("unexpected server request %s rejected", r.msg.Method)
		}
		return r.msg, nil
	}
}
func (c *client) call(method string, params any, result any) error {
	c.next++
	id := c.next
	if err := c.send(map[string]any{"id": id, "method": method, "params": params}); err != nil {
		return err
	}
	for {
		m, err := c.receive()
		if err != nil {
			return fmt.Errorf("%s: %w", method, err)
		}
		if m.Method != "" {
			c.queue = append(c.queue, m)
			continue
		}
		if string(m.ID) != fmt.Sprint(id) {
			return fmt.Errorf("%s: unexpected response id %s (want %d)", method, m.ID, id)
		}
		if m.Error != nil {
			return fmt.Errorf("%s: app-server error %d: %s", method, m.Error.Code, m.Error.Message)
		}
		if len(m.Result) == 0 {
			return fmt.Errorf("%s: response has no result", method)
		}
		if result != nil {
			if err := json.Unmarshal(m.Result, result); err != nil {
				return fmt.Errorf("%s result: %w", method, err)
			}
		}
		return nil
	}
}
func (c *client) event() (message, error) {
	if len(c.queue) > 0 {
		m := c.queue[0]
		c.queue = c.queue[1:]
		return m, nil
	}
	m, err := c.receive()
	if err == nil && m.Method == "" {
		err = fmt.Errorf("unexpected response id %s while waiting for notification", m.ID)
	}
	return m, err
}
