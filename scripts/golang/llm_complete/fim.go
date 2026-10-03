package main

import (
	"bytes"
	"context"
	"crypto/x509"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net"
	"net/http"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"time"
)

type Parameters struct {
	Model       *string         `json:"model,omitempty"`
	MaxTokens   *int            `json:"max_tokens,omitempty"`
	Stop        json.RawMessage `json:"stop,omitempty"`
	Temperature *float64        `json:"temperature,omitempty"`
	Timeout     *float64        `json:"timeout,omitempty"`
	StripSpace  *bool           `json:"strip_space,omitempty"`
}
type Provider struct {
	Name     string `json:"name"`
	Endpoint string `json:"endpoint"`
	KeyVar   string `json:"key_env"`
	Extract  string `json:"extract"`
	Parameters
}
type AgentConfig struct {
	PrefixChars *int  `json:"prefix_chars"`
	SuffixChars *int  `json:"suffix_chars"`
	ReplyChars  *int  `json:"reply_chars"`
	Log         *bool `json:"log"`
}
type LogConfig struct {
	Sources  map[string]bool `json:"sources"`
	MaxBytes int64           `json:"max_bytes"`
	Files    int             `json:"files"`
}
type Config struct {
	Providers       map[string]Provider `json:"providers"`
	DefaultProvider string              `json:"default_provider"`
	AgentFIM        AgentConfig         `json:"agent_fim"`
	Logging         LogConfig           `json:"logging"`
}
type FIMRequest struct {
	Provider string `json:"provider"`
	Prefix   string `json:"prefix"`
	Suffix   string `json:"suffix"`
	Source   string `json:"source"`
	Target   string `json:"target,omitempty"`
	Log      *bool  `json:"log,omitempty"`
	Parameters
}

func ptr[T any](v T) *T { return &v }
func configPath() string {
	if p := os.Getenv("LLM_COMPLETE_CONFIG"); p != "" {
		return p
	}
	h, _ := os.UserHomeDir()
	return filepath.Join(h, ".config/llm_complete/providers.json")
}
func readConfig() (Config, error) {
	c := Config{DefaultProvider: "codestral", Providers: map[string]Provider{}}
	for _, p := range []Provider{
		{Name: "codestral", Endpoint: "https://codestral.mistral.ai/v1/fim/completions", KeyVar: "codestral_api_key", Extract: "chat", Parameters: Parameters{Model: ptr("codestral-latest")}},
		{Name: "deepseek", Endpoint: "https://api.deepseek.com/beta/completions", KeyVar: "deepseek_api_key", Extract: "text", Parameters: Parameters{Model: ptr("deepseek-v4-pro")}},
		{Name: "deepseek-flash", Endpoint: "https://api.deepseek.com/beta/completions", KeyVar: "deepseek_api_key", Extract: "text", Parameters: Parameters{Model: ptr("deepseek-v4-flash")}},
	} {
		c.Providers[p.Name] = p
	}
	b, err := os.ReadFile(configPath())
	if os.IsNotExist(err) {
		return c, nil
	}
	if err != nil {
		return c, errors.New("cannot read providers.json")
	}
	// Decode provider entries into their existing values, retaining omitted fields.
	var raw struct {
		Providers       map[string]json.RawMessage `json:"providers"`
		DefaultProvider string                     `json:"default_provider"`
		AgentFIM        AgentConfig                `json:"agent_fim"`
		Logging         LogConfig                  `json:"logging"`
	}
	if json.Unmarshal(b, &raw) != nil {
		return c, errors.New("invalid providers.json")
	}
	for name, entry := range raw.Providers {
		p := c.Providers[name]
		if json.Unmarshal(entry, &p) != nil {
			return c, errors.New("invalid provider entry")
		}
		p.Name = name
		c.Providers[name] = p
	}
	if raw.DefaultProvider != "" {
		c.DefaultProvider = raw.DefaultProvider
	}
	c.AgentFIM = raw.AgentFIM
	c.Logging = raw.Logging
	for _, p := range c.Providers {
		if p.Endpoint == "" || p.Model == nil || *p.Model == "" || (p.Extract != "text" && p.Extract != "chat") {
			return c, fmt.Errorf("invalid provider '%s'", sanitise(p.Name))
		}
	}
	return c, nil
}
func providerNames(c Config) []string {
	out := []string{}
	for name := range c.Providers {
		out = append(out, name)
	}
	sort.Strings(out)
	return out
}
func resolve(c Config, r FIMRequest) (Provider, map[string]any, time.Duration, bool, error) {
	name := r.Provider
	if name == "" {
		name = c.DefaultProvider
	}
	p, ok := c.Providers[name]
	if !ok {
		return p, nil, 0, false, fmt.Errorf("unknown provider '%s'; known: %s", sanitise(name), strings.Join(providerNames(c), " "))
	}
	params := p.Parameters
	if r.Model != nil {
		params.Model = r.Model
	}
	if r.MaxTokens != nil {
		params.MaxTokens = r.MaxTokens
	}
	if r.Stop != nil {
		params.Stop = r.Stop
	}
	if r.Temperature != nil {
		params.Temperature = r.Temperature
	}
	if r.Timeout != nil {
		params.Timeout = r.Timeout
	}
	if r.StripSpace != nil {
		params.StripSpace = r.StripSpace
	}
	max := 64
	if params.MaxTokens != nil {
		max = *params.MaxTokens
	}
	temp := 0.0
	if params.Temperature != nil {
		temp = *params.Temperature
	}
	timeout := 20.0
	if params.Timeout != nil {
		timeout = *params.Timeout
	}
	strip := params.StripSpace != nil && *params.StripSpace
	if max < 0 || timeout <= 0 || timeout > 86400 {
		return p, nil, 0, false, errors.New("invalid max_tokens or timeout")
	}
	body := map[string]any{"model": *params.Model, "prompt": r.Prefix, "temperature": temp}
	if r.Suffix != "" {
		body["suffix"] = r.Suffix
	}
	if max != 0 {
		body["max_tokens"] = max
	}
	var stop any = "\n"
	if params.Stop != nil {
		if json.Unmarshal(params.Stop, &stop) != nil {
			return p, nil, 0, false, errors.New("invalid stop sequence")
		}
	}
	switch v := stop.(type) {
	case nil:
	case string:
		if v != "" {
			body["stop"] = v
		}
	case []any:
		for _, x := range v {
			if _, ok := x.(string); !ok {
				return p, nil, 0, false, errors.New("invalid stop sequence")
			}
		}
		if len(v) > 0 {
			body["stop"] = v
		}
	default:
		return p, nil, 0, false, errors.New("invalid stop sequence")
	}
	return p, body, time.Duration(timeout * float64(time.Second)), strip, nil
}
func apiMessage(body []byte) string {
	msg := string(body)
	var obj map[string]any
	if json.Unmarshal(body, &obj) == nil {
		value := obj["detail"]
		if value == nil || value == false {
			value = obj["message"]
		}
		if value == nil || value == false {
			if e, ok := obj["error"].(map[string]any); ok {
				value = e["message"]
			}
		}
		if value != nil && value != false {
			if s, ok := value.(string); ok {
				if s != "" {
					msg = s
				}
			} else if b, err := json.Marshal(value); err == nil {
				msg = string(b)
			}
		}
	}
	msg = strings.TrimSpace(strings.NewReplacer("\n", " ", "\r", " ", "\t", " ").Replace(msg))
	msg = sanitise(msg)
	if len([]rune(msg)) > 200 {
		msg = firstChars(msg, 200) + "…"
	}
	if msg == "" {
		msg = "(empty response body)"
	}
	return msg
}
func networkCode(err error) int {
	var ne net.Error
	var dns *net.DNSError
	var cert x509.UnknownAuthorityError
	if errors.Is(err, context.DeadlineExceeded) || errors.As(err, &ne) && ne.Timeout() {
		return 28
	}
	if errors.As(err, &dns) {
		return 6
	}
	if errors.As(err, &cert) {
		return 60
	}
	var op *net.OpError
	if errors.As(err, &op) {
		return 7
	}
	return 52
}
func performFIM(c Config, r FIMRequest) (string, int, string, map[string]any) {
	p, body, timeout, strip, err := resolve(c, r)
	if err != nil {
		return "", 1, "fim-get: " + err.Error(), nil
	}
	key := os.Getenv(p.KeyVar)
	if p.KeyVar != "" && key == "" {
		return "", 1, fmt.Sprintf("fim-get: no API key for %s (expected $%s)", p.Name, p.KeyVar), body
	}
	if r.Prefix == "" && r.Suffix == "" {
		return "", 1, "fim-get: needs a prefix, a suffix, or both", body
	}
	data, err := json.Marshal(body)
	if err != nil {
		return "", 1, "fim-get: invalid request parameters", body
	}
	ctx, cancel := context.WithTimeout(context.Background(), timeout)
	defer cancel()
	req, err := http.NewRequestWithContext(ctx, http.MethodPost, p.Endpoint, bytes.NewReader(data))
	if err != nil {
		return "", 1, "fim-get: invalid provider endpoint", body
	}
	req.Header.Set("Content-Type", "application/json")
	req.Header.Set("Accept", "application/json")
	if key != "" {
		req.Header.Set("Authorization", "Bearer "+key)
	}
	// ProxyFromEnvironment honors HTTP(S)_PROXY and NO_PROXY, intentionally not ALL_PROXY.
	tr := http.DefaultTransport.(*http.Transport).Clone()
	defer tr.CloseIdleConnections()
	client := &http.Client{Transport: tr}
	res, err := client.Do(req)
	if err != nil {
		code := networkCode(err)
		return "", code, fmt.Sprintf("fim-get: %s: curl error %d", p.Name, code), body
	}
	defer res.Body.Close()
	b, err := io.ReadAll(io.LimitReader(res.Body, 8<<20))
	if err != nil {
		code := networkCode(err)
		return "", code, fmt.Sprintf("fim-get: %s: curl error %d", p.Name, code), body
	}
	if res.StatusCode >= 400 {
		msg := apiMessage(b)
		if key != "" {
			msg = strings.ReplaceAll(msg, key, "[redacted]")
		}
		return "", 1, fmt.Sprintf("fim-get: %s: HTTP %d — %s", p.Name, res.StatusCode, msg), body
	}
	var result struct {
		Choices []struct {
			Text    string `json:"text"`
			Message struct {
				Content string `json:"content"`
			} `json:"message"`
		} `json:"choices"`
	}
	if json.Unmarshal(b, &result) != nil {
		return "", 1, fmt.Sprintf("fim-get: %s: unreadable response", p.Name), body
	}
	out := ""
	if len(result.Choices) > 0 {
		if p.Extract == "chat" {
			out = result.Choices[0].Message.Content
		} else {
			out = result.Choices[0].Text
		}
	}
	if strip {
		out = strings.TrimPrefix(out, " ")
	}
	return out, 0, "", body
}
func runFIM(args []string, in io.Reader, out, errs io.Writer) int {
	c, err := readConfig()
	if err != nil {
		fmt.Fprintln(errs, "fim-get:", err)
		return 1
	}
	if len(args) > 0 && args[0] == "providers" {
		names := providerNames(c)
		if len(args) > 1 && args[1] == "--json" {
			ps := []Provider{}
			for _, n := range names {
				ps = append(ps, c.Providers[n])
			}
			json.NewEncoder(out).Encode(ps)
		} else {
			for _, n := range names {
				fmt.Fprintln(out, n)
			}
		}
		return 0
	}
	var r FIMRequest
	if decode(in, &r) != nil {
		fmt.Fprintln(errs, "fim-get: invalid JSON request")
		return 1
	}
	result, code, msg, _ := performFIM(c, r)
	if code != 0 {
		fmt.Fprintln(errs, msg)
		return code
	}
	fmt.Fprint(out, result)
	if f, ok := out.(*os.File); ok {
		if st, err := f.Stat(); err == nil && st.Mode()&os.ModeCharDevice != 0 {
			fmt.Fprintln(out)
		}
	}
	return 0
}
