package main

import (
	"bufio"
	"bytes"
	"encoding/base64"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net"
	"net/http"
	"net/url"
	"os"
	"strconv"
	"strings"
	"time"
	"unicode/utf8"
)

// Exit statuses that are not the command's own, all as brishzq.zsh has them.
const (
	exitNotice    = 200 // a reply that is not a command's result
	exitNoBinary  = 201 // brishz_binary=y, and the garden lacks binary mode
	exitHTTPError = 22  // curl --fail on an HTTP status of 400 or more
)

const noBinaryMessage = "brishzgo: garden lacks binary support (no X-Brish-Binary header); it predates binary mode or runs with BRISH_BINARY=0"

// maxRedirects is curl's default for --location.
const maxRedirects = 50

// expectContinueTimeout is how long a raw request that streams stdin waits
// for the garden's 100 Continue before it sends the body anyway.
var expectContinueTimeout = 2 * time.Second

type client struct {
	cfg            config
	hc             *http.Client
	stdout, stderr io.Writer
	keyHeaders     [][2]string // never printed
	fallbackWhy    string      // why raw fell back, for debug output
}

func newClient(cfg config, stdout, stderr io.Writer) *client {
	tr := &http.Transport{
		Proxy: proxyFromEnv(cfg.env),
		// curl's default connect timeout; there is no overall one.
		DialContext:           (&net.Dialer{Timeout: 300 * time.Second}).DialContext,
		ExpectContinueTimeout: expectContinueTimeout,
		ForceAttemptHTTP2:     true,
		TLSHandshakeTimeout:   300 * time.Second,
	}
	hc := &http.Client{
		Transport: tr,
		CheckRedirect: func(req *http.Request, via []*http.Request) error {
			if len(via) > maxRedirects {
				return errTooManyRedirects
			}
			return nil
		},
	}
	c := &client{cfg: cfg, hc: hc, stdout: stdout, stderr: stderr}
	if cfg.apikeyFile != "" {
		c.keyHeaders = readHeaderFile(cfg.apikeyFile)
	}
	return c
}

// readHeaderFile reads a header file the way curl's `--header @file` does:
// one `Name: value` per line. A line without a colon is skipped.
func readHeaderFile(path string) [][2]string {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil
	}
	var out [][2]string
	for _, line := range strings.Split(string(data), "\n") {
		line = strings.TrimRight(line, "\r")
		name, value, ok := strings.Cut(line, ":")
		name = strings.TrimSpace(name)
		if !ok || name == "" {
			continue
		}
		out = append(out, [2]string{name, strings.TrimLeft(value, " \t")})
	}
	return out
}

func (c *client) debugf(format string, a ...any) {
	if c.cfg.debug {
		fmt.Fprintf(c.stderr, "brishzgo: "+format+"\n", a...)
	}
}

// endpointURL is the endpoint plus `sub`, with curl's guess of http:// for
// a URL without a scheme.
func endpointURL(endpoint, sub string) string {
	u := endpoint + sub
	if !strings.Contains(u, "://") {
		u = "http://" + u
	}
	return u
}

func (c *client) newRequest(u string, body io.Reader, length int64, contentType string) (*http.Request, error) {
	req, err := http.NewRequest(http.MethodPost, u, body)
	if err != nil {
		return nil, err
	}
	req.ContentLength = length
	req.Header.Set("Content-Type", contentType)
	req.Header.Set("User-Agent", "brishzgo")
	for _, h := range c.keyHeaders {
		req.Header.Add(h[0], h[1])
	}
	if c.cfg.basicAuth {
		req.SetBasicAuth("Alice", c.cfg.basicPass)
	}
	if c.cfg.debug {
		c.debugf("POST %s", u)
		for name, values := range req.Header {
			v := strings.Join(values, ", ")
			if c.secretHeader(name) {
				v = "<redacted>"
			}
			c.debugf("> %s: %s", name, v)
		}
	}
	return req, nil
}

func (c *client) secretHeader(name string) bool {
	if strings.EqualFold(name, "Authorization") {
		return true
	}
	for _, h := range c.keyHeaders {
		if strings.EqualFold(h[0], name) {
			return true
		}
	}
	return false
}

func (c *client) debugResponse(resp *http.Response) {
	if !c.cfg.debug {
		return
	}
	c.debugf("< %s", resp.Status)
	for name, values := range resp.Header {
		c.debugf("< %s: %s", name, strings.Join(values, ", "))
	}
}

// do sends the request. On a transport failure it returns the exit status
// curl would have.
func (c *client) do(req *http.Request) (*http.Response, int) {
	resp, err := c.hc.Do(req)
	if err != nil {
		c.debugf("request failed: %v", err)
		return nil, curlExitCode(err, false)
	}
	c.debugResponse(resp)
	return resp, 0
}

// printNotice prints a reply that is not a command's result as brishzq.zsh
// does, with `ec "$out"`: its trailing newlines replaced by one.
func (c *client) printNotice(body []byte) int {
	c.stdout.Write(append(bytes.TrimRight(body, "\n"), '\n'))
	return exitNotice
}

// raw runs the command through POST /zsh/raw/. fallback is true when the
// garden has no raw API (HTTP 404 or 405) or refused the request
// (X-Brish-Refused: 1); then nothing ran.
func (c *client) raw(in *stdinSource) (code int, fallback bool) {
	sub := "raw/"
	if c.cfg.nolog != "" {
		sub += "nolog/"
	}
	q := url.Values{}
	if c.cfg.session != "" {
		q.Set("session", c.cfg.session)
	}
	// The JSON API takes any non-empty string here as true, and the raw
	// API reads n, no, 0 and false as false; so 1, as brishzq.zsh sends.
	for _, kv := range [][2]string{
		{"nolog", c.cfg.nolog},
		{"failure_expected", c.cfg.failureExpected},
	} {
		if kv[1] != "" {
			q.Set(kv[0], "1")
		}
	}
	u := endpointURL(c.cfg.endpoint, sub)
	if len(q) > 0 {
		u += "?" + q.Encode()
	}

	cmd := c.cfg.command
	var body io.Reader
	length := int64(-1)
	if in.magic {
		body = io.MultiReader(bytes.NewReader(cmd), in)
	} else {
		all := make([]byte, 0, len(cmd)+len(in.literal))
		all = append(append(all, cmd...), in.literal...)
		body = bytes.NewReader(all)
		length = int64(len(all))
	}
	req, err := c.newRequest(u, body, length, "application/octet-stream")
	if err != nil {
		return curlExitCode(err, false), false
	}
	req.Header.Set("X-Brish-Cmd-Length", strconv.Itoa(len(cmd)))
	if in.magic {
		// So that a garden without the raw API answers before we send
		// any of stdin, which the fallback then still has; see replayLimit.
		// Go's transport sends the body after a final reply such as that
		// 404 unless the connection is to close, so it is.
		req.Header.Set("Expect", "100-continue")
		req.Close = true
	}
	c.debugf("command (%d bytes): %q", len(cmd), cmd)

	resp, code := c.do(req)
	if resp == nil {
		return code, false
	}
	defer resp.Body.Close()

	switch {
	case resp.StatusCode == http.StatusNotFound || resp.StatusCode == http.StatusMethodNotAllowed:
		c.fallbackWhy = fmt.Sprintf("no raw API (HTTP %d)", resp.StatusCode)
		return 0, true
	case resp.StatusCode >= 400:
		return exitHTTPError, false
	}

	h := resp.Header
	if strings.TrimSpace(h.Get("X-Brish-Refused")) == "1" {
		// The garden ran nothing: a legacy-mode garden refuses a command
		// or stdin that is not valid UTF-8 or holds a NUL, which the JSON
		// API's request carries (stdin in a temp file), and any garden
		// refuses a malformed request. Only a refusal has this header, so
		// a command that ran and returned 9000 is never run again.
		msg, _ := io.ReadAll(io.LimitReader(resp.Body, 4096))
		c.fallbackWhy = fmt.Sprintf("the raw request was refused (%q)", msg)
		return 0, true
	}

	// X-Brish-Binary: 0 means a legacy-mode garden, whose bytes went
	// through text; the reply is printed all the same, as brishzq.zsh's
	// text path does. brishz_binary=y never comes here; see run.
	retcode, errRet := strconv.Atoi(strings.TrimSpace(h.Get("X-Brish-Retcode")))
	outLen, errLen := strconv.ParseInt(strings.TrimSpace(h.Get("X-Brish-Out-Length")), 10, 64)
	if h.Get("X-Brish-Notice") == "1" || errRet != nil || errLen != nil || outLen < 0 {
		// A notice (a magic command's log), or not a raw reply at all.
		data, err := io.ReadAll(resp.Body)
		if err != nil {
			return curlExitCode(err, true), false
		}
		return c.printNotice(data), false
	}

	// Stream the two parts, so a large reply is never held in memory.
	br := bufio.NewReaderSize(resp.Body, 64<<10)
	n, err := io.CopyN(c.stdout, br, outLen)
	if err != nil {
		if n < outLen && (err == io.EOF || errors.Is(err, io.ErrUnexpectedEOF)) {
			return curlPartial, false
		}
		return curlExitCode(err, true), false
	}
	if _, err := io.Copy(c.stderr, br); err != nil {
		return curlExitCode(err, true), false
	}
	return retcode, false
}

// jsonField is one field of a JSON request, in brishzq.zsh's order.
type jsonField struct {
	name  string
	value any
}

func encodeObject(fields []jsonField) []byte {
	var b bytes.Buffer
	b.WriteByte('{')
	for i, f := range fields {
		if i > 0 {
			b.WriteByte(',')
		}
		enc := json.NewEncoder(&b)
		enc.SetEscapeHTML(false)
		enc.Encode(f.name)
		b.Truncate(b.Len() - 1) // Encode's newline
		b.WriteByte(':')
		enc.Encode(f.value)
		b.Truncate(b.Len() - 1)
	}
	b.WriteByte('}')
	return b.Bytes()
}

// jsonBody is the shape of a JSON request's body.
type jsonBody int

const (
	// cmd and stdin as strings: brishzq.zsh's request for a literal
	// brishz_in.
	jsonText jsonBody = iota
	// cmd alone, which reads stdin from a temp file: brishzq.zsh's
	// request for MAGIC_READ_STDIN.
	jsonStdinFile
	// cmd_b64 and stdin_b64, for MAGIC_READ_STDIN to a garden that cannot
	// read our temp files, when stdin is not valid UTF-8.
	jsonB64
	// brishz_binary: cmd_b64, stdin_b64, binary: 1 and b64_only: 1.
	jsonBinary
)

// jsonRequest is the body of a JSON request, with the fields in
// brishzq.zsh's order. Strings hold the bytes as jq makes a string of them
// (see jqText), since that is how brishzq.zsh builds them.
func jsonRequest(cfg config, kind jsonBody, cmd, stdin []byte) []byte {
	tail := []jsonField{
		{"session", cfg.session},
		{"json_output", "1"},
		{"nolog", cfg.nolog},
		{"failure_expected", cfg.failureExpected},
	}
	b64 := base64.StdEncoding.EncodeToString
	var fields []jsonField
	switch kind {
	case jsonBinary:
		fields = []jsonField{
			{"cmd_b64", b64(cmd)},
			{"stdin_b64", b64(stdin)},
			{"binary", 1},
			{"b64_only", 1},
		}
	case jsonB64:
		fields = []jsonField{
			{"cmd_b64", b64(cmd)},
			{"stdin_b64", b64(stdin)},
		}
	case jsonStdinFile:
		fields = []jsonField{{"cmd", jqText(cmd)}}
	default:
		fields = []jsonField{
			{"cmd", jqText(cmd)},
			{"session", cfg.session},
			{"stdin", jqText(stdin)},
		}
		tail = tail[1:]
	}
	return encodeObject(append(fields, tail...))
}

// stdinRedirect is brishzq.zsh's command for stdin in a file: the command
// in a brace group that reads it.
func stdinRedirect(path string, cmd []byte) []byte {
	return []byte("< " + quoteSingle(path) + " {\n" + string(cmd) + "\n}")
}

// cmdResult is the part of a JSON reply the client reads.
type cmdResult struct {
	Out     *string      `json:"out"`
	Err     *string      `json:"err"`
	OutB64  *string      `json:"out_b64"`
	ErrB64  *string      `json:"err_b64"`
	Retcode *json.Number `json:"retcode"`
}

// parseJSONReply returns the command's stdout, stderr and status from a
// JSON reply, or ok false when the reply is not a command's result.
func parseJSONReply(data []byte, binary bool) (out, errOut []byte, retcode int, ok bool) {
	var r cmdResult
	dec := json.NewDecoder(bytes.NewReader(data))
	dec.UseNumber()
	if err := dec.Decode(&r); err != nil || r.Retcode == nil {
		return nil, nil, 0, false
	}
	f, err := r.Retcode.Float64()
	if err != nil {
		return nil, nil, 0, false
	}
	retcode = int(f)
	if binary {
		if r.OutB64 == nil || r.ErrB64 == nil {
			return nil, nil, 0, false
		}
		out, err1 := base64.StdEncoding.DecodeString(*r.OutB64)
		errOut, err2 := base64.StdEncoding.DecodeString(*r.ErrB64)
		if err1 != nil || err2 != nil {
			return nil, nil, 0, false
		}
		return out, errOut, retcode, true
	}
	if r.Out == nil || r.Err == nil {
		return nil, nil, 0, false
	}
	return []byte(*r.Out), []byte(*r.Err), retcode, true
}

// json runs the command through the JSON API, POST /zsh/, with the request
// brishzq.zsh sends.
func (c *client) json(in *stdinSource) int {
	cmd, stdin := c.cfg.command, []byte(nil)
	kind := jsonText
	var err error
	switch {
	case c.cfg.binary:
		kind = jsonBinary
		stdin, err = in.all()
	case in.magic && c.cfg.sameMachine:
		// Stdin goes in a temp file that the command reads, as brishzq.zsh
		// sends it, so it arrives exact whatever the garden's version and
		// mode, NUL and invalid UTF-8 included.
		var path string
		var size int64
		if path, size, err = in.toFile(); err == nil {
			kind = jsonStdinFile
			cmd = stdinRedirect(path, cmd)
			read, _ := in.alreadyRead()
			c.debugf("stdin: %d bytes, %d of them already read by the raw request; in %s", size, read, path)
		}
	default:
		stdin, err = in.all()
		if in.magic && err == nil {
			// brishzq.zsh sends a temp file here too, which a garden on
			// another machine cannot read; so stdin goes in the request.
			if !utf8.Valid(stdin) {
				// JSON strings carry text only. With cmd_b64 along, a
				// garden that predates these fields runs nothing.
				kind = jsonB64
			}
			read, _ := in.alreadyRead()
			c.debugf("stdin: %d bytes, %d of them already read by the raw request", len(stdin), read)
		}
	}
	if err != nil {
		fmt.Fprintf(c.stderr, "brishzgo: %v; nothing ran\n", err)
		return 1
	}
	sub := ""
	if c.cfg.nolog != "" {
		sub = "nolog/"
	}
	body := jsonRequest(c.cfg, kind, cmd, stdin)
	req, err := c.newRequest(endpointURL(c.cfg.endpoint, sub), bytes.NewReader(body), int64(len(body)), "application/json")
	if err != nil {
		return curlExitCode(err, false)
	}
	c.debugf("command (%d bytes): %q", len(cmd), cmd)

	resp, code := c.do(req)
	if resp == nil {
		return code
	}
	defer resp.Body.Close()
	if resp.StatusCode >= 400 {
		return exitHTTPError
	}
	data, err := io.ReadAll(resp.Body)
	if err != nil {
		return curlExitCode(err, true)
	}
	if c.cfg.binary && strings.TrimSpace(resp.Header.Get("X-Brish-Binary")) != "1" {
		fmt.Fprintln(c.stderr, noBinaryMessage)
		return exitNoBinary
	}
	out, errOut, retcode, ok := parseJSONReply(data, c.cfg.binary)
	if !ok {
		return c.printNotice(data)
	}
	c.stdout.Write(out)
	c.stderr.Write(errOut)
	return retcode
}
