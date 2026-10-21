package paper

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net/http"
	"net/url"
	"os"
	"os/exec"
	"strconv"
	"strings"
	"syscall"
	"time"
)

type Options struct {
	Scraper                       string
	APIEnabled                    bool
	FetchBibTeX                   bool
	BibTeXSource                  string
	BibTeXBrowser                 bool
	EnrichDBLP                    bool
	APIKey, DeepSeekKey, HTMLMode string
}

func Enabled(s string) bool {
	switch strings.ToLower(strings.TrimSpace(s)) {
	case "", "n", "no", "0", "false":
		return false
	}
	return true
}
func EnvOptions() Options {
	o := Options{Scraper: os.Getenv("semantic_scholar_scraper"), APIKey: os.Getenv("SemanticScholar_API_key"), DeepSeekKey: os.Getenv("deepseek_api_key"), HTMLMode: os.Getenv("fhMode"), BibTeXSource: os.Getenv("semantic_scholar_bibtex_source"), BibTeXBrowser: Enabled(os.Getenv("semantic_scholar_bibtex_browser"))}
	o.EnrichDBLP = os.Getenv("semantic_scholar_dblp_metadata") == "" || Enabled(os.Getenv("semantic_scholar_dblp_metadata"))
	if o.BibTeXSource == "" {
		o.BibTeXSource = "auto"
	}
	if o.Scraper == "" {
		o.Scraper = "auto"
	}
	o.APIEnabled = o.APIKey != ""
	if v := os.Getenv("semantic_scholar_api_p"); v != "" {
		o.APIEnabled = Enabled(v)
	}
	return o
}
func (o Options) Validate() error {
	switch o.Scraper {
	case "", "auto", "page-data", "api", "deepseek", "full-html2":
	default:
		return fmt.Errorf("scraper must be auto, page-data, api, deepseek, or full-html2")
	}
	switch o.BibTeXSource {
	case "", "auto", "dblp", "doi", "semantic-scholar":
		return nil
	}
	return fmt.Errorf("bibtex-source must be auto, dblp, doi, or semantic-scholar")
}

type Client struct {
	HTTP     *http.Client
	Options  Options
	Warnf    func(string, ...any)
	Now      func() time.Time
	LoadHTML func(context.Context, string) ([]byte, error)
}

func NewClient(o Options) *Client {
	c := &Client{HTTP: &http.Client{Timeout: 90 * time.Second}, Options: o, Now: time.Now}
	c.HTTP.CheckRedirect = func(req *http.Request, via []*http.Request) error {
		if len(via) >= 10 {
			return errors.New("too many redirects")
		}
		if len(via) > 0 && (req.URL.Host != via[0].URL.Host || req.URL.Scheme != "https") &&
			(via[0].Header.Get("Authorization") != "" || via[0].Header.Get("x-api-key") != "") {
			return errors.New("refusing to redirect API credentials to a different host")
		}
		return nil
	}
	c.LoadHTML = func(ctx context.Context, url string) ([]byte, error) {
		// Shell source retrieval is the sole legacy dependency. Parsing stays in Go.
		cmd := exec.CommandContext(ctx, "zsh", "-c", `fhMode="${fhMode:-curlfull_s5}" full-html2 "$1"`, "_", url)
		cmd.Env = os.Environ()
		if o.HTMLMode != "" {
			cmd.Env = append(cmd.Env, "fhMode="+o.HTMLMode)
		}
		cmd.SysProcAttr = &syscall.SysProcAttr{Setpgid: true}
		cmd.Cancel = func() error { return syscall.Kill(-cmd.Process.Pid, syscall.SIGKILL) }
		cmd.WaitDelay = 2 * time.Second
		// Discard shell/browser chatter, which may contain environment diagnostics.
		return cmd.Output()
	}
	return c
}

func (c *Client) request(ctx context.Context, method, endpoint string, body []byte, headers map[string]string) ([]byte, *http.Response, error) {
	for attempt := 0; attempt < 3; attempt++ {
		req, err := http.NewRequestWithContext(ctx, method, endpoint, bytes.NewReader(body))
		if err != nil {
			return nil, nil, err
		}
		req.Header.Set("User-Agent", "org-paper-metadata/1.0")
		for k, v := range headers {
			req.Header.Set(k, v)
		}
		resp, err := c.HTTP.Do(req)
		if err != nil {
			return nil, nil, err
		}
		// HEAD redirects identify a page even when its HTML is bot-challenged.
		if method == http.MethodHead && PaperID(resp.Request.URL.String()) != "" {
			resp.Body.Close()
			return nil, resp, nil
		}
		data, readErr := io.ReadAll(io.LimitReader(resp.Body, (16<<20)+1))
		resp.Body.Close()
		if readErr != nil {
			return nil, resp, readErr
		}
		if len(data) > 16<<20 {
			return nil, resp, errors.New("response exceeds 16 MiB")
		}
		if resp.StatusCode >= 200 && resp.StatusCode < 300 {
			return data, resp, nil
		}
		if attempt == 2 || (resp.StatusCode != 429 && resp.StatusCode < 500) {
			return nil, resp, fmt.Errorf("%s returned HTTP %d", req.URL.Hostname(), resp.StatusCode)
		}
		delay := time.Duration(attempt+1) * time.Second
		if n, e := strconv.Atoi(resp.Header.Get("Retry-After")); e == nil && n > 0 {
			delay = time.Duration(n) * time.Second
		}
		if d, e := http.ParseTime(resp.Header.Get("Retry-After")); e == nil && time.Until(d) > 0 {
			delay = time.Until(d)
		}
		if delay > 30*time.Second {
			return nil, resp, fmt.Errorf("%s returned HTTP %d; retry requested after %s", req.URL.Hostname(), resp.StatusCode, delay.Round(time.Second))
		}
		select {
		case <-ctx.Done():
			return nil, resp, ctx.Err()
		case <-time.After(delay):
		}
	}
	panic("unreachable")
}
func (c *Client) Resolve(ctx context.Context, input string) (string, error) {
	lookup, err := Lookup(input)
	if err != nil {
		return "", err
	}
	if PaperID(lookup) != "" {
		return lookup, nil
	}
	_, resp, err := c.request(ctx, http.MethodHead, lookup, nil, nil)
	if err != nil {
		return "", fmt.Errorf("Semantic Scholar URL resolution: %w", err)
	}
	id := PaperID(resp.Request.URL.String())
	if id == "" {
		return "", errors.New("Semantic Scholar did not redirect the external ID to a public paper page")
	}
	return "https://www.semanticscholar.org/paper/" + id, nil
}
func (c *Client) pageData(ctx context.Context, url string) (Metadata, error) {
	seen := map[string]bool{}
	for step := 0; step < 8; step++ {
		id := PaperID(url)
		if seen[id] {
			return nil, errors.New("cycle in Semantic Scholar canonical IDs")
		}
		seen[id] = true
		data, _, err := c.request(ctx, http.MethodGet, "https://www.semanticscholar.org/api/1/paper/"+id, nil, nil)
		if err != nil {
			return nil, err
		}
		payload, err := decode(data)
		if err != nil {
			return nil, err
		}
		if str(payload["responseType"]) != "CANONICAL" {
			return ParsePageData(data, url)
		}
		canonical := str(payload["canonicalId"])
		if !idPattern.MatchString(canonical) {
			return nil, errors.New("invalid Semantic Scholar canonical ID")
		}
		url = "https://www.semanticscholar.org/paper/" + strings.ToLower(canonical)
	}
	return nil, errors.New("too many Semantic Scholar canonical-ID redirects")
}
func (c *Client) api(ctx context.Context, page string) (Metadata, error) {
	fields := "title,url,abstract,authors.name,year,publicationDate,venue,journal,citationCount,referenceCount,influentialCitationCount,fieldsOfStudy,isOpenAccess,externalIds,openAccessPdf"
	if c.Options.FetchBibTeX {
		fields += ",citationStyles"
	}
	endpoint := "https://api.semanticscholar.org/graph/v1/paper/" + PaperID(page) + "?fields=" + url.QueryEscape(fields)
	headers := map[string]string{}
	if c.Options.APIKey != "" {
		headers["x-api-key"] = c.Options.APIKey
	}
	data, _, err := c.request(ctx, http.MethodGet, endpoint, nil, headers)
	if err != nil {
		return nil, err
	}
	m, err := ParseJSON(data, page)
	if err == nil && !strings.EqualFold(m.First("paperId"), PaperID(page)) {
		err = errors.New("API paper ID does not match the requested paper")
	}
	return m, err
}
func (c *Client) deepseek(ctx context.Context, page string) (Metadata, error) {
	if c.Options.DeepSeekKey == "" {
		return nil, errors.New("deepseek_api_key is not configured")
	}
	body, _ := json.Marshal(map[string]string{"url": page})
	data, _, err := c.request(ctx, http.MethodPost, "https://api.deepseek.com/v1/tools/fetch", body, map[string]string{"Authorization": "Bearer " + c.Options.DeepSeekKey, "Content-Type": "application/json"})
	if err != nil {
		return nil, err
	}
	return ParseFetch(data, page)
}
func (c *Client) Fetch(ctx context.Context, input string) (Metadata, error) {
	if err := c.Options.Validate(); err != nil {
		return nil, err
	}
	page, err := c.Resolve(ctx, input)
	if err != nil {
		return nil, err
	}
	var backends []string
	switch c.Options.Scraper {
	case "", "auto":
		backends = []string{"page-data"}
		if c.Options.APIEnabled {
			backends = append(backends, "api")
		}
		if c.Options.DeepSeekKey != "" {
			backends = append(backends, "deepseek")
		}
	case "deepseek":
		backends = []string{"deepseek", "page-data"}
	case "full-html2":
		backends = []string{"full-html2", "page-data"}
	default:
		backends = []string{c.Options.Scraper}
	}
	var failures []error
	for _, backend := range backends {
		if err = ctx.Err(); err != nil {
			return nil, err
		}
		var m Metadata
		source := "semantic-scholar-" + backend
		switch backend {
		case "page-data":
			m, err = c.pageData(ctx, page)
		case "api":
			m, err = c.api(ctx, page)
		case "deepseek":
			m, err = c.deepseek(ctx, page)
			source = "deepseek-fetch"
		case "full-html2":
			var raw []byte
			raw, err = c.LoadHTML(ctx, page)
			if err == nil {
				m, err = ParseHTML(raw, page)
			}
			source = "semantic-scholar-html"
		}
		if ctx.Err() != nil {
			return nil, ctx.Err()
		}
		if err == nil {
			err = m.Validate()
		}
		if err == nil {
			if c.Options.EnrichDBLP {
				enrichmentCtx, enrichmentCancel := context.WithTimeout(ctx, 20*time.Second)
				enrichmentErr := c.enrichDBLP(enrichmentCtx, m)
				enrichmentCancel()
				if enrichmentErr != nil && c.Warnf != nil {
					if errors.Is(enrichmentErr, context.DeadlineExceeded) {
						c.Warnf("DBLP metadata enrichment timed out; keeping Semantic Scholar metadata")
					} else {
						c.Warnf("DBLP metadata enrichment failed: %v", enrichmentErr)
					}
				}
			}
			if c.Options.FetchBibTeX {
				citationTimeout := 30 * time.Second
				if c.Options.BibTeXBrowser {
					citationTimeout = 50 * time.Second
				}
				citationCtx, cancel := context.WithTimeout(ctx, citationTimeout)
				citationErr := c.fetchBibTeX(citationCtx, m)
				cancel()
				if citationErr != nil && c.Warnf != nil {
					c.Warnf("BibTeX export failed: %v", citationErr)
				}
			}
			if ctx.Err() != nil {
				return nil, ctx.Err()
			}
			m.Stamp(source, c.Now())
			return m, nil
		}
		failures = append(failures, fmt.Errorf("%s failed: %w", backend, err))
		if c.Warnf != nil {
			c.Warnf("%s failed: %v", backend, err)
		}
	}
	return nil, fmt.Errorf("could not recover complete Semantic Scholar metadata: %w", errors.Join(failures...))
}
