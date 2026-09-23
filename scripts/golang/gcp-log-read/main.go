// gcp-log-read: `gcloud logging read`, but with the time window cut into
// slices that are fetched in parallel.
//
// Why: the Logging API pages sequentially, and each page of an audit-log
// query takes the backend about five seconds to produce whether it holds
// forty entries or eight hundred. A month of fleet lifecycle events is six
// pages, so `gcloud logging read` spends half a minute waiting on round trips
// it cannot overlap. Cutting the window into N slices turns that into N
// independent queries, each one or two pages long, run at once.
//
// The output is a JSON array of the raw LogEntry objects, newest first, which
// is what `gcloud logging read --format=json` prints, so a caller can swap
// one for the other. Any slice that fails fails the whole run: a partial read
// would silently drop lifecycle events, and a spend estimate built on it
// would under-report.
//
// Stdlib only, so `go install` works offline.
package main

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"flag"
	"fmt"
	"io"
	"net/http"
	"os"
	"os/exec"
	"sort"
	"strings"
	"sync"
	"time"
)

const endpoint = "https://logging.googleapis.com/v2/entries:list"

type options struct {
	project  string
	filter   string
	since    time.Time
	until    time.Time
	slices   int
	limit    int
	pageSize int
	timeout  time.Duration
	retries  int
}

type slice struct {
	from, to time.Time
}

// entry keeps the LogEntry verbatim and decodes only what sorting and
// de-duplication need.
type entry struct {
	raw       json.RawMessage
	insertID  string
	timestamp time.Time
}

func main() {
	opts, err := parseFlags(os.Args[1:])
	if err != nil {
		fmt.Fprintln(os.Stderr, "gcp-log-read:", err)
		os.Exit(2)
	}

	token, err := accessToken()
	if err != nil {
		fmt.Fprintln(os.Stderr, "gcp-log-read:", err)
		os.Exit(1)
	}

	entries, err := fetchAll(context.Background(), http.DefaultClient, endpoint, token, opts)
	if err != nil {
		fmt.Fprintln(os.Stderr, "gcp-log-read:", err)
		os.Exit(1)
	}

	if err := writeJSON(os.Stdout, entries); err != nil {
		fmt.Fprintln(os.Stderr, "gcp-log-read:", err)
		os.Exit(1)
	}
}

func parseFlags(args []string) (options, error) {
	var o options
	var since, until string
	fs := flag.NewFlagSet("gcp-log-read", flag.ContinueOnError)
	fs.StringVar(&o.project, "project", "", "project id (required)")
	fs.StringVar(&o.filter, "filter", "", "Logging query, WITHOUT a timestamp bound; the slices add their own")
	fs.StringVar(&since, "since", "", "window start, RFC3339 (required)")
	fs.StringVar(&until, "until", "", "window end, RFC3339 (default: now)")
	fs.IntVar(&o.slices, "slices", 8, "parallel time slices")
	fs.IntVar(&o.limit, "limit", 0, "keep only the newest N entries (0: all)")
	fs.IntVar(&o.pageSize, "page-size", 1000, "entries per request (API maximum 1000)")
	fs.DurationVar(&o.timeout, "timeout", 120*time.Second, "per-request timeout")
	fs.IntVar(&o.retries, "retries", 5, "retries per request on 429 and 5xx")
	if err := fs.Parse(args); err != nil {
		return o, err
	}

	if o.project == "" {
		return o, errors.New("--project is required")
	}
	if since == "" {
		return o, errors.New("--since is required")
	}
	var err error
	if o.since, err = time.Parse(time.RFC3339, since); err != nil {
		return o, fmt.Errorf("--since: %w", err)
	}
	o.until = time.Now().UTC()
	if until != "" {
		if o.until, err = time.Parse(time.RFC3339, until); err != nil {
			return o, fmt.Errorf("--until: %w", err)
		}
	}
	if !o.until.After(o.since) {
		return o, errors.New("--until must be after --since")
	}
	if o.slices < 1 {
		o.slices = 1
	}
	if o.pageSize < 1 || o.pageSize > 1000 {
		o.pageSize = 1000
	}
	return o, nil
}

// accessToken prefers $GCP_ACCESS_TOKEN, so a caller that already holds one
// saves a gcloud start, and otherwise asks gcloud for the active account's.
func accessToken() (string, error) {
	if t := strings.TrimSpace(os.Getenv("GCP_ACCESS_TOKEN")); t != "" {
		return t, nil
	}
	out, err := exec.Command("gcloud", "auth", "print-access-token").Output()
	if err != nil {
		return "", fmt.Errorf("gcloud auth print-access-token: %w", err)
	}
	t := strings.TrimSpace(string(out))
	if t == "" {
		return "", errors.New("gcloud auth print-access-token printed nothing")
	}
	return t, nil
}

// splitWindow cuts [from, to) into n contiguous, non-overlapping slices.
func splitWindow(from, to time.Time, n int) []slice {
	total := to.Sub(from)
	if n < 1 {
		n = 1
	}
	step := total / time.Duration(n)
	// A slice narrower than a second buys nothing and makes the RFC3339
	// bounds collide.
	if step < time.Second {
		return []slice{{from, to}}
	}
	out := make([]slice, 0, n)
	for i := 0; i < n; i++ {
		a := from.Add(step * time.Duration(i))
		b := from.Add(step * time.Duration(i+1))
		if i == n-1 {
			b = to
		}
		out = append(out, slice{a, b})
	}
	return out
}

func sliceFilter(base string, s slice) string {
	// `>=` on the left and `<` on the right, so a boundary entry lands in
	// exactly one slice.
	bound := fmt.Sprintf(`timestamp>="%s" AND timestamp<"%s"`,
		s.from.UTC().Format(time.RFC3339Nano), s.to.UTC().Format(time.RFC3339Nano))
	if strings.TrimSpace(base) == "" {
		return bound
	}
	return "(" + base + ") AND " + bound
}

func fetchAll(ctx context.Context, client *http.Client, url, token string, o options) ([]entry, error) {
	slices := splitWindow(o.since, o.until, o.slices)

	type result struct {
		entries []entry
		err     error
	}
	results := make([]result, len(slices))

	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	var wg sync.WaitGroup
	for i, s := range slices {
		wg.Add(1)
		go func(i int, s slice) {
			defer wg.Done()
			es, err := fetchSlice(ctx, client, url, token, o, s)
			if err != nil {
				// One failed slice dooms the run; stop the others early.
				cancel()
			}
			results[i] = result{es, err}
		}(i, s)
	}
	wg.Wait()

	var merged []entry
	for i, r := range results {
		if r.err != nil && !errors.Is(r.err, context.Canceled) {
			return nil, fmt.Errorf("slice %s..%s: %w",
				slices[i].from.Format(time.RFC3339), slices[i].to.Format(time.RFC3339), r.err)
		}
	}
	for _, r := range results {
		if r.err != nil {
			return nil, r.err
		}
		merged = append(merged, r.entries...)
	}
	return mergeEntries(merged, o.limit), nil
}

// mergeEntries de-duplicates by insertId, sorts newest first (the order
// `gcloud logging read` uses) and keeps the newest `limit`.
func mergeEntries(es []entry, limit int) []entry {
	seen := make(map[string]bool, len(es))
	out := es[:0]
	for _, e := range es {
		if e.insertID != "" {
			key := e.insertID + "\x00" + e.timestamp.Format(time.RFC3339Nano)
			if seen[key] {
				continue
			}
			seen[key] = true
		}
		out = append(out, e)
	}
	sort.SliceStable(out, func(i, j int) bool { return out[i].timestamp.After(out[j].timestamp) })
	if limit > 0 && len(out) > limit {
		out = out[:limit]
	}
	return out
}

type listRequest struct {
	ResourceNames []string `json:"resourceNames"`
	Filter        string   `json:"filter"`
	OrderBy       string   `json:"orderBy"`
	PageSize      int      `json:"pageSize"`
	PageToken     string   `json:"pageToken,omitempty"`
}

type listResponse struct {
	Entries       []json.RawMessage `json:"entries"`
	NextPageToken string            `json:"nextPageToken"`
}

func fetchSlice(ctx context.Context, client *http.Client, url, token string, o options, s slice) ([]entry, error) {
	req := listRequest{
		ResourceNames: []string{"projects/" + o.project},
		Filter:        sliceFilter(o.filter, s),
		OrderBy:       "timestamp desc",
		PageSize:      o.pageSize,
	}
	var out []entry
	for {
		resp, err := postWithRetry(ctx, client, url, token, req, o)
		if err != nil {
			return nil, err
		}
		for _, raw := range resp.Entries {
			e, err := decodeEntry(raw)
			if err != nil {
				return nil, err
			}
			out = append(out, e)
		}
		// The API may hand back an empty page that still carries a token:
		// it stopped scanning at a deadline, not at the end. Only a missing
		// token means done.
		if resp.NextPageToken == "" {
			return out, nil
		}
		req.PageToken = resp.NextPageToken
	}
}

func decodeEntry(raw json.RawMessage) (entry, error) {
	var head struct {
		InsertID  string `json:"insertId"`
		Timestamp string `json:"timestamp"`
	}
	if err := json.Unmarshal(raw, &head); err != nil {
		return entry{}, fmt.Errorf("undecodable entry: %w", err)
	}
	ts, _ := time.Parse(time.RFC3339Nano, head.Timestamp)
	return entry{raw: raw, insertID: head.InsertID, timestamp: ts}, nil
}

func postWithRetry(ctx context.Context, client *http.Client, url, token string, body listRequest, o options) (listResponse, error) {
	payload, err := json.Marshal(body)
	if err != nil {
		return listResponse{}, err
	}

	backoff := time.Second
	for attempt := 0; ; attempt++ {
		resp, status, err := post(ctx, client, url, token, payload, o.timeout)
		if err == nil {
			return resp, nil
		}
		if ctx.Err() != nil {
			return listResponse{}, ctx.Err()
		}
		// 429 is the per-project read quota, which a shared project spends
		// together; 5xx is the backend. Both are worth waiting out. A 4xx is
		// our mistake and will not improve.
		retryable := status == 0 || status == http.StatusTooManyRequests || status >= 500
		if !retryable || attempt >= o.retries {
			return listResponse{}, err
		}
		select {
		case <-time.After(backoff):
		case <-ctx.Done():
			return listResponse{}, ctx.Err()
		}
		backoff *= 2
	}
}

func post(ctx context.Context, client *http.Client, url, token string, payload []byte, timeout time.Duration) (listResponse, int, error) {
	ctx, cancel := context.WithTimeout(ctx, timeout)
	defer cancel()

	req, err := http.NewRequestWithContext(ctx, http.MethodPost, url, bytes.NewReader(payload))
	if err != nil {
		return listResponse{}, 0, err
	}
	req.Header.Set("Authorization", "Bearer "+token)
	req.Header.Set("Content-Type", "application/json")

	res, err := client.Do(req)
	if err != nil {
		return listResponse{}, 0, err
	}
	defer res.Body.Close()
	data, err := io.ReadAll(res.Body)
	if err != nil {
		return listResponse{}, res.StatusCode, err
	}
	if res.StatusCode != http.StatusOK {
		msg := strings.TrimSpace(string(data))
		var apiErr struct {
			Error struct {
				Message string `json:"message"`
			} `json:"error"`
		}
		if json.Unmarshal(data, &apiErr) == nil && apiErr.Error.Message != "" {
			msg = apiErr.Error.Message
		}
		return listResponse{}, res.StatusCode, fmt.Errorf("HTTP %d: %s", res.StatusCode, msg)
	}

	var out listResponse
	if err := json.Unmarshal(data, &out); err != nil {
		return listResponse{}, res.StatusCode, fmt.Errorf("undecodable response: %w", err)
	}
	return out, res.StatusCode, nil
}

func writeJSON(w io.Writer, es []entry) error {
	var buf bytes.Buffer
	buf.WriteByte('[')
	for i, e := range es {
		if i > 0 {
			buf.WriteByte(',')
		}
		buf.Write(e.raw)
	}
	buf.WriteString("]\n")
	_, err := w.Write(buf.Bytes())
	return err
}
