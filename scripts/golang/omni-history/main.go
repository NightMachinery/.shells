package main

import (
	"crypto/rand"
	"crypto/subtle"
	"encoding/hex"
	"encoding/json"
	"errors"
	"flag"
	"fmt"
	"io"
	"log"
	"net"
	"net/http"
	"net/url"
	"os"
	"os/signal"
	"path/filepath"
	"reflect"
	"regexp"
	"strings"
	"sync"
	"syscall"
	"time"
)

const version = "1.0.0"
const maxRows = 5000
const maxMatches = 200

type entry struct {
	URL          string `json:"url"`
	Title        string `json:"title"`
	search, host string
}
type fileStamp struct {
	Exists   bool
	Size     int64
	Modified int64
	Info     os.FileInfo
}

func stamp(path string) fileStamp {
	info, err := os.Stat(path)
	if err != nil {
		return fileStamp{}
	}
	return fileStamp{true, info.Size(), info.ModTime().UnixNano(), info}
}
func sameStamp(a, b fileStamp) bool {
	return a.Exists == b.Exists && a.Size == b.Size && a.Modified == b.Modified &&
		(!a.Exists || os.SameFile(a.Info, b.Info))
}
func historyStamps(path string) []fileStamp {
	return []fileStamp{stamp(path), stamp(path + "-journal"), stamp(path + "-wal")}
}
func sameStamps(a, b []fileStamp) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if !sameStamp(a[i], b[i]) {
			return false
		}
	}
	return true
}
func copyFile(source, target string) error {
	in, err := os.Open(source)
	if err != nil {
		return err
	}
	defer in.Close()
	out, err := os.OpenFile(target, os.O_WRONLY|os.O_CREATE|os.O_EXCL, 0600)
	if err != nil {
		return err
	}
	_, copyErr := io.Copy(out, in)
	closeErr := out.Close()
	if copyErr != nil {
		return copyErr
	}
	return closeErr
}
func snapshot(path string) ([]entry, error) {
	names := []string{path, path + "-journal", path + "-wal"}
	for attempt := 0; attempt < 3; attempt++ {
		before := historyStamps(path)
		if !before[0].Exists {
			return nil, errors.New("history unavailable")
		}
		dir, err := os.MkdirTemp("", "hs-omni-history-")
		if err != nil {
			return nil, err
		}
		var rows [][]string
		func() {
			defer os.RemoveAll(dir)
			for i, name := range names {
				if before[i].Exists {
					if err = copyFile(name, filepath.Join(dir, filepath.Base(name))); err != nil {
						return
					}
				}
			}
			if !sameStamps(before, historyStamps(path)) {
				err = errors.New("history changed during snapshot")
				return
			}
			target := filepath.Join(dir, filepath.Base(path))
			var check [][]string
			check, err = sqliteQuery(target, "PRAGMA quick_check(1)")
			if err != nil {
				return
			}
			if len(check) != 1 || len(check[0]) != 1 || check[0][0] != "ok" {
				err = errors.New("snapshot integrity failed")
				return
			}
			rows, err = sqliteQuery(target, fmt.Sprintf(`SELECT url,title FROM urls WHERE hidden=0
                AND (url LIKE 'https://%%' OR url LIKE 'http://%%') ORDER BY last_visit_time DESC,id DESC LIMIT %d`, maxRows))
		}()
		if err != nil {
			time.Sleep(30 * time.Millisecond)
			continue
		}
		result := make([]entry, 0, len(rows))
		for _, row := range rows {
			parsed, e := url.Parse(row[0])
			if e != nil || (parsed.Scheme != "https" && parsed.Scheme != "http") || parsed.Hostname() == "" {
				continue
			}
			result = append(result, entry{URL: row[0], Title: row[1], host: strings.ToLower(parsed.Hostname()), search: lower(row[1] + "\n" + row[0] + "\nhistory")})
		}
		return result, nil
	}
	return nil, errors.New("history snapshot unavailable")
}

// Match Lua's existing case-insensitive literal policy without Unicode folding.
func lower(s string) string {
	return strings.Map(func(r rune) rune {
		if r >= 'A' && r <= 'Z' {
			return r + 32
		}
		return r
	}, s)
}

var originPattern = regexp.MustCompile(`(?i)^(https?)://([^/?#]+)([^?#]*)([?#].*)?$`)

func canonical(value string) string {
	m := originPattern.FindStringSubmatch(value)
	if m == nil {
		return value
	}
	scheme, host, path := strings.ToLower(m[1]), strings.ToLower(m[2]), m[3]
	if scheme == "https" {
		host = strings.TrimSuffix(host, ":443")
	} else {
		host = strings.TrimSuffix(host, ":80")
	}
	if path == "" {
		path = "/"
	}
	return scheme + "://" + host + path + m[4]
}

type searchRequest struct {
	Query   string          `json:"query"`
	Aliases json.RawMessage `json:"aliases"`
	Exclude []string        `json:"exclude"`
}
type searchResult struct {
	OK       bool    `json:"ok"`
	Ready    bool    `json:"ready"`
	History  []entry `json:"history"`
	Total    int     `json:"total"`
	Revision uint64  `json:"revision"`
	Error    string  `json:"error,omitempty"`
}

func matches(rows []entry, request searchRequest) []entry {
	terms := strings.FieldsFunc(lower(request.Query), func(r rune) bool { return r == ' ' || r == '\t' || r == '\n' || r == '\r' || r == '\v' || r == '\f' })
	aliases := map[string]string{}
	var rawAliases map[string]json.RawMessage
	json.Unmarshal(request.Aliases, &rawAliases)
	for host, raw := range rawAliases {
		var words []any
		if json.Unmarshal(raw, &words) != nil {
			continue
		}
		values := []string{}
		for _, word := range words {
			if text, ok := word.(string); ok {
				values = append(values, lower(text))
			}
		}
		aliases[host] = strings.Join(values, " ")
	}
	seen := map[string]bool{}
	for _, value := range request.Exclude {
		seen[canonical(value)] = true
	}
	result := make([]entry, 0, maxMatches)
	for _, row := range rows {
		match := true
		for _, term := range terms {
			if !strings.Contains(row.search, term) && !strings.Contains(aliases[row.host], term) {
				match = false
				break
			}
		}
		if !match {
			continue
		}
		key := canonical(row.URL)
		if seen[key] {
			continue
		}
		seen[key] = true
		result = append(result, row)
		if len(result) == maxMatches {
			break
		}
	}
	return result
}

type index struct {
	sync.RWMutex
	rows      []entry
	ready     bool
	revision  uint64
	lastError bool
}

func (idx *index) handler(token string) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Cache-Control", "no-store")
		if r.Method != "POST" || r.URL.Path != "/search" || r.Header.Get("Origin") != "" ||
			subtle.ConstantTimeCompare([]byte(r.Header.Get("Authorization")), []byte("Bearer "+token)) != 1 {
			http.Error(w, "Forbidden", http.StatusForbidden)
			return
		}
		var request searchRequest
		r.Body = http.MaxBytesReader(w, r.Body, 1<<20)
		if json.NewDecoder(r.Body).Decode(&request) != nil || len(request.Query) > 65536 {
			http.Error(w, "Invalid request", http.StatusBadRequest)
			return
		}
		idx.RLock()
		rows, ready, revision, failed := idx.rows, idx.ready, idx.revision, idx.lastError
		idx.RUnlock()
		result := searchResult{OK: true, Ready: ready, History: []entry{}, Total: len(rows), Revision: revision}
		if failed {
			result.OK = false
			result.Error = "Browser history could not be refreshed"
		} else if ready {
			result.History = matches(rows, request)
		}
		w.Header().Set("Content-Type", "application/json")
		json.NewEncoder(w).Encode(result)
	})
}
func (idx *index) refresh(root string, quit <-chan os.Signal, parent int) {
	var old []fileStamp
	var stateStamp fileStamp
	profile := "Default"
	previousPath := ""
	checked := time.Time{}
	tick := time.NewTicker(2 * time.Second)
	defer tick.Stop()
	for {
		if parent > 0 && syscall.Kill(parent, 0) != nil {
			return
		}
		statePath := filepath.Join(root, "Local State")
		next := stamp(statePath)
		if !sameStamp(stateStamp, next) {
			profile = "Default"
			var state struct {
				Profile struct {
					LastUsed string `json:"last_used"`
				} `json:"profile"`
			}
			raw, err := os.ReadFile(statePath)
			if err == nil && json.Unmarshal(raw, &state) == nil {
				name := state.Profile.LastUsed
				if name != "" && name != "." && name != ".." && filepath.Base(name) == name {
					profile = name
				}
			}
			stateStamp = next
		}
		path := filepath.Join(root, profile, "History")
		current := historyStamps(path)
		// Retry failures and refresh at least once/minute even if timestamps lie.
		if path != previousPath || (!sameStamps(current, old) && time.Since(checked) >= 5*time.Second) || time.Since(checked) > time.Minute {
			rows, err := snapshot(path)
			idx.Lock()
			if err == nil {
				if path != previousPath || !reflect.DeepEqual(rows, idx.rows) {
					idx.revision++
				}
				idx.rows = rows
				idx.ready = true
				idx.lastError = false
				old = current
				previousPath = path
				checked = time.Now()
			} else {
				idx.rows = nil
				idx.ready = false
				idx.lastError = true
				old = nil
			}
			idx.Unlock()
		}
		select {
		case <-quit:
			return
		case <-tick.C:
		}
	}
}
func main() {
	home, _ := os.UserHomeDir()
	root := flag.String("root", filepath.Join(home, "Library", "Application Support", "BraveSoftware", "Brave-Browser"), "Chromium user-data directory")
	parent := flag.Int("parent", 0, "exit when this parent process disappears")
	flag.Parse()
	listener, err := net.Listen("tcp4", "127.0.0.1:0")
	if err != nil {
		os.Exit(1)
	}
	secret := make([]byte, 32)
	if _, err = rand.Read(secret); err != nil {
		os.Exit(1)
	}
	token := hex.EncodeToString(secret)
	idx := &index{}
	quit := make(chan os.Signal, 1)
	signal.Notify(quit, os.Interrupt, syscall.SIGTERM)
	server := &http.Server{Handler: idx.handler(token), ReadHeaderTimeout: 2 * time.Second, ReadTimeout: 3 * time.Second, WriteTimeout: 3 * time.Second, IdleTimeout: 10 * time.Second, ErrorLog: log.New(io.Discard, "", 0)}
	go func() { idx.refresh(*root, quit, *parent); server.Close() }()
	// Only one small startup frame uses stdout. History never travels via a pipe.
	json.NewEncoder(os.Stdout).Encode(map[string]any{"version": version, "url": "http://" + listener.Addr().String() + "/search", "token": token})
	server.Serve(listener)
}
