package main

import (
	"bufio"
	"bytes"
	"encoding/json"
	"fmt"
	"net/http"
	"net/http/httptest"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
)

func fixture(t *testing.T) string {
	t.Helper()
	path := filepath.Join(t.TempDir(), "History")
	query := `CREATE TABLE urls(id INTEGER PRIMARY KEY,url TEXT,title TEXT,hidden INTEGER,last_visit_time INTEGER);
    INSERT INTO urls VALUES(1,'https://example.org/old','Old',0,10),
    (2,'https://example.org/new','New',0,20),(3,'https://example.org/hidden','Hidden',1,30),
    (4,'file:///fixture','Local',0,40);`
	if out, err := exec.Command("/usr/bin/sqlite3", path, query).CombinedOutput(); err != nil {
		t.Fatalf("fixture: %v %s", err, out)
	}
	return path
}
func TestSnapshotAndFilters(t *testing.T) {
	path := fixture(t)
	before, _ := os.ReadFile(path)
	rows, err := snapshot(path)
	if err != nil || len(rows) != 2 || rows[0].Title != "New" {
		t.Fatalf("snapshot: %v %#v", err, rows)
	}
	after, _ := os.ReadFile(path)
	if !bytes.Equal(before, after) {
		t.Fatal("source database changed")
	}
	if _, err = snapshot(filepath.Join(t.TempDir(), "missing")); err == nil {
		t.Fatal("missing history accepted")
	}
}
func TestLockedTransactions(t *testing.T) {
	for _, mode := range []string{"rollback", "wal"} {
		t.Run(mode, func(t *testing.T) {
			path := fixture(t)
			script := `import sqlite3,sys
d=sqlite3.connect(sys.argv[1])
if sys.argv[2]=='wal':
 d.execute('PRAGMA journal_mode=WAL')
 d.execute("UPDATE urls SET title='WAL committed' WHERE id=2");d.commit()
else:
 d.execute('PRAGMA locking_mode=EXCLUSIVE');d.execute('PRAGMA cache_size=1');d.execute('BEGIN EXCLUSIVE')
d.execute('UPDATE urls SET title=? WHERE id=2',('Pending'*10000,))
print('READY',flush=True)
input()
d.rollback();d.close()
`
			cmd := exec.Command("/usr/bin/python3", "-u", "-c", script, path, mode)
			in, _ := cmd.StdinPipe()
			out, _ := cmd.StdoutPipe()
			if err := cmd.Start(); err != nil {
				t.Fatal(err)
			}
			defer func() { in.Close(); cmd.Wait() }()
			if line, err := bufio.NewReader(out).ReadString('\n'); err != nil || line != "READY\n" {
				t.Fatalf("fixture readiness: %v", err)
			}
			rows, err := snapshot(path)
			expected := "New"
			if mode == "wal" {
				expected = "WAL committed"
			}
			if err != nil || len(rows) != 2 || rows[0].Title != expected {
				t.Fatalf("uncommitted transaction leaked: %v %#v", err, rows)
			}
		})
	}
}
func testRows() []entry {
	return []entry{
		{URL: "https://chat.example.org/channel/two", Title: "Channel [two]+", host: "chat.example.org", search: lower("Channel [two]+\nhttps://chat.example.org/channel/two\nhistory")},
		{URL: "https://example.org/", Title: "Other", host: "example.org", search: "other\nhttps://example.org/\nhistory"},
	}
}
func TestLiteralANDAndAliases(t *testing.T) {
	rows := testRows()
	request := searchRequest{Query: "TEAM [two]+", Aliases: json.RawMessage(`{"chat.example.org":["Team chat",42]}`)}
	if got := matches(rows, request); len(got) != 1 {
		t.Fatalf("AND alias match: %d", len(got))
	}
	request.Query = "[two]+ missing"
	if len(matches(rows, request)) != 0 {
		t.Fatal("AND terms ignored")
	}
	request.Query = "chat channel"
	request.Exclude = []string{"https://CHAT.EXAMPLE.ORG:443/channel/two"}
	if len(matches(rows, request)) != 0 {
		t.Fatal("existing tab URL not suppressed")
	}
	request = searchRequest{Query: ".*"}
	if len(matches(rows, request)) != 0 {
		t.Fatal("regex instead of literal match")
	}
	rows = append(rows, entry{URL: "https://EXAMPLE.ORG:443", search: "history"})
	if len(matches(rows, searchRequest{})) != 2 {
		t.Fatal("duplicate URL not suppressed")
	}
	huge := make([]entry, 1000)
	for i := range huge {
		huge[i] = entry{URL: fmt.Sprintf("https://example.org/%d", i), search: "history"}
	}
	if len(matches(huge, searchRequest{})) != maxMatches {
		t.Fatal("response was not bounded")
	}
}
func TestHTTPContract(t *testing.T) {
	idx := &index{rows: testRows(), ready: true, revision: 1}
	handler := idx.handler("test-token")
	call := func(auth, origin, body string) *httptest.ResponseRecorder {
		r := httptest.NewRequest(http.MethodPost, "http://127.0.0.1/search", strings.NewReader(body))
		r.Header.Set("Authorization", auth)
		r.Header.Set("Origin", origin)
		w := httptest.NewRecorder()
		handler.ServeHTTP(w, r)
		return w
	}
	if call("", "", `{}`).Code != 403 {
		t.Fatal("missing authorization accepted")
	}
	if call("Bearer test-token", "https://example.org", `{}`).Code != 403 {
		t.Fatal("browser-origin request accepted")
	}
	if call("Bearer test-token", "", `{"query":`).Code != 400 {
		t.Fatal("malformed body accepted")
	}
	w := call("Bearer test-token", "", `{"query":"channel two","aliases":[],"exclude":[]}`)
	var result searchResult
	if w.Code != 200 || json.Unmarshal(w.Body.Bytes(), &result) != nil || !result.OK || len(result.History) != 1 {
		t.Fatalf("search result: %d", w.Code)
	}
	if w.Header().Get("Cache-Control") != "no-store" || w.Header().Get("Access-Control-Allow-Origin") != "" {
		t.Fatal("unexpected caching or CORS")
	}
	idx.lastError = true
	w = call("Bearer test-token", "", `{}`)
	if json.Unmarshal(w.Body.Bytes(), &result) != nil || result.OK || len(result.History) != 0 {
		t.Fatal("failed refresh returned stale history")
	}
}
func BenchmarkIndexedSearch(b *testing.B) {
	rows := make([]entry, maxRows)
	for i := range rows {
		value := fmt.Sprintf("https://example.org/paper/%d", i)
		rows[i] = entry{URL: value, search: lower("Research paper\n" + value + "\nhistory")}
	}
	request := searchRequest{Query: "paper 4999"}
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		matches(rows, request)
	}
}
