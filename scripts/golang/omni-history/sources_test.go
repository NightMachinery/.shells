package main

import (
	"encoding/json"
	"net/http/httptest"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
)

func TestBrowserSchemas(t *testing.T) {
	for _, kind := range []string{"safari", "firefox"} {
		t.Run(kind, func(t *testing.T) {
			path := filepath.Join(t.TempDir(), "History.db")
			query := `CREATE TABLE history_items(id INTEGER PRIMARY KEY,url TEXT);CREATE TABLE history_visits(history_item INTEGER,title TEXT,visit_time REAL);
 INSERT INTO history_items VALUES(1,'https://example.org/old'),(2,'https://example.org/new'),(3,'file:///private');
 INSERT INTO history_visits VALUES(1,'Older',1),(2,'New',2),(2,'Newest',3),(3,'Local',4);`
			if kind == "firefox" {
				query = `CREATE TABLE moz_places(id INTEGER PRIMARY KEY,url TEXT,title TEXT,hidden INTEGER,last_visit_date INTEGER);
 INSERT INTO moz_places VALUES(1,'https://example.org/old','Older',0,1),(2,'https://example.org/new','Newest',0,3),(3,'file:///private','Local',0,4),(4,'https://example.org/hidden','Hidden',1,5);`
			}
			if out, err := exec.Command("/usr/bin/sqlite3", path, query).CombinedOutput(); err != nil {
				t.Fatalf("fixture: %v %s", err, out)
			}
			before, _ := os.ReadFile(path)
			rows, err := snapshotKind(path, kind)
			after, _ := os.ReadFile(path)
			if err != nil || len(rows) != 2 || rows[0].Title != "Newest" {
				t.Fatalf("schema: %v %#v", err, rows)
			}
			if string(before) != string(after) {
				t.Fatal("source changed")
			}
		})
	}
}
func TestFirefoxProfile(t *testing.T) {
	root := t.TempDir()
	path := filepath.Join(root, "profiles.ini")
	os.WriteFile(path, []byte("[Profile0]\nPath=Profiles/first\nIsRelative=1\nDefault=1\n[Profile1]\nPath=Profiles/second\nIsRelative=1\n[InstallFixture]\nDefault=Profiles/second\n"), 0600)
	if got := firefoxProfile(root); got != filepath.Join(root, "Profiles/second") {
		t.Fatal(got)
	}
	os.WriteFile(path, []byte("[Profile0]\nPath=../../outside\nIsRelative=1\nDefault=1\n"), 0600)
	if got := firefoxProfile(root); got != filepath.Join(root, "unavailable-profile") {
		t.Fatal(got)
	}
}
func TestBrowserIndexIsolation(t *testing.T) {
	brave := &index{ready: true, rows: []entry{{URL: "https://example.org/brave", search: "brave"}}}
	safari := &index{ready: true, rows: []entry{{URL: "https://example.org/safari", search: "safari"}}}
	handler := brave.handler("password", func(browser string) (*index, error) {
		if browser == "safari" {
			return safari, nil
		}
		return brave, nil
	})
	req := httptest.NewRequest("POST", "/search", strings.NewReader(`{"browser":"safari","query":""}`))
	req.SetBasicAuth("omni", "password")
	w := httptest.NewRecorder()
	handler.ServeHTTP(w, req)
	var result searchResult
	json.Unmarshal(w.Body.Bytes(), &result)
	if w.Code != 200 || len(result.History) != 1 || result.History[0].URL != "https://example.org/safari" {
		t.Fatalf("isolation: %d %s", w.Code, w.Body.String())
	}
	sources := newSources(t.TempDir(), t.TempDir(), brave, make(chan os.Signal), 0)
	if _, err := sources.get("../../private"); err == nil {
		t.Fatal("arbitrary source accepted")
	}
	if sources.sources["safari"].index != nil {
		t.Fatal("unused source started")
	}
}
