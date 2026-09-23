package main

import (
	"context"
	"encoding/json"
	"fmt"
	"net/http"
	"net/http/httptest"
	"strings"
	"sync/atomic"
	"testing"
	"time"
)

func TestSplitWindowCoversExactly(t *testing.T) {
	from := time.Date(2026, 9, 1, 0, 0, 0, 0, time.UTC)
	to := from.Add(37*24*time.Hour + 17*time.Second)
	ss := splitWindow(from, to, 8)
	if len(ss) != 8 {
		t.Fatalf("got %d slices, want 8", len(ss))
	}
	if !ss[0].from.Equal(from) || !ss[len(ss)-1].to.Equal(to) {
		t.Fatalf("slices do not span the window: %v", ss)
	}
	for i := 1; i < len(ss); i++ {
		if !ss[i].from.Equal(ss[i-1].to) {
			t.Fatalf("gap or overlap between slice %d and %d", i-1, i)
		}
	}
}

func TestSplitWindowTinyIsOneSlice(t *testing.T) {
	from := time.Date(2026, 9, 1, 0, 0, 0, 0, time.UTC)
	if n := len(splitWindow(from, from.Add(3*time.Second), 8)); n != 1 {
		t.Fatalf("got %d slices, want 1", n)
	}
}

func mkEntry(id string, ts time.Time) entry {
	raw := fmt.Sprintf(`{"insertId":%q,"timestamp":%q}`, id, ts.Format(time.RFC3339Nano))
	e, err := decodeEntry(json.RawMessage(raw))
	if err != nil {
		panic(err)
	}
	return e
}

func TestMergeSortsNewestFirstDedupesAndLimits(t *testing.T) {
	t0 := time.Date(2026, 9, 1, 0, 0, 0, 0, time.UTC)
	es := []entry{
		mkEntry("a", t0),
		mkEntry("c", t0.Add(2*time.Hour)),
		mkEntry("b", t0.Add(time.Hour)),
		mkEntry("c", t0.Add(2*time.Hour)),
	}
	got := mergeEntries(es, 2)
	if len(got) != 2 || got[0].insertID != "c" || got[1].insertID != "b" {
		t.Fatalf("got %v", got)
	}
}

// A fake Logging API: every slice returns one entry per page for two pages,
// the first slice 429s once, and an empty page with a token must not end
// the paging.
func TestFetchAllPagesRetriesAndMerges(t *testing.T) {
	var calls, throttled int32
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		atomic.AddInt32(&calls, 1)
		var req listRequest
		if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
			t.Errorf("bad request body: %v", err)
		}
		if r.Header.Get("Authorization") != "Bearer tok" {
			t.Errorf("missing bearer token")
		}
		if !strings.Contains(req.Filter, `timestamp>="`) || !strings.HasPrefix(req.Filter, "(base) AND ") {
			t.Errorf("filter not bounded: %q", req.Filter)
		}
		lo := req.Filter[strings.Index(req.Filter, `>="`)+3:]
		lo = lo[:strings.Index(lo, `"`)]
		start, _ := time.Parse(time.RFC3339Nano, lo)
		if start.Equal(time.Date(2026, 9, 1, 0, 0, 0, 0, time.UTC)) && req.PageToken == "" &&
			atomic.CompareAndSwapInt32(&throttled, 0, 1) {
			w.WriteHeader(http.StatusTooManyRequests)
			fmt.Fprint(w, `{"error":{"message":"quota"}}`)
			return
		}
		var resp string
		switch req.PageToken {
		case "":
			resp = fmt.Sprintf(`{"entries":[{"insertId":"%s-1","timestamp":%q}],"nextPageToken":"empty"}`,
				lo, start.Add(time.Minute).Format(time.RFC3339Nano))
		case "empty":
			resp = `{"entries":[],"nextPageToken":"p2"}`
		case "p2":
			resp = fmt.Sprintf(`{"entries":[{"insertId":"%s-2","timestamp":%q}]}`,
				lo, start.Add(2*time.Minute).Format(time.RFC3339Nano))
		}
		fmt.Fprint(w, resp)
	}))
	defer srv.Close()

	from := time.Date(2026, 9, 1, 0, 0, 0, 0, time.UTC)
	o := options{
		project: "p", filter: "base", since: from, until: from.Add(4 * time.Hour),
		slices: 4, pageSize: 1000, timeout: 5 * time.Second, retries: 2,
	}
	got, err := fetchAll(context.Background(), srv.Client(), srv.URL, "tok", o)
	if err != nil {
		t.Fatal(err)
	}
	if len(got) != 8 {
		t.Fatalf("got %d entries, want 8", len(got))
	}
	for i := 1; i < len(got); i++ {
		if got[i].timestamp.After(got[i-1].timestamp) {
			t.Fatalf("not newest first at %d", i)
		}
	}
	if throttled != 1 {
		t.Fatalf("the 429 path was not exercised")
	}
}

func TestFetchAllFailsOnHardError(t *testing.T) {
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusForbidden)
		fmt.Fprint(w, `{"error":{"message":"denied"}}`)
	}))
	defer srv.Close()

	from := time.Date(2026, 9, 1, 0, 0, 0, 0, time.UTC)
	o := options{project: "p", since: from, until: from.Add(time.Hour), slices: 3,
		pageSize: 1000, timeout: 5 * time.Second, retries: 2}
	if _, err := fetchAll(context.Background(), srv.Client(), srv.URL, "tok", o); err == nil ||
		!strings.Contains(err.Error(), "denied") {
		t.Fatalf("want a 'denied' error, got %v", err)
	}
}
