package main

import (
	"bytes"
	"strings"
	"testing"
)

func testEnv(m map[string]string) func(string) string {
	return func(k string) string {
		if m == nil {
			return ""
		}
		return m[k]
	}
}

func TestBoolshAndBoolshEnvY(t *testing.T) {
	for _, falseVal := range []string{"", "n", "N", "no", "NO", "No", "0"} {
		if boolsh(falseVal) {
			t.Errorf("boolsh(%q) = true, want false", falseVal)
		}
	}
	for _, trueVal := range []string{"y", "Y", "yes", "1", "true", "anything"} {
		if !boolsh(trueVal) {
			t.Errorf("boolsh(%q) = false, want true", trueVal)
		}
	}

	for _, emptyOrTrue := range []string{"", "y", "yes", "1", "true"} {
		if !boolshEnvY(testEnv(map[string]string{"k": emptyOrTrue}), "k") {
			t.Errorf("boolshEnvY(%q) = false, want true", emptyOrTrue)
		}
	}
	if !boolshEnvY(testEnv(nil), "missing") {
		t.Errorf("boolshEnvY(unset) = false, want true")
	}
	for _, falseVal := range []string{"n", "N", "no", "NO", "0"} {
		if boolshEnvY(testEnv(map[string]string{"k": falseVal}), "k") {
			t.Errorf("boolshEnvY(%q) = true, want false", falseVal)
		}
	}
}

func TestReplModeSampleParity(t *testing.T) {
	in := `<p><a href="https://bing.com">bing!</a>
<b><a href='/posts/internal/1.html'>mm</a></b>
<img src='repo/a.png' />
<img src='//example.com/b.png' />
</p><a href="https://google.com">gingin</a>`

	var out bytes.Buffer
	err := run(
		[]string{"http://manpages.ubuntu.com/sth/", "http://manpages.ubuntu.com///"},
		testEnv(nil),
		strings.NewReader(in),
		&out,
		nil,
	)
	if err != nil {
		t.Fatalf("run failed: %v", err)
	}

	want := `<p><a href="https://bing.com">bing!</a>
<b><a href="http://manpages.ubuntu.com/posts/internal/1.html">mm</a></b>
<img src="http://manpages.ubuntu.com/sth/repo/a.png"/>
<img src="http://example.com/b.png"/>
</p><a href="https://google.com">gingin</a>` + "\n"

	if out.String() != want {
		t.Fatalf("got:\n%s\nwant:\n%s", out.String(), want)
	}
}

func TestUrlRootDefaultsToUrlCurrent(t *testing.T) {
	in := `<a href="/a">a</a><a href="b/c">b</a><a href="//cdn.test/d">c</a>`
	var out bytes.Buffer
	err := run(
		[]string{"https://example.com/dir///"},
		testEnv(nil),
		strings.NewReader(in),
		&out,
		nil,
	)
	if err != nil {
		t.Fatalf("run failed: %v", err)
	}

	want := `<a href="https://example.com/dir/a">a</a><a href="https://example.com/dir/b/c">b</a><a href="https://cdn.test/d">c</a>` + "\n"
	if out.String() != want {
		t.Fatalf("got %q, want %q", out.String(), want)
	}
}

func TestAllNonHTTPAttributesMatched(t *testing.T) {
	// Matches `[attr_name]:not([attr_name^="http"])` across arbitrary elements.
	in := `<link href="/s.css"/><script src="app.js"></script><a href="#sec">sec</a><a href="mailto:x@y">mail</a><a href="">empty</a>`
	var out bytes.Buffer
	err := run(
		[]string{"https://example.com/p/", "https://example.com"},
		testEnv(nil),
		strings.NewReader(in),
		&out,
		nil,
	)
	if err != nil {
		t.Fatalf("run failed: %v", err)
	}

	want := `<link href="https://example.com/s.css"/><script src="https://example.com/p/app.js"></script><a href="https://example.com/p/#sec">sec</a><a href="https://example.com/p/mailto:x@y">mail</a><a href="https://example.com/p/">empty</a>` + "\n"
	if out.String() != want {
		t.Fatalf("got %q, want %q", out.String(), want)
	}
}

func TestAbsolutifyDisabled(t *testing.T) {
	in := `<a href="/rel">rel</a><img src="img.png"/>`
	for _, offVal := range []string{"n", "no", "0", "N"} {
		var out bytes.Buffer
		err := run(
			nil,
			testEnv(map[string]string{"html_links_absolutify_p": offVal}),
			strings.NewReader(in),
			&out,
			nil,
		)
		if err != nil {
			t.Fatalf("run(%q) failed: %v", offVal, err)
		}
		want := `<a href="/rel">rel</a><img src="img.png"/>` + "\n"
		if out.String() != want {
			t.Fatalf("offVal=%q: got %q, want %q", offVal, out.String(), want)
		}
	}
}

func TestURLFinalResolution(t *testing.T) {
	in := `<a href="https://t.co/abc">short</a><a href="http://empty.example">empty</a><a href="/local">loc</a><img src="https://img.example/1.png"/>`
	var calls []string
	resolver := func(u string) string {
		calls = append(calls, u)
		if u == "https://t.co/abc" {
			return "https://target.example/article"
		}
		return ""
	}

	var out bytes.Buffer
	err := run(
		[]string{"https://dummy_url.com"},
		testEnv(map[string]string{
			"html_links_url_final_p":  "y",
			"html_links_absolutify_p": "n",
		}),
		strings.NewReader(in),
		&out,
		resolver,
	)
	if err != nil {
		t.Fatalf("run failed: %v", err)
	}

	if len(calls) != 2 || calls[0] != "https://t.co/abc" || calls[1] != "http://empty.example" {
		t.Fatalf("unexpected resolver calls: %#v", calls)
	}
	want := `<a href="https://target.example/article">short</a><a href="http://empty.example">empty</a><a href="/local">loc</a><img src="https://img.example/1.png"/>` + "\n"
	if out.String() != want {
		t.Fatalf("got %q, want %q", out.String(), want)
	}
}

func TestFullDocumentPreserved(t *testing.T) {
	in := `<!DOCTYPE html><html><head><link href="/a.css"/></head><body><a href="b.html">b</a></body></html>`
	var out bytes.Buffer
	err := run(
		[]string{"https://example.com/sub/", "https://example.com/"},
		testEnv(nil),
		strings.NewReader(in),
		&out,
		nil,
	)
	if err != nil {
		t.Fatalf("run failed: %v", err)
	}
	want := `<!DOCTYPE html><html><head><link href="https://example.com/a.css"/></head><body><a href="https://example.com/sub/b.html">b</a></body></html>` + "\n"
	if out.String() != want {
		t.Fatalf("got %q, want %q", out.String(), want)
	}
}

func TestProtocolRelativeWithoutSchemeInRoot(t *testing.T) {
	in := `<img src="//cdn.example.com/a.png"/>`
	var out bytes.Buffer
	err := run(
		[]string{"example.com"},
		testEnv(nil),
		strings.NewReader(in),
		&out,
		nil,
	)
	if err != nil {
		t.Fatalf("run failed: %v", err)
	}
	want := `<img src="//cdn.example.com/a.png"/>` + "\n"
	if out.String() != want {
		t.Fatalf("got %q, want %q", out.String(), want)
	}
}

func TestMissingArgsErrorsWhenAbsolutifyEnabled(t *testing.T) {
	var out bytes.Buffer
	err := run(nil, testEnv(nil), strings.NewReader(`<a href="/a">a</a>`), &out, nil)
	if err == nil {
		t.Fatalf("expected error when args empty and absolutify enabled")
	}
}

func TestEmptyInput(t *testing.T) {
	var out bytes.Buffer
	err := run([]string{"https://example.com"}, testEnv(nil), strings.NewReader(""), &out, nil)
	if err != nil {
		t.Fatalf("run failed: %v", err)
	}
	if out.String() != "\n" {
		t.Fatalf("got %q, want %q", out.String(), "\n")
	}
}

func TestFilterEnvDropsOnlyTargetKey(t *testing.T) {
	env := []string{"PATH=/bin", "brishz_in=MAGIC_READ_STDIN", "brishz_in_extra=1"}
	got := filterEnv(env, "brishz_in")
	if len(got) != 2 || got[0] != "PATH=/bin" || got[1] != "brishz_in_extra=1" {
		t.Fatalf("filterEnv got %#v", got)
	}
}
