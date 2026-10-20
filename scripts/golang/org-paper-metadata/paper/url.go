package paper

import (
	"fmt"
	"net/url"
	"regexp"
	"strings"
)

var paperPath = regexp.MustCompile(`^/paper/(?:[^/?#]+/)?([a-fA-F0-9]{40})/?$`)
var idPattern = regexp.MustCompile(`^[a-fA-F0-9]{40}$`)
var arxivPattern = regexp.MustCompile(`^(?:\d{4}\.\d{4,5}|[a-zA-Z.-]+/\d{7})(?:v\d+)?$`)
var versionPattern = regexp.MustCompile(`v\d+$`)
var aliasPattern = regexp.MustCompile(`(?i)^/(arxiv|corpusid|acl|doi):(.+)$`)

func webURL(raw string) bool {
	u, err := url.Parse(raw)
	return err == nil && (u.Scheme == "http" || u.Scheme == "https") && u.Hostname() != "" && u.User == nil
}
func scholarHost(host string) bool {
	return host == "www.semanticscholar.org" || host == "semanticscholar.org"
}
func PaperID(raw string) string {
	u, err := url.Parse(raw)
	if err != nil || !scholarHost(strings.ToLower(u.Hostname())) {
		return ""
	}
	if m := paperPath.FindStringSubmatch(u.Path); m != nil {
		return strings.ToLower(m[1])
	}
	return ""
}
func arxivID(raw string) (string, bool) {
	value := raw
	if u, err := url.Parse(raw); err == nil && u.Host != "" {
		host := strings.ToLower(u.Hostname())
		if host != "arxiv.org" && host != "www.arxiv.org" && host != "export.arxiv.org" {
			return "", false
		}
		value = strings.TrimPrefix(u.Path, "/")
		parts := strings.SplitN(value, "/", 2)
		if len(parts) != 2 || (parts[0] != "abs" && parts[0] != "pdf" && parts[0] != "html") {
			return "", false
		}
		value = strings.TrimSuffix(parts[1], ".pdf")
	}
	if !arxivPattern.MatchString(value) {
		return "", false
	}
	return versionPattern.ReplaceAllString(value, ""), true
}

// Lookup returns a canonical page or an external-ID URL whose HEAD redirect identifies it.
// It never searches titles or asks a language model to identify a paper.
func Lookup(input string) (string, error) {
	input = strings.TrimSpace(input)
	if idPattern.MatchString(input) {
		return "https://www.semanticscholar.org/paper/" + strings.ToLower(input), nil
	}
	if id, ok := arxivID(input); ok {
		return "https://api.semanticscholar.org/arXiv:" + id, nil
	}
	u, err := url.Parse(input)
	if err != nil || u.User != nil || (u.Scheme != "https" && u.Scheme != "http") {
		return "", fmt.Errorf("unsupported paper URL: %s", input)
	}
	if id := PaperID(input); id != "" {
		return "https://www.semanticscholar.org/paper/" + id, nil
	}
	if strings.EqualFold(u.Hostname(), "api.semanticscholar.org") && aliasPattern.MatchString(u.Path) {
		u.Scheme = "https"
		u.RawQuery = ""
		u.Fragment = ""
		if strings.HasPrefix(strings.ToLower(u.Path), "/arxiv:") {
			u.Path = versionPattern.ReplaceAllString(u.Path, "")
		}
		return u.String(), nil
	}
	return "", fmt.Errorf("unsupported paper URL: %s", input)
}

// Identity is used only to deduplicate recognized links, never to search for papers.
func Identity(input string) (string, bool) { s, e := Lookup(input); return s, e == nil }
