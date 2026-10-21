package paper

import (
	"net/url"
	"regexp"
	"strings"
)

// LatestArxivURL selects the versionless URL without looking up a version number.
// Other hosts, credentials and non-paper paths are deliberately left alone.
func LatestArxivURL(raw string) string {
	u, err := url.Parse(raw)
	if err != nil || (u.Scheme != "https" && u.Scheme != "http") || u.User != nil {
		return raw
	}
	if u.Port() != "" && u.Port() != "80" && u.Port() != "443" {
		return raw
	}
	id, ok := arxivID(raw)
	if !ok {
		return raw
	}
	kind := strings.SplitN(strings.TrimPrefix(u.Path, "/"), "/", 2)[0]
	u.Scheme = "https"
	u.Host = "arxiv.org"
	u.Path = "/" + kind + "/" + id
	u.RawPath = ""
	return u.String()
}

var drawerURLRE = regexp.MustCompile(`https?://[^\s<>\[\]{}"']+`)

// LatestArxivURLs changes only recognized URL tokens, retaining surrounding text.
func LatestArxivURLs(text string) string {
	return drawerURLRE.ReplaceAllStringFunc(text, func(raw string) string {
		candidate := strings.TrimRight(raw, ".,;)")
		return LatestArxivURL(candidate) + raw[len(candidate):]
	})
}

func (m Metadata) normalizeArxivLinks() {
	for _, key := range []string{"arxiv", "links", "pdf_urls", "openAccessPdf"} {
		old := m[key]
		if old == nil {
			continue
		}
		delete(m, key)
		for _, value := range old {
			m.Put(key, LatestArxivURLs(value))
		}
	}
}

func arxivPDF(raw string) bool {
	u, err := url.Parse(raw)
	if err != nil {
		return false
	}
	_, ok := arxivID(raw)
	return ok && strings.HasPrefix(u.Path, "/pdf/")
}
