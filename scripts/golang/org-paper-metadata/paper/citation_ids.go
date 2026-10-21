package paper

import (
	"net/url"
	"regexp"
	"strings"
	"unicode"
)

var doiIDRE = regexp.MustCompile(`(?i)^10\.[0-9]{4,9}/.+$`)
var dblpKeyRE = regexp.MustCompile(`^[A-Za-z0-9_-]+(?:/[A-Za-z0-9_.:-]+)+$`)

func normalizeDOI(value string) string {
	value = strings.TrimSpace(value)
	if strings.Contains(value, "://") {
		return normalizeDOIURL(value)
	}
	if strings.HasPrefix(strings.ToLower(value), "doi:") {
		value = strings.TrimSpace(value[4:])
	}
	if !doiIDRE.MatchString(value) || strings.ContainsFunc(value, func(r rune) bool { return unicode.IsSpace(r) || unicode.IsControl(r) }) {
		return ""
	}
	return strings.ToLower(value)
}
func normalizeDOIURL(value string) string {
	u, e := url.Parse(value)
	if e != nil || (u.Scheme != "https" && u.Scheme != "http") || u.User != nil || u.Port() != "" || (strings.ToLower(u.Hostname()) != "doi.org" && strings.ToLower(u.Hostname()) != "dx.doi.org") {
		return ""
	}
	return normalizeDOI(strings.TrimPrefix(u.Path, "/"))
}
func normalizeDBLP(value string) string {
	value = strings.TrimSpace(value)
	if strings.Contains(value, "://") {
		return normalizeDBLPURL(value)
	}
	value = strings.TrimPrefix(value, "DBLP:")
	if !dblpKeyRE.MatchString(value) {
		return ""
	}
	for _, part := range strings.Split(value, "/") {
		if part == "." || part == ".." {
			return ""
		}
	}
	return value
}
func normalizeDBLPURL(value string) string {
	u, e := url.Parse(value)
	if e != nil || (u.Scheme != "https" && u.Scheme != "http") || u.User != nil || u.Port() != "" {
		return ""
	}
	switch strings.ToLower(u.Hostname()) {
	case "dblp.org", "dblp.uni-trier.de", "dblp.dagstuhl.de":
	default:
		return ""
	}
	if !strings.HasPrefix(u.Path, "/rec/") {
		return ""
	}
	key := strings.TrimPrefix(u.Path, "/rec/")
	for _, suffix := range []string{".html", ".bib", ".xml"} {
		key = strings.TrimSuffix(key, suffix)
	}
	return normalizeDBLP(key)
}
func (m Metadata) addExternalIDs(ids map[string]any) {
	if doi := normalizeDOI(str(ids["DOI"])); doi != "" {
		m["doi"] = []string{doi}
	}
	if key := normalizeDBLP(str(ids["DBLP"])); key != "" {
		m["dblp"] = []string{key}
	}
}
