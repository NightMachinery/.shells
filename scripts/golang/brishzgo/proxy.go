package main

import (
	"errors"
	"fmt"
	"net"
	"net/http"
	"net/url"
	"strconv"
	"strings"
)

// The proxy comes from the environment as curl (8) picks it, since
// brishzq.zsh's requests are curl's: Go's http.ProxyFromEnvironment
// ignores ALL_PROXY, which the scripts' pxs sets, honors HTTP_PROXY, which
// curl ignores, and never proxies a loopback address, which curl does.

var errUnsupportedProxy = errors.New("unsupported proxy scheme")

// firstEnv is the first of the variables that is set and not empty; curl
// treats an empty one as unset.
func firstEnv(env lookupEnv, names ...string) string {
	for _, name := range names {
		if v := env.get(name); v != "" {
			return v
		}
	}
	return ""
}

// proxyFromEnv returns the transport's Proxy function for the environment:
//
//   - none when no_proxy (else NO_PROXY) matches the host (see noProxyP);
//   - else <scheme>_proxy: http_proxy alone for http (curl ignores
//     HTTP_PROXY, which a CGI request can set), https_proxy then
//     HTTPS_PROXY for https;
//   - else all_proxy, then ALL_PROXY.
func proxyFromEnv(env lookupEnv) func(*http.Request) (*url.URL, error) {
	return func(req *http.Request) (*url.URL, error) {
		if noProxyP(req.URL.Hostname(), firstEnv(env, "no_proxy", "NO_PROXY")) {
			return nil, nil
		}
		scheme := strings.ToLower(req.URL.Scheme)
		names := []string{scheme + "_proxy"}
		if scheme != "http" {
			names = append(names, strings.ToUpper(scheme)+"_PROXY")
		}
		p := firstEnv(env, names...)
		if p == "" {
			p = firstEnv(env, "all_proxy", "ALL_PROXY")
		}
		if p == "" {
			return nil, nil
		}
		return parseProxy(p)
	}
}

// parseProxy reads a proxy as curl does: http:// when it has no scheme,
// and port 1080 when it has no port (443 for an https:// proxy). Go speaks
// http, https, socks5 and socks5h proxies; curl also socks4 and socks4a.
func parseProxy(p string) (*url.URL, error) {
	if !strings.Contains(p, "://") {
		p = "http://" + p
	}
	u, err := url.Parse(p)
	if err != nil {
		return nil, fmt.Errorf("%w: %v", errUnsupportedProxy, err)
	}
	u.Scheme = strings.ToLower(u.Scheme)
	switch u.Scheme {
	case "http", "https", "socks5", "socks5h":
	default:
		return nil, fmt.Errorf("%w %q", errUnsupportedProxy, u.Scheme)
	}
	if u.Port() == "" {
		port := 1080
		if u.Scheme == "https" {
			port = 443
		}
		u.Host = net.JoinHostPort(u.Hostname(), strconv.Itoa(port))
	}
	return u, nil
}

// noProxyP is curl's Curl_check_noproxy: whether the no_proxy list exempts
// host. `*` alone exempts every host. Otherwise the list is split at
// commas and blanks. For a host name, an entry matches the name itself or
// a domain it is in (`example.com` and `.example.com` both match
// `www.example.com`), ignoring case and a trailing dot. For an IP address,
// an entry matches the same address, or a network in CIDR notation.
func noProxyP(host, noProxy string) bool {
	if host == "" || noProxy == "" {
		return false
	}
	if noProxy == "*" {
		return true
	}
	ip := net.ParseIP(host)
	name := strings.ToLower(strings.TrimSuffix(host, "."))
	for _, tok := range strings.FieldsFunc(noProxy, func(r rune) bool {
		return r == ',' || r == ' ' || r == '\t'
	}) {
		if ip != nil {
			if _, network, err := net.ParseCIDR(tok); err == nil {
				if network.Contains(ip) {
					return true
				}
			} else if tip := net.ParseIP(tok); tip != nil && tip.Equal(ip) {
				return true
			}
			continue
		}
		tok = strings.ToLower(strings.TrimPrefix(strings.TrimSuffix(tok, "."), "."))
		if tok != "" && (name == tok || strings.HasSuffix(name, "."+tok)) {
			return true
		}
	}
	return false
}
