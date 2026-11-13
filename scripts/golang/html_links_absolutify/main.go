package main

import (
	"bytes"
	"fmt"
	"io"
	"os"
	"os/exec"
	"regexp"
	"strings"

	"golang.org/x/net/html"
	"golang.org/x/net/html/atom"
)

var (
	fullDocRe      = regexp.MustCompile(`(?i)<(?:!doctype|html|head|body)(?:[\s>/]|$)`)
	schemePrefixRe = regexp.MustCompile(`^([^/]+:)//`)
)

type options struct {
	urlCurrent  string
	urlRoot     string
	urlFinalP   bool
	absolutifyP bool
	resolveURL  func(string) string
}

func boolsh(s string) bool {
	switch strings.ToLower(s) {
	case "", "n", "no", "0":
		return false
	default:
		return true
	}
}

func boolshEnvY(getenv func(string) string, name string) bool {
	v := getenv(name)
	if v == "" {
		return true
	}
	return boolsh(v)
}

func filterEnv(environ []string, dropKey string) []string {
	prefix := dropKey + "="
	out := make([]string, 0, len(environ))
	for _, entry := range environ {
		if !strings.HasPrefix(entry, prefix) {
			out = append(out, entry)
		}
	}
	return out
}

func resolveURLFinalViaBrish(rawURL string) string {
	var cmd *exec.Cmd
	if bin, err := exec.LookPath("brishzgo"); err == nil {
		cmd = exec.Command(bin, "--", "reval-memoi", "urlfinalg", rawURL)
	} else if bin, err := exec.LookPath("brishzq.zsh"); err == nil {
		cmd = exec.Command(bin, "reval-memoi", "urlfinalg", rawURL)
	} else {
		return ""
	}
	cmd.Env = filterEnv(os.Environ(), "brishz_in")
	out, err := cmd.Output()
	if err != nil {
		return ""
	}
	return strings.TrimSpace(string(out))
}

func walkElements(n *html.Node, fn func(*html.Node)) {
	if n.Type == html.ElementNode {
		fn(n)
	}
	for c := n.FirstChild; c != nil; c = c.NextSibling {
		walkElements(c, fn)
	}
}

func findAttr(n *html.Node, key string) (int, string, bool) {
	for i, a := range n.Attr {
		if a.Key == key {
			return i, a.Val, true
		}
	}
	return -1, "", false
}

func absolutifyURL(urlRel, urlCurrent, urlRoot string) string {
	if strings.HasPrefix(urlRel, "//") {
		if m := schemePrefixRe.FindStringSubmatch(urlRoot); len(m) >= 2 {
			return m[1] + urlRel
		}
		return urlRel
	}
	if strings.HasPrefix(urlRel, "/") {
		return urlRoot + urlRel
	}
	return urlCurrent + "/" + urlRel
}

func transformNodes(nodes []*html.Node, opts options) {
	resolve := opts.resolveURL
	if resolve == nil {
		resolve = resolveURLFinalViaBrish
	}
	for _, attrName := range []string{"src", "href"} {
		if opts.urlFinalP && strings.EqualFold(attrName, "href") {
			for _, root := range nodes {
				walkElements(root, func(el *html.Node) {
					idx, val, ok := findAttr(el, attrName)
					if !ok || !strings.HasPrefix(val, "http") {
						return
					}
					if resolved := resolve(val); resolved != "" {
						el.Attr[idx].Val = resolved
					}
				})
			}
		}
		if opts.absolutifyP {
			for _, root := range nodes {
				walkElements(root, func(el *html.Node) {
					idx, val, ok := findAttr(el, attrName)
					if !ok || strings.HasPrefix(val, "http") {
						return
					}
					el.Attr[idx].Val = absolutifyURL(val, opts.urlCurrent, opts.urlRoot)
				})
			}
		}
	}
}

func processHTML(in string, opts options) (string, error) {
	var nodes []*html.Node
	if fullDocRe.MatchString(in) {
		doc, err := html.Parse(strings.NewReader(in))
		if err != nil {
			return "", err
		}
		nodes = []*html.Node{doc}
	} else {
		bodyCtx := &html.Node{Type: html.ElementNode, Data: "body", DataAtom: atom.Body}
		frag, err := html.ParseFragment(strings.NewReader(in), bodyCtx)
		if err != nil {
			return "", err
		}
		nodes = frag
	}

	transformNodes(nodes, opts)

	var buf bytes.Buffer
	for _, n := range nodes {
		if err := html.Render(&buf, n); err != nil {
			return "", err
		}
	}
	return buf.String(), nil
}

func run(args []string, getenv func(string) string, stdin io.Reader, stdout io.Writer, resolveURL func(string) string) error {
	urlFinalP := boolsh(getenv("html_links_url_final_p"))
	absolutifyP := boolshEnvY(getenv, "html_links_absolutify_p")

	if absolutifyP && len(args) == 0 {
		return fmt.Errorf("html_links_absolutify: missing url_current argument")
	}

	urlCurrent := ""
	if len(args) >= 1 {
		urlCurrent = args[0]
	}
	urlRoot := urlCurrent
	if len(args) >= 2 {
		urlRoot = args[1]
	}
	urlRoot = strings.TrimRight(urlRoot, "/")
	urlCurrent = strings.TrimRight(urlCurrent, "/")

	rawIn, err := io.ReadAll(stdin)
	if err != nil {
		return err
	}

	out, err := processHTML(string(rawIn), options{
		urlCurrent:  urlCurrent,
		urlRoot:     urlRoot,
		urlFinalP:   urlFinalP,
		absolutifyP: absolutifyP,
		resolveURL:  resolveURL,
	})
	if err != nil {
		return err
	}
	_, err = fmt.Fprintln(stdout, out)
	return err
}

func main() {
	if err := run(os.Args[1:], os.Getenv, os.Stdin, os.Stdout, resolveURLFinalViaBrish); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
}
