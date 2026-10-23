package main

import (
	"errors"
	"os"
	"path/filepath"
	"strings"
	"sync"
)

// Only fixed browser IDs cross the API. Clients cannot supply filesystem paths.
// Unused browsers do no history I/O and have no index goroutine.
type source struct {
	root, kind string
	index      *index
}
type sourceSet struct {
	sync.Mutex
	sources map[string]*source
	quit    <-chan os.Signal
	parent  int
}

func newSources(home, braveRoot string, brave *index, quit <-chan os.Signal, parent int) *sourceSet {
	support := filepath.Join(home, "Library", "Application Support")
	return &sourceSet{sources: map[string]*source{
		"brave":   {braveRoot, "chromium", brave},
		"chrome":  {filepath.Join(support, "Google", "Chrome"), "chromium", nil},
		"edge":    {filepath.Join(support, "Microsoft Edge"), "chromium", nil},
		"arc":     {filepath.Join(support, "Arc", "User Data"), "chromium", nil},
		"dia":     {filepath.Join(support, "Dia", "User Data"), "chromium", nil},
		"safari":  {filepath.Join(home, "Library", "Safari"), "safari", nil},
		"firefox": {filepath.Join(support, "Firefox"), "firefox", nil},
	}, quit: quit, parent: parent}
}
func (sources *sourceSet) get(browser string) (*index, error) {
	if browser == "" {
		browser = "brave"
	}
	sources.Lock()
	defer sources.Unlock()
	selected := sources.sources[browser]
	if selected == nil {
		return nil, errors.New("unsupported browser")
	}
	if selected.index == nil {
		selected.index = &index{}
		go selected.index.refreshKind(selected.root, selected.kind, sources.quit, sources.parent)
	}
	return selected.index, nil
}

// Firefox records its preferred profile in profiles.ini. Resolve only paths
// beneath its own root; an unavailable/externally located profile fails safely.
func firefoxProfile(root string) string {
	raw, err := os.ReadFile(filepath.Join(root, "profiles.ini"))
	if err != nil || len(raw) > 1<<20 {
		return filepath.Join(root, "unavailable-profile")
	}
	type profile struct {
		path                string
		relative, preferred bool
	}
	profiles := []profile{}
	section := ""
	current := profile{relative: true}
	installed := ""
	flush := func() {
		if strings.HasPrefix(section, "Profile") && current.path != "" {
			profiles = append(profiles, current)
		}
	}
	for _, line := range strings.Split(string(raw), "\n") {
		line = strings.TrimSpace(line)
		if strings.HasPrefix(line, "[") && strings.HasSuffix(line, "]") {
			flush()
			section = strings.Trim(line, "[]")
			current = profile{relative: true}
			continue
		}
		key, value, ok := strings.Cut(line, "=")
		if !ok {
			continue
		}
		if strings.HasPrefix(section, "Install") && key == "Default" {
			installed = value
		}
		if strings.HasPrefix(section, "Profile") {
			switch key {
			case "Path":
				current.path = value
			case "IsRelative":
				current.relative = value == "1"
			case "Default":
				current.preferred = value == "1"
			}
		}
	}
	flush()
	for pass := 0; pass < 3; pass++ {
		for _, p := range profiles {
			if !p.relative || p.path == "" || filepath.IsAbs(p.path) {
				continue
			}
			clean := filepath.Clean(p.path)
			if clean == ".." || strings.HasPrefix(clean, ".."+string(filepath.Separator)) {
				continue
			}
			if (pass == 0 && p.path == installed) || (pass == 1 && p.preferred) || pass == 2 {
				return filepath.Join(root, clean)
			}
		}
	}
	return filepath.Join(root, "unavailable-profile")
}
