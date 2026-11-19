package main

import (
	"crypto/sha256"
	"embed"
	"encoding/hex"
	"fmt"
	"io/fs"
	"strings"
)

// The program carries its own source, so `daily_sync version' names the
// source it was built from on any platform: a cross-built copy on another
// machine prints the same hash as the local build. h-daily-sync-engine names
// the shipped binaries by it.
//
//go:embed *.go go.mod
var sourceFS embed.FS

// sourceHash is a hash of the module's go.mod and non-test .go files: the
// files go-local-dep watches.
func sourceHash() string {
	ents, err := fs.ReadDir(sourceFS, ".")
	if err != nil {
		return "unknown"
	}
	h := sha256.New()
	for _, e := range ents { // sorted by name
		n := e.Name()
		if strings.HasSuffix(n, "_test.go") {
			continue
		}
		b, err := sourceFS.ReadFile(n)
		if err != nil {
			return "unknown"
		}
		fmt.Fprintf(h, "%s\x00%d\x00", n, len(b))
		h.Write(b)
	}
	return hex.EncodeToString(h.Sum(nil))[:16]
}
