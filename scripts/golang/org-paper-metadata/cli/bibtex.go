package cli

import (
	"flag"
	"strconv"
)

// Both switches accept normal boolean values; the last supplied switch wins.
func bibTeXFlags(flags *flag.FlagSet) *bool {
	enabled := new(bool)
	*enabled = true
	flags.BoolFunc("bibtex", "include a folded BibTeX section (default: enabled)", func(v string) error {
		b, e := strconv.ParseBool(v)
		if e == nil {
			*enabled = b
		}
		return e
	})
	flags.BoolFunc("no-bibtex", "omit BibTeX; updater removes only marked generated sections", func(v string) error {
		b, e := strconv.ParseBool(v)
		if e == nil {
			*enabled = !b
		}
		return e
	})
	return enabled
}
