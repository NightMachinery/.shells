package update

import (
	"strings"
	"testing"
)

func TestRefreshRemovesOldVisibleSummaryPrefix(t *testing.T) {
	out := transform(t, "* [["+page+"][Old]]\n")
	out = strings.Replace(out, "The new summary.", "*Semantic Scholar TLDR:* The old summary.", 1)
	out = transform(t, out)
	if strings.Contains(out, "*Semantic Scholar TLDR:*") || !strings.Contains(out, "# org-paper-metadata: summary\n#+BEGIN_QUOTE\nThe new summary.\n#+END_QUOTE") {
		t.Fatal(out)
	}
}
