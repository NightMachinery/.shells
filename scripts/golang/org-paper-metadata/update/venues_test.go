package update

import (
	"strings"
	"testing"
)

func TestOldLongVenueTagIsReplacedAndPersonalMiddleTagsSurvive(t *testing.T) {
	text := "* @citations/18 @2021/December @someTag @EuropeanConferenceOnArtificialIntelligence [[" + page + "][Old]] :reading:\n:PROPERTIES:\n:venue: European Conference on Artificial Intelligence\n:ID: keep-me\n:END:\nMy private notes.\n"
	d, _ := Parse(text)
	targets, _ := d.Targets()
	m := fresh()
	m["venue"] = []string{"European Conference on Artificial Intelligence"}
	out, err := d.Apply(d.Plan(targets[0], m))
	if err != nil || strings.Contains(out, "@EuropeanConferenceOnArtificialIntelligence") || strings.Count(out, "@ECAI") != 1 || !strings.Contains(out, "@someTag") || !strings.Contains(out, ":reading:") || !strings.Contains(out, "My private notes.") || !strings.Contains(out, ":ID: keep-me") {
		t.Fatal(out, err)
	}
	if again := transform(t, out); !strings.Contains(again, "@someTag") {
		t.Fatal(again)
	}
}
