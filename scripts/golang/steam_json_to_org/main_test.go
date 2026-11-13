package main

import (
	"bytes"
	"strings"
	"testing"
)

func TestSteamFullSample(t *testing.T) {
	input := `{
  "title": ["Factorio [Space Age]"],
  "description_summary": ["Should not appear in properties or quote."],
  "description": ["** About This Game\n\n*Factorio* is a game\\\\\nabout building factories.\u00a0Enjoy!"],
  "releaseDate": ["14 Aug, 2020"],
  "metacriticScore": ["90"],
  "allReviews": ["- 96% of the 150,000 user reviews for this game are positive."],
  "recentReviews": ["- 97% of the 2,000 user reviews in the last 30 days are positive."],
  "tags": ["Automation", "Base Building", "+", "Strategy", ""],
  "developers": ["<<developers_list>>\n[[https://store.steampowered.com/developer/wube][Wube Software LTD.]]"],
  "publisher": ["Wube Software LTD."],
  "platforms": [
    ["platform_img", "win"],
    ["platform_img", "MAC"],
    ["platform_img", "win"],
    ["platform_img", "steamplay"],
    ["platform_img", "linux"],
    ["platform_img"]
  ],
  "capabilities": ["Single-player", "Online Co-op"],
  "minReqs": [":PROPERTIES:\n:ID: min\n:END:\n*Minimum:*\\\\\n- *OS:* Windows 10\n- *RAM:* 8 GB"],
  "recommendedReqs": ["*Recommended:*\n\n- *OS:*\u00a0Windows 11\n*RAM:* 16 GB"],
  "metacriticUrl": ["https://www.metacritic.com/game/factorio"],
  "img": ["https://cdn.example.com/header.jpg"]
}`

	var out bytes.Buffer
	url := "https://store.steampowered.com/app/427520/Factorio/"
	if err := run([]string{url}, strings.NewReader(input), &out); err != nil {
		t.Fatalf("run failed: %v", err)
	}

	want := strings.Join([]string{
		"[[https://store.steampowered.com/app/427520/Factorio/][Factorio {Space Age}]]",
		":PROPERTIES:",
		":releaseDate: 14 Aug, 2020",
		":metacriticScore: 90",
		":allReviews: - 96% of the 150,000 user reviews for this game are positive.",
		":recentReviews: - 97% of the 2,000 user reviews in the last 30 days are positive.",
		":tags: Automation",
		":tags+: Base Building",
		":tags+: Strategy",
		":developers: [[https://store.steampowered.com/developer/wube][Wube Software LTD.]]",
		":publisher: Wube Software LTD.",
		":platforms: win",
		":platforms+: mac",
		":platforms+: linux",
		":capabilities: Single-player",
		":capabilities+: Online Co-op",
		":minReqs: *Minimum:* |> *OS:* Windows 10 |> *RAM:* 8 GB",
		":recommendedReqs: *Recommended:* |> *OS:* Windows 11 |> *RAM:* 16 GB",
		":metacriticUrl: https://www.metacritic.com/game/factorio",
		":img: https://cdn.example.com/header.jpg",
		":visibility: folded",
		":END:",
		"#+BEGIN_QUOTE",
		"*Factorio* is a game",
		"about building factories. Enjoy!",
		"#+END_QUOTE",
		"",
	}, "\n")

	if out.String() != want {
		t.Fatalf("output mismatch:\ngot:\n%s\nwant:\n%s", out.String(), want)
	}
}

func TestSteamEmptyTitleAndDescriptionStripped(t *testing.T) {
	input := `{
  "title": [""],
  "description": ["*** About This Content\n  "],
  "tags": ["+", "   "],
  "platforms": [null, ["only_one"], ["platform_img", "vr"]]
}`
	var out bytes.Buffer
	if err := run([]string{"https://store.steampowered.com/app/1/"}, strings.NewReader(input), &out); err != nil {
		t.Fatalf("run failed: %v", err)
	}
	want := strings.Join([]string{
		"[[https://store.steampowered.com/app/1/]]",
		":PROPERTIES:",
		":visibility: folded",
		":END:",
		"",
	}, "\n")
	if out.String() != want {
		t.Fatalf("got:\n%q\nwant:\n%q", out.String(), want)
	}
}

func TestSteamTitleEscapesBackslashBrackets(t *testing.T) {
	input := `{"title": ["Game \\\\[Deluxe\\\\] [PC]"]}`
	var out bytes.Buffer
	if err := run([]string{"https://store.steampowered.com/app/2/"}, strings.NewReader(input), &out); err != nil {
		t.Fatalf("run failed: %v", err)
	}
	wantHead := "[[https://store.steampowered.com/app/2/][Game {Deluxe} {PC}]]\n"
	if !strings.HasPrefix(out.String(), wantHead) {
		t.Fatalf("got %q, want prefix %q", out.String(), wantHead)
	}
}

func TestSteamDescriptionAboutOnlyStripsLine(t *testing.T) {
	input := `{"title": ["T"], "description": ["Intro\n* About This Game\nBody line"]}`
	var out bytes.Buffer
	if err := run([]string{"https://store.steampowered.com/app/3/"}, strings.NewReader(input), &out); err != nil {
		t.Fatalf("run failed: %v", err)
	}
	wantQuote := "#+BEGIN_QUOTE\nIntro\n\nBody line\n#+END_QUOTE\n"
	if !strings.HasSuffix(out.String(), wantQuote) {
		t.Fatalf("got:\n%q\nwant suffix:\n%q", out.String(), wantQuote)
	}
}

func TestSteamReqsOtherSuffix(t *testing.T) {
	input := `{"title": ["T"], "otherReqs": ["- OS: Linux\n\n-\tCPU: x86_64\\\\"]}`
	// otherReqs is transformed as *Reqs in parseInput, even though not in propertyKeys.
	data, err := parseInput([]byte(input))
	if err != nil {
		t.Fatalf("parseInput failed: %v", err)
	}
	if got, want := data["otherReqs"][0], "- OS: Linux |> CPU: x86_64"; got != want {
		t.Fatalf("got %q, want %q", got, want)
	}
}

func TestSteamNoArgsAndErrors(t *testing.T) {
	var out bytes.Buffer
	if err := run(nil, strings.NewReader(`{"title": ["T"]}`), &out); err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if !strings.HasPrefix(out.String(), "[[][T]]\n") {
		t.Fatalf("got %q", out.String())
	}

	for _, bad := range []string{"", "   ", "null", "[]", `{"title": [123]}`} {
		out.Reset()
		if err := run([]string{"u"}, strings.NewReader(bad), &out); err == nil {
			t.Fatalf("expected error for input %q", bad)
		}
	}
}
