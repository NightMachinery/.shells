# browsers

Reading the front tab of a Chromium-family browser — its URL, title, HTML, or
every open tab — and opening URLs into one.

Everything runs through `chrome-cli`, which drives Chrome's AppleScript
dictionary. The browser it talks to is chosen by the `CHROME_BUNDLE_IDENTIFIER`
environment variable, so a single set of generic functions covers every
Chromium browser and picking one is just a matter of setting that variable.
macOS only. The code is `zshlang/auto-load/others/browsers/chrome, gen.zsh`.

## The generic layer

These act on whatever browser is currently selected:

    browser-current-url             # front tab's URL, cleaned through unalix
    browser-current-title
    browser-current-html            # source, with relative links absolutified
    browser-current-links           # every URL in the page, deduplicated
    browser-all-urls                # every open tab
    browser-open URL...             # no arguments means the clipboard
    browser-open-file FILE
    browser-open-pdf FILE

`h-browser-current-url` is the raw form of `browser-current-url`, without the
tracker stripping.

## Selecting a browser

`with-<browser>` runs a command against a specific browser, via
[agfi:reval-env]:

    with-brave browser-current-url
    with-chrome browser-all-urls

The wrappers are `with-chrome`, `with-brave`, `with-vivaldi`, `with-arc` and
`with-edge`, holding the bundle IDs `com.google.Chrome`, `com.brave.Browser`,
`com.vivaldi.Vivaldi`, `company.thebrowser.Browser` and `com.microsoft.edgemac`
respectively.

Each also gets pre-composed aliases, so the common calls are one word:

    vivaldi-current-url
    brave-all-urls
    chrome-current-title
    arc-current-links
    brave-open URL...

Every browser but Edge has the full set of six (`-current-html`,
`-current-links`, `-current-url`, `-all-urls`, `-current-title`, and
`org-link-<browser>-current`); Edge is missing `-current-html` and
`-current-links`.

## The default browser

With no `CHROME_BUNDLE_IDENTIFIER` set, everything falls back to
`browser_default_bundle_id`, declared at the top of `chrome, gen.zsh` and
currently Vivaldi. That variable is the one place to change to switch default
browsers — do not hardcode a bundle ID anywhere else.

It is read in two places, which must never disagree: the `chrome-cli` wrapper,
and [agfi:browser-focus-p] in `luna-bells.zsh`. The latter compares the
frontmost application against it, and feeds [agfi:llm-focus-p] in `stt.zsh`,
which decides whether dictated speech gets tagged for a web LLM. A default that
drifted between the two would mean reading one browser while watching another.

## Emacs

`night/org-link-browser-current` in `~/doom.d/autoload/night-zsh-wrappers.el`
inserts an org link to the front tab, bound to `SPC l ;`. It takes an optional
browser name and dispatches to the matching `org-link-<browser>-current` zsh
alias; `night/org-link-brave-current`, `-vivaldi-`, `-chrome-`, `-edge-` and
`-arc-` are the per-browser entry points. Only chrome (`lc`) and edge (`le`)
have keybindings; the rest are `M-x` only. Safari is separate and goes through
AppleScript via `org-mac-link`.

## Caveats

Run `brishz-restart` after editing any of this. BrishGarden keeps persistent
zsh shells and will otherwise keep running the old definitions, which affects
the Emacs bindings, Hammerspoon, and the agent hooks.

AppleScript coverage varies by browser. Chrome and Brave implement the
dictionary faithfully; the others are less complete, so an empty result from
`browser-current-url` is more likely a browser limitation than a bug here.
Check by running the same call under `with-chrome`.
