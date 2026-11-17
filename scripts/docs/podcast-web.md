# Podcast library

`python/podcast-web` manages a library of YouTube podcast channels, full upload
histories and playlists. It queues older episodes for download, links existing
audio files, and saves reversible archive choices on the server. Hiding a
downloaded or archived episode affects the list only, not the audio or RSS feed.
The header has Light and Dark themes. Light is the default; the browser remembers
your choice and applies it before rendering the page.

Open **Download queue** near the top to see the current download, waiting episodes
in order, and recent completed/failed jobs. The queue includes episodes hidden
by list filters. Failed jobs have a retry button; finished jobs link to their audio.

Use **Install app** in the header to add the library to your home screen or desktop.
It opens in a standalone window with a headphone icon. When the browser cannot
show an installation prompt, the button shows its menu instructions instead.
The manifest is loaded with credentials so a private deployment can stay behind
its normal login. Its identity and scope resolve to the page's own directory.

The app remains online: the service worker provides a generic reconnect page,
and does not cache the private page, episode data, login responses, audio, or
mutations. It does not touch other apps' caches. Normal online navigation always
fetches the current page, whose changing assets have content-hashed URLs.

Run it in the Python environment that already runs `podcaster.py` (`yt-dlp`,
`jdatetime`, Pillow and FFmpeg are required):

```sh
python python/podcast-web/build.py
python python/podcast-web/server.py --channel CHANNEL_ID \
  --audio-dir "$HOME/Downloads/yt/CHANNEL_DIRECTORY" \
  --base-url 'https://files.example.com/yt/CHANNEL_DIRECTORY' \
  --title 'Podcast library'
```

The server binds `127.0.0.1:8812`; `PORT` overrides it. Serve `index.html`,
`app.js`, `theme.js`, `pwa.js`, `style.css`, the manifest, icons, and `sw.js`
behind an authenticated reverse proxy, and strip
the page's `/api` prefix when proxying to this server. JSON mutations require
same-origin browser requests with `X-Podcast-Action: 1`. Authentication belongs
to the private page's proxy, as with other private HTML reports.

Configure the same values through `PODCAST_CHANNEL`, `PODCAST_AUDIO_DIR`,
`PODCAST_BASE_URL`, and `PODCAST_TITLE`. `PODCAST_STATE` defaults to
`~/.local/state/podcast-web`. Give each channel its own state directory.
The SQLite database stores catalog metadata, playlist membership, archives,
and download status outside the served tree. Back it up to preserve archives.

The initial catalog fetch reads the full uploads playlist, without the CLI's
recent-episode limit. Every registered YouTube channel’s public playlists are
discovered and their complete contents cached in the background, including
newly added channels. Two playlist workers keep requests bounded while source
catalogs and audio downloads use separate workers.

Missing or six-hour-old playlist catalogs and contents refresh automatically;
restart resumes unfinished caching. Refresh fetches the selected playlist and
the channel’s Podcasts and Playlists tabs, then caches new collections. Podcast
collections are included even when there is no Playlists tab. Playlists can also
be added by YouTube URL. Each playlist shows its cached episode count, and the
playlist section reports cache progress and failures. Successful caches remain
available when YouTube fails. Search supports Persian and English titles.
Filters persist per browser, while archives persist across devices. A downloaded
library view also lists files whose videos are no longer in the channel catalog.
Each list shows its visible and total episode counts. Filter checkboxes include
downloaded/archived counts for the selected playlist, and a summary explains hidden
episodes. YouTube Shorts are hidden by default using Shorts URLs and a cached channel Shorts tab, never duration guesses. Minimum and maximum lengths use minutes (inclusive, decimals allowed); unknown lengths are excluded while a limit is set. These filters are saved in the browser. “Show all” clears search, length limits and all hide filters. The Shorts index refreshes on startup when missing, every six hours, or with Refresh catalog.

Downloads run one at a time, preserve the existing M4A format, include a video ID
in the filename, update the existing feed atomically, and append to the CLI's
`.downloaded.txt`. Existing files are matched by video ID or normalized title,
using embedded YouTube IDs, audio tags and RSS titles to handle renamed videos
and sanitized filenames. Direct audio links are encoded
URLs to real, nonempty files, rather than guesses based on the download archive.
The shared `.podcaster.lock` serializes feed updates and downloads with the CLI.
Interrupted downloads show a retry action after restart.

`build.py` computes asset hashes automatically. Run it after changing the UI.
The application has no runtime JavaScript dependencies or external UI assets.

Run the integration suite with:

```sh
python -m unittest discover -s python/podcast-web -v
```

It covers archive persistence, catalog refresh failure, download deduplication,
shared locking, existing audio matching, feed updates and browser request checks.

Episode descriptions end with the original upload date in the same named-month
layout as [agfi:now], for example:

```text
Original upload date: 1404/Esfand12/13 Wednesday 4/March3/2026
```

Both the web downloader and CLI use `python/podcast_dates.py`. The web source has
an internal symlink to that helper, which the server deploy dereferences. Include
`podcast_dates.py` in the server's watched files so changing it restarts the service.
The original date comes from YouTube's `upload_date` or the audio's embedded date,
never from its download time. Date-only metadata is printed without an invented
clock time. Unknown dates leave the description unchanged. Rebuilding a feed
replaces the existing suffix rather than duplicating it.

Existing RSS descriptions can be updated without modifying audio, episode GUIDs,
publication dates or feed order:

```sh
python python/podcast-web/backfill_descriptions.py --audio-dir "$AUDIO_DIR"
python python/podcast-web/backfill_descriptions.py --audio-dir "$AUDIO_DIR" --apply
```

The first command previews counts. The second takes the downloader's shared lock,
keeps a `feed.before-upload-dates.rss` backup, and updates the feed atomically.

The PWA manifest and service worker keep stable URLs. `build.py` regenerates the
original headphone artwork at 192, 512, and 180 pixels (Pillow is already a CLI
dependency), hashes the install script, and changes the worker revision when the
app changes. Rendered icon files are gitignored. Running the build twice on
identical inputs changes no files.
The manifest omits `id` so its identity comes from the resolved `start_url`,
keeping separate pages on the same origin distinct.

With Playwright and its Chromium browser installed, verify installability and
login/offline behavior using synthetic data in a temporary browser profile:

```sh
python python/podcast-web/verify_pwa.py
```

The checks include credentialed manifest requests, native prompt handling,
worker scope, no private offline copies, cross-origin login redirects, and
preservation of another app's cache. `--screenshots DIRECTORY` saves phone views.
Manifest setup follows the [web.dev manifest guide](https://web.dev/articles/add-manifest).


Choose a **Podcast** to browse its episodes and playlists. Add a YouTube channel
URL, handle or ID under **Add podcasts and combine feeds**. Each channel has its
own catalog, archives, audio directory and RSS URL; the existing primary channel
keeps its original files and feed URL. The queue spans the whole library and
labels each job with its podcast.

New channels have automatic downloads **off**. Enabling them records a baseline
of known uploads and then checks for new uploads every 30 minutes. Older episodes
remain manual. The original channel retains its enabled setting. The optional
runtime policy `~/.config/podcast-web/automation.json` makes the legacy CLI's
non-interactive channel runs yield to the app, so an old cron job cannot bypass
the toggle. Interactive CLI downloads are unaffected.

Check multiple podcast sources to create a stable combined RSS URL. It includes
downloaded audio from exactly those sources and updates when new audio arrives.
The **global feed** includes every source whose global-feed exclusion is off; the original primary channel is always excluded. Feeds may initially be empty:
loading a catalog does not download audio. RSS metadata, original-date suffixes,
and episode identity are retained when combining feeds.

Without a configured TOML settings file, registry and saved combinations live
in `library.sqlite` beside the primary
`catalog.sqlite`. New catalogs live under `channels/<channel-id>/` in that state
directory; new audio/RSS lives under `channels/<channel-id>/` beside the primary
audio directory. Aggregates live under `feeds/` in that audio base. Back up the
whole state directory to preserve settings, archives and combined selections.

A deployment can move the app without moving audio or changing subscriptions.
`PODCAST_APP_PATH` sets the canonical manifest start URL/scope, and `PODCAST_PWA_ID`
can retain the old resolved identity. Serve the API's `/manifest.webmanifest`
response at the manifest's public URL in that case, and keep old entry URLs as
redirects. Watch `library.py` and the dereferenced `podcast_policy.py` helper when
deploying the server.

### Telegram audio channels

The Add podcast field also accepts public `https://t.me/channel` URLs. Telegram
audio and voice posts have stable message IDs, archives, original upload dates,
manual downloads and individual/combined RSS feeds. Audio keeps its original
format; text, images and non-audio attachments are omitted. Public previews can
load a catalog, but downloading audio and reading complete metadata require a
Telegram **user** session. Bot tokens cannot read channel history.

Set `PODCAST_TELEGRAM_SESSION` to an already-authorized Telethon SQLite session
file on the server. The adapter reads its authorization into memory, without
modifying that file or logging in, joining channels or sending messages. API
credentials come from `TELEGRAM_API_ID` / `TELEGRAM_API_HASH`, or the first two
lines of the existing private `~/.telegram-config`. Keep sessions and credentials
outside repositories and the served tree. The Python environment needs Telethon.

New sources start with automatic downloads off. The **Exclude from global feed**
checkbox controls global membership independently of archives and automation.
The original podcast remains excluded. Explicit combined feeds can include any
checked sources, even those excluded from the global feed.

### Persistent settings and browser preferences

Set `PODCAST_SETTINGS_FILE` to `library.toml` in a dedicated directory outside
the served tree. Global settings and the primary source ID live in that file;
each `podcasts/<source-id>.toml` holds one source’s registration, automation and
global-feed flags, archives and playlist bookmarks. Saved combinations live in
`feeds.toml`. Editing one podcast changes only its own file.

The first startup exports existing SQLite choices. An older combined TOML file
is migrated automatically, replacing its root manifest only after all modular
files have been written. Readers, writers and Git snapshots share a file lock.
Later startups rebuild the SQLite settings mirror and archived flags from the
modular bundle, even after cache loss.
Existing known playlist IDs are preserved on migration; later discoveries are
cache data, while explicitly added playlist bookmarks are saved to TOML.

Set `PODCAST_SETTINGS_SYNC` to a trusted executable command that versions and
pushes the complete managed TOML bundle in one commit. Saves are atomic and serialized. Git runs in a background
worker; failed syncs retain the local TOML and retry after a minute. The UI shows
pending/error status. Do not put credentials or sessions in the settings repo.
For hand edits, stop the app, pull the repo, edit and commit, then restart it.
The app does not automatically pull or overwrite remote changes after conflicts.

The configured state directory holds rebuildable runtime data:

- `library.sqlite` mirrors durable registrations/settings and contains automatic
  download checkpoints (`auto_seen`, baseline and last-check information).
- `catalog.sqlite` holds the primary episode catalog, playlist memberships,
  fetch timestamps, archive mirror and download jobs.
- `channels/<source-id>/catalog.sqlite` holds the same cache/job data for each
  added YouTube or Telegram source. Failed refreshes retain successful caches.

The generated `~/.config/podcast-web/automation.json` lets the legacy CLI yield
to the app; it can be regenerated. Audio files and RSS feeds remain separately
stored data. Losing an automatic-download checkpoint establishes a fresh
baseline instead of downloading the historical catalog again.

Browser local storage holds `podcast-web-preferences` (hide filters, length
limits, playlist, sort and search), `podcast-web-theme`, and `podcast-web-source`
(last selected podcast). These preferences are per browser/origin and do not
sync across devices. Checked sources before creating a combined feed exist only
in page memory; creating the feed saves its membership in TOML.

Back up/version the TOML plus the audio/feed directories and any separately
configured Telegram session. SQLite caches can be rebuilt. Without TOML enabled,
back up the entire state directory using SQLite’s backup API for live databases.
