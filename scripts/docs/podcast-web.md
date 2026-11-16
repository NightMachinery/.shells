# Podcast library

`python/podcast-web` adds a browser for a YouTube podcast channel's full upload
history and playlists. It queues older episodes for download, links existing
audio files, and saves reversible archive choices on the server. Hiding a
downloaded or archived episode affects the list only, not the audio or RSS feed.
The header has Light and Dark themes. Light is the default; the browser remembers
your choice and applies it before rendering the page.

Open **Download queue** near the top to see the current download, waiting episodes
in order, and recent completed/failed jobs. The queue includes episodes hidden
by list filters. Failed jobs have a retry button; finished jobs link to their audio.

Run it in the Python environment that already runs `podcaster.py` (`yt-dlp`
and FFmpeg are required):

```sh
python python/podcast-web/build.py
python python/podcast-web/server.py --channel CHANNEL_ID \
  --audio-dir "$HOME/Downloads/yt/CHANNEL_DIRECTORY" \
  --base-url 'https://files.example.com/yt/CHANNEL_DIRECTORY' \
  --title 'Podcast library'
```

The server binds `127.0.0.1:8812`; `PORT` overrides it. Serve `index.html`,
`app.js`, `theme.js`, and `style.css` behind an authenticated reverse proxy, and strip
the page's `/api` prefix when proxying to this server. JSON mutations require
same-origin browser requests with `X-Podcast-Action: 1`. Authentication belongs
to the private page's proxy, as with other private HTML reports.

Configure the same values through `PODCAST_CHANNEL`, `PODCAST_AUDIO_DIR`,
`PODCAST_BASE_URL`, and `PODCAST_TITLE`. `PODCAST_STATE` defaults to
`~/.local/state/podcast-web`. Give each channel its own state directory.
The SQLite database stores catalog metadata, playlist membership, archives,
and download status outside the served tree. Back it up to preserve archives.

The initial catalog fetch reads the full uploads playlist, without the CLI's
recent-episode limit. Playlist contents load when first opened. Refresh fetches
the selected playlist and the channel's Podcasts and Playlists tabs. Podcast
collections are included even when there is no Playlists tab. You can also add
a playlist using its YouTube URL. Cached catalogs
remain available when YouTube fails. Search supports Persian and English titles.
Filters persist per browser, while archives persist across devices. A downloaded
library view also lists files whose videos are no longer in the channel catalog.
Each list shows its visible and total episode counts. Filter checkboxes include
downloaded/archived counts for the selected playlist, and a summary explains hidden
episodes. “Show all” clears the search and both hide filters for the current view.

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
