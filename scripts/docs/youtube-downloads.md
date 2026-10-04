# YouTube downloads

[agfi:y] downloads video through the [agfi:youtube-dl] wrapper, which runs
yt-dlp. Its shared [agfi:ybase] options request both manual and automatically
generated English subtitles with `--write-subs --write-auto-subs --sub-langs
'en.*'`. English variants such as `en-orig` are included; other languages are
excluded. When manual and automatic captions share a language tag, yt-dlp uses
the manual track. Distinct English tags can produce multiple subtitle tracks.

[agfi:y] embeds the selected subtitles in the downloaded video. Its existing
`no-keep-subs` compatibility option removes subtitle sidecars after embedding.
Videos with no English captions still download without subtitles.

The same defaults apply to variants built on [agfi:y], including [agfi:ysmall],
[agfi:y1080] and [agfi:ymp4]. To opt out of automatic captions for one download,
use `y --no-write-auto-subs <url>`.

Run [agfi:brishz-restart] after changing the aliases so persistent garden shells
load the new options. Existing terminal shells can source
`$NIGHTDIR/zshlang/auto-load/others/scraping/youtube.zsh`.
