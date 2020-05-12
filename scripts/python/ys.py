#!/usr/bin/env python3
import subprocess, sys
import youtube_dl


player_run = False
p = None


def hook(status):
    global player_run, p
    if not player_run and status['downloaded_bytes'] >= 10^6:
        print(status)
        print(f"Running mpv for {status['filename']} ...")
        player_run = True
        p = subprocess.Popen(['mpv', '--script-opts-add=autoload-disabled=yes', status['filename']])


opts = {
    'format': 'best',
    'nopart': True,
    'progress_hooks': [hook],
    # 'verbose': True,
    # https://github.com/ytdl-org/youtube-dl/issues/25250
    # our best bet is to use an aria2c hook
    # 'external_downloader': 'aria2c',
    'noplaylist': True,
    'writesubtitles': True,
}

with youtube_dl.YoutubeDL(opts) as ydl:
    ydl.download([sys.argv[1]])

p.wait()
