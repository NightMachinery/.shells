#!/usr/bin/env python3

"""
podcaster.py - Podcast RSS Feed Generator

Creates standardized podcast RSS feeds from audio files or YouTube channels,
automatically handling metadata and artwork.

Example usage:
    # Generate podcast from local audio files
    python podcaster.py local audiobook.m4b --base-url "https://files.example.com/" --base-dir "~/Downloads" --image cover.jpg

    # Generate podcast from a YouTube channel
    python podcaster.py yt UC_x5XG1OV2P6uZZ5FSM9Ttw --base-url "https://files.example.com/" --base-dir "~/Podcasts" --out-dir "Google Developers" -n 50

Dependencies:
    - mutagen
    - Pillow
    - feedgen
    - yt-dlp
    - iterfzf
    - tqdm
    - pynight (custom module)

Install dependencies with:
    pip install -U feedgen Pillow mutagen yt-dlp iterfzf tqdm
"""

import os
import sys
import argparse
import logging
import datetime
from typing import List, Optional, Dict, Any, Iterable
import email.utils
import re

from mutagen import File as MutagenFile
from mutagen.id3 import ID3, APIC
from mutagen.mp4 import MP4Cover, MP4
from PIL import Image
from feedgen.feed import FeedGenerator
from yt_dlp import YoutubeDL
from tqdm import tqdm
from pynight.common_fzf import rtl_iterfzf

from pynight.common_files import list_children
from pynight.common_sort import version_sort_key

# Shared regex pattern
YOUTUBE_CHANNEL_ID_PATTERN = r"^UC[a-zA-Z0-9_-]{22}$"
YOUTUBE_CHANNEL_URL_PATTERN = r"channel/(UC[a-zA-Z0-9_-]{22})"


def parse_arguments() -> argparse.Namespace:
    """Parse command-line arguments.

    Returns:
        argparse.Namespace: Parsed arguments.
    """
    parser = argparse.ArgumentParser(description="Podcast RSS Feed Generator")
    parser.add_argument(
        "--log-file",
        type=str,
        help="Path to a file to output logs.",
    )
    subparsers = parser.add_subparsers(dest="command", required=True)

    # Define shared arguments
    def add_shared_arguments(parser):
        """Add arguments that are common to both local and YouTube parsers."""
        parser.add_argument(
            "--base-url",
            type=str,
            required=True,
            help="Base URL where the audio files and images are hosted.",
        )
        parser.add_argument(
            "--base-dir",
            type=str,
            help="Base directory for file operations.",
        )
        parser.add_argument(
            "-f",
            "--overwrite",
            action=argparse.BooleanOptionalAction,
            help="Allow overwriting existing files.",
        )
        parser.add_argument(
            "-v",
            "--verbose",
            action=argparse.BooleanOptionalAction,
            help="Enable detailed logging output.",
        )
        parser.add_argument(
            "--fake-dates",
            action=argparse.BooleanOptionalAction,
            help="Generate fake dates to maintain episode order in podcast players.",
        )
        parser.add_argument(
            "--title",
            type=str,
            help="Override podcast title.",
        )
        parser.add_argument(
            "--description",
            type=str,
            help="Set podcast description.",
        )
        parser.add_argument(
            "--author",
            type=str,
            help="Set podcast author.",
        )
        parser.add_argument(
            "--link",
            type=str,
            help="Podcast main link URL.",
        )
        parser.add_argument(
            "--tqdm",
            action=argparse.BooleanOptionalAction,
            default=True,
            help="Use tqdm for download progress.",
        )
        parser.add_argument(
            "--non-interactive",
            action=argparse.BooleanOptionalAction,
            default=False,
            help="Automatically select the last n videos without prompting.",
        )

    # Subparser for local audio files
    parser_local = subparsers.add_parser(
        "local",
        help="Generate podcast from local audio files",
    )
    add_shared_arguments(parser_local)
    parser_local.add_argument(
        "audio_files",
        nargs="*",
        help="Audio files to include in the feed.",
    )
    parser_local.add_argument(
        "-o",
        "--output",
        type=str,
        help='Output path for RSS feed. Use "-" for stdout.',
    )
    parser_local.add_argument(
        "--scan-for-audio",
        action=argparse.BooleanOptionalAction,
        help="Scan current directory for audio files.",
    )
    parser_local.add_argument(
        "--image",
        type=str,
        help="Path to image file or audio file to extract image from.",
    )
    parser_local.set_defaults(base_dir=None)

    # Subparser for YouTube channel
    parser_yt = subparsers.add_parser(
        "yt",
        help="Generate podcast from a YouTube channel",
    )
    add_shared_arguments(parser_yt)
    parser_yt.add_argument(
        "channel_input",
        help="YouTube channel ID or URL to fetch videos from.",
    )
    parser_yt.add_argument(
        "--out-dir", type=str, help="Output directory for the podcast files."
    )
    parser_yt.add_argument(
        "--audio-format",
        choices=["mp3", "m4a", "wav"],
        default="m4a",
        help="Preferred audio format for output files.",
    )
    parser_yt.add_argument(
        "--audio-bitrate",
        type=int,
        default=256,
        help="Preferred audio bitrate in kbps.",
    )
    parser_yt.add_argument(
        "-n",
        "--num-videos",
        type=int,
        default=10,
        help="Limit to the last n videos from the channel.",
    )
    parser_yt.set_defaults(base_dir="~/Podcasts", overwrite=True)

    args = parser.parse_args()
    return args


def configure_logging(*, verbose: bool, log_file: Optional[str] = None):
    """Configure logging level.

    Args:
        verbose (bool): Enable detailed logging if True.
        log_file (Optional[str]): Path to a file to output logs.
    """
    level = logging.DEBUG if verbose else logging.INFO
    handlers = [logging.StreamHandler()]
    if log_file:
        handlers.append(logging.FileHandler(log_file))

    logging.basicConfig(
        level=level, format="%(levelname)s: %(message)s", handlers=handlers
    )


def yt_channel_id_get(text: str) -> str:
    """Extract the YouTube channel ID from the given text."""
    if re.match(YOUTUBE_CHANNEL_ID_PATTERN, text):
        return text

    match = re.search(YOUTUBE_CHANNEL_URL_PATTERN, text)
    if match:
        return match.group(1)

    try:
        ydl_opts = {'quiet': True, 'skip_download': True}
        with YoutubeDL(ydl_opts) as ydl:
            info = ydl.extract_info(text, download=False)
            channel_id = info.get('channel_id')
            if channel_id:
                return channel_id
            if info.get('id') and re.match(YOUTUBE_CHANNEL_ID_PATTERN, info['id']):
                return info['id']
    except Exception as e:
        logging.error(f"Error extracting channel ID from {text}: {e}")

    raise ValueError(f"Cannot extract channel ID from {text}")


class TqdmUpdater:
    def __init__(self):
        self.pbar = None

    def download_hook(self, d):
        if d['status'] == 'downloading':
            total = d.get('total_bytes') or d.get('total_bytes_estimate')
            downloaded = d.get('downloaded_bytes', 0)
            if self.pbar is None and total is not None:
                self.pbar = tqdm(total=total, unit='B', unit_scale=True, desc='Downloading')
            if self.pbar:
                self.pbar.update(downloaded - self.pbar.n)
        elif d['status'] == 'finished':
            if self.pbar:
                self.pbar.close()
                self.pbar = None
            logging.info(f"Downloaded {d['filename']}")
        elif d['status'] == 'error':
            if self.pbar:
                self.pbar.close()
                self.pbar = None
            logging.error(f"Error downloading {d.get('filename', 'unknown file')}")


def download_hook(d):
    """Fallback download hook for yt-dlp downloads (without tqdm)."""
    if d["status"] == "finished":
        logging.info(f"Downloaded {d['filename']}")
    elif d["status"] == "error":
        logging.error(f"Error downloading {d.get('filename', 'unknown file')}")
    elif d["status"] == "downloading":
        pass  # Add basic progress logging here if needed


def find_audio_files(
    *,
    working_dir: str,
    paths: Optional[Iterable[str]] = None,
    scan_for_audio: bool = False,
    audio_extensions: Optional[Iterable[str]] = None,
) -> List[str]:
    """Find audio files based on input methods.

    Args:
        working_dir (str): Working directory to search for audio files.
        paths (Optional[Iterable[str]]): Iterable of file paths.
        scan_for_audio (bool): Whether to scan the current directory for audio files.
        audio_extensions (Optional[Iterable[str]]): Audio file extensions to search for.

    Returns:
        List[str]: List of audio file paths.
    """
    if audio_extensions is None:
        audio_extensions = [".mp3", ".m4a", ".m4b", ".wav"]
    else:
        # Ensure the extensions start with a dot
        audio_extensions = [
            ext if ext.startswith(".") else "." + ext for ext in audio_extensions
        ]

    audio_files = []

    if paths:
        # Positional arguments provided
        audio_files = [
            os.path.abspath(path.strip())
            for path in paths
            if path.strip().lower().endswith(tuple(audio_extensions))
        ]
        logging.debug(f"Audio files from positional arguments: {audio_files}")

    elif scan_for_audio:
        # Scan working directory
        pattern = r".*(" + "|".join([re.escape(ext) for ext in audio_extensions]) + ")$"
        audio_files = list_children(
            working_dir,
            include_patterns=[pattern],
            recursive=True,
        )
        logging.debug(f"Audio files from scanning working directory: {audio_files}")

        # Sort audio files using version_sort_key
        audio_files.sort(key=version_sort_key)
        logging.debug(f"Sorted audio files:\n{audio_files}")

    else:
        logging.error("No audio files provided.")
        sys.exit(1)

    if not audio_files:
        logging.error("No audio files found.")
        sys.exit(1)

    return audio_files


def extract_audio_metadata(*, file_path: str) -> Dict[str, Any]:
    """Extract metadata from an audio file.

    Args:
        file_path (str): Path to the audio file.

    Returns:
        Dict[str, Any]: Metadata dictionary.
    """
    try:
        audio = MutagenFile(file_path, easy=True)
        if not audio:
            raise ValueError(f"Unsupported audio format for file {file_path}")
        metadata = {
            "file_path": file_path,
            "title": audio.get(
                "title", [os.path.splitext(os.path.basename(file_path))[0]]
            )[0],
            "date": os.path.getmtime(file_path),
            "duration": int(audio.info.length),
            "size": os.path.getsize(file_path),
            "author": audio.get("artist", [None])[0],
            "description": audio.get("description", [None])[0],
        }
        logging.debug(f"Extracted metadata for {file_path}: {metadata}")
        return metadata
    except Exception as e:
        logging.error(f"Error extracting metadata from {file_path}: {e}")
        return {}


def extract_image_from_audio(file_path: str) -> Optional[str]:
    """Extract album art from an audio file.

    Args:
        file_path (str): Path to the audio file.

    Returns:
        Optional[str]: Path to the extracted image file.
    """
    try:
        audio = MutagenFile(file_path)
        image_data = None
        if isinstance(audio, MP4):
            covers = audio.tags.get("covr")
            if covers:
                image_data = covers[0]
        elif "APIC:" in audio.tags:
            apic = audio.tags.get("APIC:")
            image_data = apic.data
        elif isinstance(audio.tags, ID3):
            for tag in audio.tags.values():
                if isinstance(tag, APIC):
                    image_data = tag.data
                    break
        else:
            logging.warning(f"No album art found in {file_path}")
            return None

        if image_data:
            image_path = os.path.splitext(file_path)[0] + "_cover.jpg"
            with open(image_path, "wb") as img_file:
                img_file.write(image_data)
            logging.debug(f"Extracted album art to {image_path}")
            return image_path
        else:
            logging.warning(f"No album art found in {file_path}")
            return None
    except Exception as e:
        logging.error(f"Error extracting album art from {file_path}: {e}")
        return None


def validate_image(*, image_path: str) -> bool:
    """Validate image requirements (minimum size, aspect ratio).

    Args:
        image_path (str): Path to the image file.

    Returns:
        bool: True if valid, False otherwise.
    """
    min_size = (400, 400)
    max_size = (3000, 3000)
    try:
        with Image.open(image_path) as img:
            width, height = img.size
            if width < min_size[0] or height < min_size[1]:
                logging.warning(
                    f"Image {image_path} is smaller than minimum recommended size {min_size}"
                )
                return False
            if width > max_size[0] or height > max_size[1]:
                logging.warning(
                    f"Image {image_path} is larger than maximum allowed size {max_size}"
                )
                return False
            if width != height:
                logging.warning(
                    f"Image {image_path} is not square (aspect ratio is {width}:{height})"
                )
                return False
            return True
    except Exception as e:
        logging.error(f"Error validating image {image_path}: {e}")
        return False


def find_highest_resolution_image(working_dir: str) -> Optional[str]:
    """Find the image with the highest resolution in the working directory.

    Args:
        working_dir (str): Working directory to search for images.

    Returns:
        Optional[str]: Path to the highest resolution image.
    """
    max_resolution = 0
    album_art_path = None
    image_files = list_children(
        working_dir,
        include_patterns=[r".*\.(jpg|jpeg|png)$"],
        recursive=True,
    )
    for image_path in image_files:
        try:
            with Image.open(image_path) as img:
                resolution = img.size[0] * img.size[1]
                if resolution > max_resolution:
                    max_resolution = resolution
                    album_art_path = image_path
                    logging.debug(
                        f"Found higher resolution image: {album_art_path} with resolution {resolution}"
                    )
        except Exception as e:
            logging.error(f"Error opening image {image_path}: {e}")
    return album_art_path


def generate_rss_feed(
    episodes: List[Dict[str, Any]],
    *,
    title: str,
    description: str,
    author: str,
    album_art_url: Optional[str],
    output_path: str,
    link_url: str,
    fake_dates: bool = False,
):
    """Generate the RSS feed using python-feedgen.

    Args:
        episodes (List[Dict[str, Any]]): List of episode metadata.
        title (str): Podcast title.
        description (str): Podcast description.
        author (str): Podcast author.
        album_art_url (Optional[str]): URL to the album art image.
        output_path (str): Output path for the RSS feed.
        link_url (str): Podcast main link URL.
        fake_dates (bool): Generate fake dates to maintain episode order.
    """
    fg = FeedGenerator()
    fg.load_extension("podcast")

    fg.title(title)
    fg.link(href=link_url, rel="alternate")
    fg.description(description)
    fg.language("en")
    fg.podcast.itunes_author(author)
    if album_art_url:
        fg.podcast.itunes_image(album_art_url)

    if fake_dates:
        # Generate dates starting from now and going backwards
        now = datetime.datetime.now().timestamp()
        interval = 24 * 60 * 60  # One day in seconds
        for i, ep in enumerate(episodes):
            ep["date"] = now - (i * interval)

    # Define MIME types based on audio format
    MIME_TYPES = {
        "mp3": "audio/mpeg",
        "m4a": "audio/mp4",
        "wav": "audio/wav",
    }

    for ep in episodes:
        fe = fg.add_entry()
        fe.id(ep["url"])
        fe.title(ep["title"])
        fe.description(ep.get("description", "No description available."))
        audio_format = os.path.splitext(ep["file_path"])[1][1:].lower()
        mime_type = MIME_TYPES.get(audio_format, "audio/mpeg")
        fe.enclosure(
            ep["url"], str(ep["size"]), mime_type
        )
        pub_date = email.utils.formatdate(ep["date"], usegmt=True)
        fe.pubDate(pub_date)
        duration = str(datetime.timedelta(seconds=ep.get("duration")))
        fe.podcast.itunes_duration(duration)
        if ep.get("author"):
            fe.podcast.itunes_author(ep["author"])

    if output_path == "-":
        print(fg.rss_str(pretty=True).decode("utf-8"))
    else:
        fg.rss_file(output_path)
        logging.info(f"RSS feed generated at {output_path}")


def handle_local_mode(args: argparse.Namespace):
    """Handle the local mode for processing audio files.

    Args:
        args (argparse.Namespace): Parsed arguments.
    """
    base_url_sanitized = args.base_url.rstrip("/")
    base_dir = os.path.expanduser(args.base_dir) if args.base_dir else os.getcwd()
    if not os.path.isdir(base_dir):
        logging.error(
            f"Base directory {base_dir} does not exist or is not a directory."
        )
        sys.exit(1)

    working_dir = os.getcwd()
    if len(args.audio_files) == 1 and os.path.isdir(args.audio_files[0]):
        working_dir = os.path.abspath(args.audio_files[0])
        args.audio_files = []
        args.scan_for_audio = True

    audio_files = find_audio_files(
        working_dir=working_dir,
        paths=args.audio_files if args.audio_files else None,
        scan_for_audio=args.scan_for_audio,
    )

    if not audio_files:
        logging.error("No audio files found.")
        sys.exit(1)

    episodes = []
    for file_path in audio_files:
        metadata = extract_audio_metadata(file_path=file_path)
        if metadata:
            rel_path = os.path.relpath(file_path, base_dir)
            metadata["url"] = f"{base_url_sanitized}/{rel_path.replace(os.sep, '/')}"
            episodes.append(metadata)

    if not episodes:
        logging.error("No valid audio files processed.")
        sys.exit(1)

    # Handle album art
    album_art_path = None
    if args.image:
        if os.path.isfile(args.image):
            album_art_path = os.path.abspath(args.image)
        else:
            logging.error(f"Image file {args.image} does not exist.")
            sys.exit(1)
    elif len(episodes) == 1:
        # Try extracting image from the audio file
        album_art_path = extract_image_from_audio(episodes[0]["file_path"])
    else:
        # Try finding the image with the biggest resolution in the working directory
        album_art_path = find_highest_resolution_image(working_dir)

    if album_art_path and not validate_image(image_path=album_art_path):
        logging.warning(f"Album art {album_art_path} does not meet requirements.")
    elif not album_art_path:
        logging.warning("No album art found.")
    album_art_url = None
    if album_art_path:
        rel_art_path = os.path.relpath(album_art_path, base_dir)
        album_art_url = f"{base_url_sanitized}/{rel_art_path.replace(os.sep, '/')}"

    # Determine podcast title and output file name
    if len(episodes) == 1:
        default_title = os.path.splitext(os.path.basename(episodes[0]["file_path"]))[0]
        default_output = f"{default_title}.rss"
    else:
        default_title = os.path.basename(working_dir)
        default_output = "feed.rss"

    default_output = os.path.join(working_dir, default_output)

    podcast_title = args.title or default_title
    podcast_author = args.author or episodes[0].get("author") or "Unknown Author"
    podcast_description = args.description or "No description available."
    link_url = args.link or base_url_sanitized

    output_path = args.output or default_output

    if not args.overwrite and os.path.exists(output_path):
        logging.error(
            f"Output file {output_path} already exists. Use --overwrite to overwrite."
        )
        sys.exit(1)

    generate_rss_feed(
        episodes,
        title=podcast_title,
        description=podcast_description,
        author=podcast_author,
        album_art_url=album_art_url,
        output_path=output_path,
        link_url=link_url,
        fake_dates=args.fake_dates,
    )


def handle_yt_mode(args: argparse.Namespace):
    """Handle the YouTube mode for processing a YouTube channel.

    Args:
        args (argparse.Namespace): Parsed arguments.
    """
    base_url_sanitized = args.base_url.rstrip("/")
    base_dir = os.path.expanduser(args.base_dir)
    if not os.path.isdir(base_dir):
        os.makedirs(base_dir)
        logging.info(f"Created base directory {base_dir}")

    try:
        channel_id = yt_channel_id_get(args.channel_input)
    except ValueError as e:
        logging.error(str(e))
        sys.exit(1)

    channel_url = f"https://www.youtube.com/channel/{channel_id}"

    tqdm_updater = TqdmUpdater() if args.tqdm else None
    progress_hooks = [tqdm_updater.download_hook] if args.tqdm else [download_hook]

    playlist_end = args.num_videos

    ydl_opts = {
        "ignoreerrors": True,
        "format": "bestaudio/best",
        "outtmpl": os.path.join(
            base_dir, args.out_dir or "%(uploader)s", "%(title)s.%(ext)s"
        ),
        "download_archive": os.path.join(base_dir, ".downloaded.txt"),
        "writethumbnail": True,
        "writesubtitles": True,
        "subtitleslangs": ["en"],
        "verbose": args.verbose,
        "overwrites": args.overwrite,
        "progress_hooks": progress_hooks,
        "continuedl": True,
        "noplaylist": False,
        "playlistend": playlist_end,
        "postprocessors": [
            {
                "key": "FFmpegExtractAudio",
                "preferredcodec": args.audio_format,
                "preferredquality": str(args.audio_bitrate),
            },
            {
                "key": "EmbedThumbnail",
            },
            {
                "key": "FFmpegMetadata",
            },
        ],
    }

    with YoutubeDL(ydl_opts) as ydl:
        logging.info(f"Fetching videos from {channel_url}")
        channel_info = ydl.extract_info(channel_url, download=False)
        if not channel_info:
            logging.error("Failed to retrieve channel information.")
            sys.exit(1)

        # Get the uploader's name for output directory
        uploader = (
            channel_info.get("channel")
            or channel_info.get("uploader")
            or "UnknownChannel"
        )
        output_dir = os.path.join(base_dir, args.out_dir or uploader)
        if not os.path.exists(output_dir):
            os.makedirs(output_dir)
            logging.info(f"Created output directory {output_dir}")

        # List videos (limited by playlist_end)
        videos = channel_info.get("entries", [])
        if not videos:
            logging.error("No videos found in the channel.")
            sys.exit(1)

        # Filter out None entries
        videos = [video for video in videos if video]
        if not videos:
            logging.error("No valid videos found in the channel.")
            sys.exit(1)

        # Sort videos by upload date (most recent first)
        videos = sorted(
            videos,
            key=lambda x: x.get("upload_date", ""),
            reverse=True,
        )

        if args.non_interactive:
            selected_videos = videos[:args.num_videos]
            logging.info(f"Selecting the last {len(selected_videos)} videos automatically.")
            video_urls = [video.get("webpage_url") for video in selected_videos]
        else:
            # Interactive mode: Allow selection from a larger set
            video_titles = [f"{video.get('title')}" for idx, video in enumerate(videos) if video.get('title')]
            selected_indices = rtl_iterfzf(video_titles, multi=True).selected_indices
            selected_videos = [videos[i] for i in selected_indices if 0 <= i < len(videos)]
            if not selected_videos:
                logging.info("No videos selected.")
                sys.exit(0)
            video_urls = [video.get("webpage_url") for video in selected_videos]

        # Download selected videos
        logging.info("Starting download of selected videos...")
        ydl.download(video_urls)

    # After download, process the downloaded files
    audio_extensions = [f".{args.audio_format.lower()}"]
    audio_files = find_audio_files(
        working_dir=output_dir, scan_for_audio=True, audio_extensions=audio_extensions
    )

    if not audio_files:
        logging.error("No audio files found after download.")
        sys.exit(1)

    episodes = []
    for file_path in audio_files:
        metadata = extract_audio_metadata(file_path=file_path)
        if metadata:
            rel_path = os.path.relpath(file_path, base_dir)
            metadata["url"] = f"{base_url_sanitized}/{rel_path.replace(os.sep, '/')}"
            episodes.append(metadata)

    if not episodes:
        logging.error("No valid audio files processed.")
        sys.exit(1)

    # Handle album art
    album_art_path = find_highest_resolution_image(output_dir)
    if album_art_path and not validate_image(image_path=album_art_path):
        logging.warning(f"Album art {album_art_path} does not meet requirements.")
    elif not album_art_path:
        logging.warning("No album art found.")
    album_art_url = None
    if album_art_path:
        rel_art_path = os.path.relpath(album_art_path, base_dir)
        album_art_url = f"{base_url_sanitized}/{rel_art_path.replace(os.sep, '/')}"

    podcast_title = args.title or uploader
    podcast_author = args.author or uploader
    podcast_description = (
        args.description or f"Podcast feed for YouTube channel {uploader}"
    )
    link_url = args.link or base_url_sanitized

    output_path = os.path.join(output_dir, "feed.rss")

    if not args.overwrite and os.path.exists(output_path):
        logging.error(
            f"Output file {output_path} already exists. Use --overwrite to overwrite."
        )
        sys.exit(1)

    generate_rss_feed(
        episodes,
        title=podcast_title,
        description=podcast_description,
        author=podcast_author,
        album_art_url=album_art_url,
        output_path=output_path,
        link_url=link_url,
        fake_dates=args.fake_dates,
    )


def main():
    """Main function to generate podcast RSS feed."""
    args = parse_arguments()
    configure_logging(verbose=args.verbose, log_file=args.log_file)

    if args.command == "local":
        handle_local_mode(args)
    elif args.command == "yt":
        handle_yt_mode(args)
    else:
        logging.error("Unknown command.")
        sys.exit(1)


if __name__ == "__main__":
    main()
