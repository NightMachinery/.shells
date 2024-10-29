#!/usr/bin/env python3

"""
podcaster.py - Podcast RSS Feed Generator

Creates standardized podcast RSS feeds from audio files, automatically handling metadata and artwork.

Example usage:
    python podcaster.py audiobook.m4b --base-url "https://files.lilf.ir/" --base-dir "~/Downloads" --image cover.jpg

Dependencies:
    - mutagen
    - Pillow
    - feedgen

pip-install feedgen Pillow mutagen
"""

import os
import sys
import argparse
import logging
import datetime
from typing import List, Optional, Dict, Any, Iterable
import email.utils

from mutagen import File as MutagenFile
from mutagen.id3 import ID3, APIC
from mutagen.mp4 import MP4Cover, MP4
from PIL import Image
from feedgen.feed import FeedGenerator


def parse_arguments() -> argparse.Namespace:
    """Parse command-line arguments.

    Returns:
        argparse.Namespace: Parsed arguments.
    """
    parser = argparse.ArgumentParser(description='Podcast RSS Feed Generator')
    parser.add_argument('audio_files', nargs='*', help='Audio files to include in the feed.')
    parser.add_argument('-o', '--output', type=str,
                        help='Output path for RSS feed. Use "-" for stdout.')
    parser.add_argument('-f', '--overwrite', action='store_true',
                        help='Allow overwriting existing output file (default: disabled).')
    parser.add_argument('-v', '--verbose', action='store_true',
                        help='Enable detailed logging output.')
    parser.add_argument('--title', type=str, help='Override podcast title.')
    parser.add_argument('--description', type=str, help='Set podcast description.')
    parser.add_argument('--author', type=str, help='Set podcast author.')
    parser.add_argument('--base-url', type=str, required=True,
                        help='Base URL where the audio files and images are hosted.')
    parser.add_argument('--base-dir', type=str, required=True,
                        help='Base directory from which to compute relative paths.')
    parser.add_argument('--link', type=str, help='Podcast main link URL.')
    parser.add_argument('--scan-for-audio', action='store_true',
                        help='Scan current directory for audio files.')
    parser.add_argument('--image', type=str,
                        help='Path to image file or audio file to extract image from.')
    args = parser.parse_args()
    return args


def configure_logging(*, verbose: bool):
    """Configure logging level.

    Args:
        verbose (bool): Enable detailed logging if True.
    """
    level = logging.DEBUG if verbose else logging.INFO
    logging.basicConfig(level=level, format='%(levelname)s: %(message)s')


def find_audio_files(*, base_dir: str, paths: Optional[Iterable[str]] = None, scan_for_audio: bool = False) -> List[str]:
    """Find audio files based on input methods.

    Args:
        base_dir (str): Base directory to search for audio files.
        paths (Optional[Iterable[str]]): Iterable of file paths.
        scan_for_audio (bool): Whether to scan the current directory for audio files.

    Returns:
        List[str]: List of audio file paths.
    """
    audio_extensions = ('.mp3', '.m4a', '.m4b', '.wav')
    audio_files = []

    if paths:
        # Positional arguments provided
        audio_files = [os.path.abspath(path.strip()) for path in paths if path.strip().lower().endswith(audio_extensions)]
        logging.debug(f'Audio files from positional arguments: {audio_files}')
    elif not sys.stdin.isatty():
        # Read from stdin
        input_files = [line.strip() for line in sys.stdin if line.strip()]
        audio_files = [os.path.abspath(path) for path in input_files if path.lower().endswith(audio_extensions)]
        logging.debug(f'Audio files from stdin: {audio_files}')
    elif scan_for_audio or sys.stdin.isatty():
        # Scan current directory
        for root, _, files in os.walk(os.getcwd()):
            for f in files:
                if f.lower().endswith(audio_extensions):
                    audio_files.append(os.path.join(root, f))
        logging.debug(f'Audio files from scanning current directory: {audio_files}')
    else:
        logging.error('No audio files provided.')
        sys.exit(1)

    if not audio_files:
        logging.error('No audio files found.')
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
            raise ValueError(f'Unsupported audio format for file {file_path}')
        metadata = {
            'file_path': file_path,
            'title': audio.get('title', [os.path.splitext(os.path.basename(file_path))[0]])[0],
            'date': os.path.getmtime(file_path),
            'duration': int(audio.info.length),
            'size': os.path.getsize(file_path),
            'author': audio.get('artist', [None])[0],
            'description': audio.get('description', [None])[0],
        }
        logging.debug(f'Extracted metadata for {file_path}: {metadata}')
        return metadata
    except Exception as e:
        logging.error(f'Error extracting metadata from {file_path}: {e}')
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
            covers = audio.tags.get('covr')
            if covers:
                image_data = covers[0]
        elif 'APIC:' in audio.tags:
            apic = audio.tags.get('APIC:')
            image_data = apic.data
        elif isinstance(audio.tags, ID3):
            for tag in audio.tags.values():
                if isinstance(tag, APIC):
                    image_data = tag.data
                    break
        else:
            logging.warning(f'No album art found in {file_path}')
            return None

        if image_data:
            image_path = os.path.splitext(file_path)[0] + '_cover.jpg'
            with open(image_path, 'wb') as img_file:
                img_file.write(image_data)
            logging.debug(f'Extracted album art to {image_path}')
            return image_path
        else:
            logging.warning(f'No album art found in {file_path}')
            return None
    except Exception as e:
        logging.error(f'Error extracting album art from {file_path}: {e}')
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
                logging.warning(f'Image {image_path} is smaller than minimum recommended size {min_size}')
                return False
            if width > max_size[0] or height > max_size[1]:
                logging.warning(f'Image {image_path} is larger than maximum allowed size {max_size}')
                return False
            if width != height:
                logging.warning(f'Image {image_path} is not square (aspect ratio is {width}:{height})')
                return False
            return True
    except Exception as e:
        logging.error(f'Error validating image {image_path}: {e}')
        return False


def generate_rss_feed(
    episodes: List[Dict[str, Any]],
    *,
    title: str,
    description: str,
    author: str,
    album_art_url: Optional[str],
    output_path: str,
    link_url: str,
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
    """
    fg = FeedGenerator()
    fg.load_extension('podcast')

    fg.title(title)
    fg.link(href=link_url, rel='alternate')
    fg.description(description)
    fg.language('en')
    fg.podcast.itunes_author(author)
    if album_art_url:
        fg.podcast.itunes_image(album_art_url)

    for ep in episodes:
        fe = fg.add_entry()
        fe.id(ep['url'])
        fe.title(ep['title'])
        fe.description(ep.get('description', 'No description available.'))
        fe.enclosure(ep['url'], str(ep['size']), 'audio/mpeg')  # Adjust MIME type as needed
        pub_date = email.utils.formatdate(ep['date'], usegmt=True)
        fe.pubDate(pub_date)
        duration = str(datetime.timedelta(seconds=ep.get('duration')))
        fe.podcast.itunes_duration(duration)
        if ep.get('author'):
            fe.podcast.itunes_author(ep['author'])

    if output_path == '-':
        print(fg.rss_str(pretty=True).decode('utf-8'))
    else:
        fg.rss_file(output_path)
        logging.info(f'RSS feed generated at {output_path}')


def main():
    """Main function to generate podcast RSS feed."""
    args = parse_arguments()
    configure_logging(verbose=args.verbose)

    base_dir = os.path.expanduser(args.base_dir)
    if not os.path.isdir(base_dir):
        logging.error(f'Base directory {base_dir} does not exist or is not a directory.')
        sys.exit(1)

    audio_files = find_audio_files(
        base_dir=base_dir,
        paths=args.audio_files if args.audio_files else None,
        scan_for_audio=args.scan_for_audio
    )

    if not audio_files:
        logging.error('No audio files found.')
        sys.exit(1)

    episodes = []
    for file_path in audio_files:
        metadata = extract_audio_metadata(file_path=file_path)
        if metadata:
            rel_path = os.path.relpath(file_path, base_dir)
            metadata['url'] = f"{args.base_url.rstrip('/')}/{rel_path.replace(os.sep, '/')}"
            episodes.append(metadata)

    if not episodes:
        logging.error('No valid audio files processed.')
        sys.exit(1)

    # Handle album art
    album_art_path = None
    if args.image:
        if os.path.isfile(args.image):
            album_art_path = os.path.abspath(args.image)
        else:
            logging.error(f'Image file {args.image} does not exist.')
            sys.exit(1)
    elif len(episodes) == 1:
        # Try extracting image from the audio file
        album_art_path = extract_image_from_audio(episodes[0]['file_path'])
    else:
        # Try finding the image with the biggest resolution in the current directory
        max_resolution = 0
        for root, _, files in os.walk(os.getcwd()):
            for f in files:
                if f.lower().endswith(('.jpg', '.jpeg', '.png')):
                    image_path = os.path.join(root, f)
                    try:
                        with Image.open(image_path) as img:
                            resolution = img.size[0] * img.size[1]
                            if resolution > max_resolution:
                                max_resolution = resolution
                                album_art_path = image_path
                                logging.debug(f'Found higher resolution image: {album_art_path} with resolution {resolution}')
                    except Exception as e:
                        logging.error(f'Error opening image {image_path}: {e}')

    if album_art_path and not validate_image(image_path=album_art_path):
        logging.warning(f"Album art {album_art_path} does not meet requirements.")
    elif not album_art_path:
        logging.warning('No album art found.')
    album_art_url = None
    if album_art_path:
        rel_art_path = os.path.relpath(album_art_path, base_dir)
        album_art_url = f"{args.base_url.rstrip('/')}/{rel_art_path.replace(os.sep, '/')}"

    # Determine podcast title and output file name
    if len(episodes) == 1:
        default_title = os.path.splitext(os.path.basename(episodes[0]['file_path']))[0]
        default_output = f"{default_title}.rss"
    else:
        default_title = os.path.basename(os.getcwd())
        default_output = 'feed.rss'

    podcast_title = args.title or default_title
    podcast_author = args.author or episodes[0].get('author') or 'Unknown Author'
    podcast_description = args.description or 'No description available.'
    link_url = args.link or args.base_url.rstrip('/')

    output_path = args.output or default_output

    if not args.overwrite and os.path.exists(output_path):
        logging.error(f'Output file {output_path} already exists. Use --overwrite to overwrite.')
        sys.exit(1)

    generate_rss_feed(
        episodes,
        title=podcast_title,
        description=podcast_description,
        author=podcast_author,
        album_art_url=album_art_url,
        output_path=output_path,
        link_url=link_url,
    )


if __name__ == "__main__":
    main()

