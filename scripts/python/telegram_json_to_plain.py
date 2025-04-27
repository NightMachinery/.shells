#!/usr/bin/env python3
import json
import sys
import argparse
import os
from datetime import datetime, timedelta
from enum import Enum
from typing import Dict, List, Optional, TextIO, Union, Callable
from pynight.common_debugging2 import ipdb_enable


class ReplyMode(Enum):
    IGNORE = "ignore"
    EMAIL = "email"
    THREAD = "thread"


class MediaMode(Enum):
    NONE = "none"
    DETAILED = "detailed"
    COMPACT = "compact"


class ForwardMode(Enum):
    IGNORE = "ignore"
    ATTRIBUTION = "attribution"


def format_timestamp(timestamp: int, format_str: str = "%m/%d/%Y %H:%M") -> str:
    """Convert Unix timestamp to formatted date string."""
    date = datetime.fromtimestamp(timestamp)
    return date.strftime(format_str)


def convert_to_plain_text(text) -> str:
    """
    Convert a complex text structure to plain text.
    Text can be a string or a list containing strings and entity objects.
    """
    if isinstance(text, str):
        return text

    if not text:  # Handle None or empty list
        return ""

    if isinstance(text, list):
        result = []
        for item in text:
            if isinstance(item, str):
                result.append(item)
            elif isinstance(item, dict) and 'type' in item and 'text' in item:
                # Format entity based on its type
                entity_type = item['type']
                entity_text = item['text']

                if entity_type == 'mention':
                    result.append(f"[mention] {entity_text}")
                elif entity_type == 'hashtag':
                    result.append(f"[hashtag] {entity_text}")
                elif entity_type == 'link':
                    result.append(f"[link] {entity_text}")
                else:
                    # For other entity types we don't specifically handle
                    result.append(f"[{entity_type}] {entity_text}")
            else:
                # For any other structure we don't understand
                result.append(str(item))

        return "\n".join(result)

    # For any other type, convert to string
    return str(text)


def format_file_size(size_bytes: int) -> str:
    """Format file size in bytes to a human-readable format."""
    if size_bytes < 1024:
        return f"{size_bytes} B"
    elif size_bytes < 1024 * 1024:
        return f"{size_bytes / 1024:.1f} KB"
    else:
        return f"{size_bytes / (1024 * 1024):.2f} MB"


def format_duration(seconds: int) -> str:
    """Format duration in seconds to MM:SS or HH:MM:SS format."""
    duration = timedelta(seconds=seconds)
    if duration.total_seconds() < 3600:
        minutes, seconds = divmod(duration.total_seconds(), 60)
        return f"{int(minutes)}:{int(seconds):02d}"
    else:
        hours, remainder = divmod(duration.total_seconds(), 3600)
        minutes, seconds = divmod(remainder, 60)
        return f"{int(hours)}:{int(minutes):02d}:{int(seconds):02d}"


def get_media_description(message: Dict, media_mode: MediaMode) -> str:
    """Generate a text description of the media in the message."""
    if media_mode == MediaMode.NONE:
        return ""

    # Handle photo attachments
    if "photo" in message:
        width = message.get("width", 0)
        height = message.get("height", 0)
        file_size = message.get("photo_file_size", 0)

        if media_mode == MediaMode.DETAILED:
            return f"[PHOTO: {width}x{height} pixels, {format_file_size(file_size)}]"
        elif media_mode == MediaMode.COMPACT:
            return f"[📷 {width}×{height}]"
        else:
            raise ValueError(f"Unsupported media mode: {media_mode}")

    # Handle file attachments
    if "file" in message and "file_name" in message:
        file_name = message.get("file_name", "")
        file_size = message.get("file_size", 0)
        media_type = message.get("media_type", "")
        mime_type = message.get("mime_type", "")
        duration = message.get("duration_seconds", 0)
        width = message.get("width", 0)
        height = message.get("height", 0)

        # Handle audio files
        if media_type == "audio_file":
            duration_formatted = format_duration(duration)
            if media_mode == MediaMode.DETAILED:
                return f"[AUDIO: \"{file_name}\", {duration_formatted}, {format_file_size(file_size)}]"
            elif media_mode == MediaMode.COMPACT:
                # Shorten file name if needed, keeping only part before extension or first 25 chars
                short_name = os.path.splitext(file_name)[0]
                short_name = (short_name[:25] + "...") if len(short_name) > 28 else short_name
                return f"[🎵 \"{short_name}\" ({duration_formatted})]"
            else:
                raise ValueError(f"Unsupported media mode: {media_mode}")

        # Handle video/animation files
        elif media_type == "animation" or mime_type.startswith("video/"):
            duration_formatted = format_duration(duration)
            media_label = "ANIMATION" if media_type == "animation" else "VIDEO"
            emoji = "📹" if media_type == "animation" else "🎬"

            if media_mode == MediaMode.DETAILED:
                return f"[{media_label}: \"{file_name}\", {width}x{height} pixels, {duration_formatted}, {format_file_size(file_size)}]"
            elif media_mode == MediaMode.COMPACT:
                short_name = os.path.splitext(file_name)[0]
                short_name = (short_name[:25] + "...") if len(short_name) > 28 else short_name
                return f"[{emoji} \"{short_name}\" ({duration_formatted})]"
            else:
                raise ValueError(f"Unsupported media mode: {media_mode}")

        # Handle other file types
        else:
            if media_mode == MediaMode.DETAILED:
                return f"[FILE: \"{file_name}\", {mime_type}, {format_file_size(file_size)}]"
            elif media_mode == MediaMode.COMPACT:
                short_name = os.path.splitext(file_name)[0]
                short_name = (short_name[:25] + "...") if len(short_name) > 28 else short_name
                return f"[📄 \"{short_name}\"]"
            else:
                raise ValueError(f"Unsupported media mode: {media_mode}")

    return ""


def format_chat(
    chat_data: Dict, *,
    reply_mode: Union[str, ReplyMode] = ReplyMode.IGNORE.value,
    media_mode: Union[str, MediaMode] = MediaMode.NONE.value,
    forward_mode: Union[str, ForwardMode] = ForwardMode.ATTRIBUTION.value,
    timestamp_formatter: Callable[[int], str] = None
) -> str:
    """Format chat data into readable text."""
    # Convert string modes to enums
    if isinstance(reply_mode, str):
        try:
            reply_mode = ReplyMode(reply_mode)
        except ValueError:
            valid_modes = [mode.value for mode in ReplyMode]
            raise ValueError(f"Invalid reply mode: {reply_mode}. Valid modes are: {valid_modes}")

    if isinstance(media_mode, str):
        try:
            media_mode = MediaMode(media_mode)
        except ValueError:
            valid_modes = [mode.value for mode in MediaMode]
            raise ValueError(f"Invalid media mode: {media_mode}. Valid modes are: {valid_modes}")

    if isinstance(forward_mode, str):
        try:
            forward_mode = ForwardMode(forward_mode)
        except ValueError:
            valid_modes = [mode.value for mode in ForwardMode]
            raise ValueError(f"Invalid forward mode: {forward_mode}. Valid modes are: {valid_modes}")

    # Use default timestamp formatter if none provided
    if timestamp_formatter is None:
        timestamp_formatter = lambda ts: format_timestamp(ts)

    formatted_messages = []
    message_map = {}  # To store messages by ID for referencing

    # First pass: store all messages by ID if we need to reference replies
    if reply_mode != ReplyMode.IGNORE:
        for message in chat_data['messages']:
            if message.get('type', '') != 'service':
                message_map[message['id']] = message

    # Process each message
    for message in chat_data['messages']:
        # Skip service messages
        if message.get('type', '') == 'service':
            continue

        # Format date
        date_str = timestamp_formatter(int(message['date_unixtime']))

        # Get the sender's name and message text
        sender = message['from']
        raw_text = message.get('text', '')
        text = convert_to_plain_text(raw_text)

        # Handle reply modes
        reply_info = ''
        reply_prefix = ''

        if reply_mode != ReplyMode.IGNORE and 'reply_to_message_id' in message:
            reply_id = message['reply_to_message_id']
            replied_message = message_map.get(reply_id)

            if replied_message:
                replied_sender = replied_message['from']
                replied_raw_text = replied_message.get('text', '')
                replied_text = convert_to_plain_text(replied_raw_text)
                replied_date_str = timestamp_formatter(int(replied_message['date_unixtime']))

                if reply_mode == ReplyMode.EMAIL:
                    # Format quoted text with proper '>' prefix for each line
                    quoted_text = ""
                    if replied_text:
                        # Split by newlines and add '>' prefix to each line
                        quoted_lines = [f"> {line}" for line in replied_text.split('\n')]
                        quoted_text = '\n'.join(quoted_lines)
                    reply_prefix = f"On {replied_date_str}, {replied_sender} wrote:\n{quoted_text}\n\n"
                elif reply_mode == ReplyMode.THREAD:
                    reply_info = f" [RE: MSG_{reply_id}]"

        # Handle forwarded messages
        forward_prefix = ""
        if "forwarded_from" in message and forward_mode == ForwardMode.ATTRIBUTION:
            forward_source = message["forwarded_from"]
            forward_prefix = f"[Forwarded from {forward_source}]\n"

        # Get media description if applicable
        media_desc = get_media_description(message, media_mode)
        if media_desc:
            if text:
                text = f"{media_desc}\n{text}"
            else:
                text = media_desc

        # Add forwarded message attribution if applicable
        if forward_prefix:
            text = f"{forward_prefix}{text}"

        # Format the message line based on reply mode
        if reply_mode == ReplyMode.THREAD:
            formatted_line = f"[MSG_{message['id']}] {sender}, [{date_str}]{reply_info}\n{reply_prefix}{text}\n"
        else:
            formatted_line = f"{sender}, [{date_str}]\n{reply_prefix}{text}\n"

        formatted_messages.append(formatted_line)

        # Check for reactions
        if 'reactions' in message:
            for reaction in message['reactions']:
                emoji = reaction['emoji']
                for recent in reaction['recent']:
                    reactor_name = recent['from']
                    formatted_messages.append(f"{reactor_name}'s Reaction: {emoji}")

        # Add a blank line after each message for readability
        formatted_messages.append('')

    # Return the formatted chat
    return '\n'.join(formatted_messages)


def process_chat_file(
    file: TextIO, *,
    reply_mode: str = 'ignore',
    media_mode: str = 'none',
    forward_mode: str = 'attribution'
) -> str:
    """Process a chat file and return formatted output."""
    try:
        chat_data = json.load(file)
        return format_chat(
            chat_data,
            reply_mode=reply_mode,
            media_mode=media_mode,
            forward_mode=forward_mode
        )
    except json.JSONDecodeError as e:
        raise ValueError(f"Invalid JSON format: {e}")


def main():
    parser = argparse.ArgumentParser(description='Format chat JSON to readable text')
    parser.add_argument(
        'file',
        nargs='?',
        type=argparse.FileType('r'),
        default=(None if sys.stdin.isatty() else sys.stdin),
        help='JSON file to process (reads from stdin if not provided and stdin is not a terminal)'
    )
    parser.add_argument(
        '--reply-mode',
        choices=[mode.value for mode in ReplyMode],
        # default=ReplyMode.IGNORE.value,
        default=ReplyMode.EMAIL.value,
        help='How to display message replies'
    )
    parser.add_argument(
        '--media-mode',
        choices=[mode.value for mode in MediaMode],
        # default=MediaMode.NONE.value,
        default=MediaMode.DETAILED.value,
        help='How to display media attachments'
    )
    parser.add_argument(
        '--forward-mode',
        choices=[mode.value for mode in ForwardMode],
        default=ForwardMode.ATTRIBUTION.value,
        help='How to display forwarded messages'
    )
    args = parser.parse_args()

    if args.file is None:
        parser.print_help()
        sys.exit(1)

    try:
        formatted_chat = process_chat_file(
            args.file,
            reply_mode=args.reply_mode,
            media_mode=args.media_mode,
            forward_mode=args.forward_mode
        )
        print(formatted_chat)
    except ValueError as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)
    # except Exception as e:
    #     print(f"Error processing chat: {e}", file=sys.stderr)
    #     sys.exit(1)
    finally:
        if args.file is not sys.stdin:
            args.file.close()


if __name__ == "__main__":
    ipdb_enable(tlg_chat_id=None)
    main()
