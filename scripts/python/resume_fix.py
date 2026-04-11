#!/usr/bin/env python3

import os
import sys

def format_size(size_in_bytes):
    """Converts bytes to a human-readable format."""
    for unit in['B', 'KB', 'MB', 'GB', 'TB']:
        if size_in_bytes < 1024.0:
            return f"{size_in_bytes:.2f} {unit}"
        size_in_bytes /= 1024.0
    return f"{size_in_bytes:.2f} PB"

def fix_partially_downloaded_file(file_path):
    chunk_size = 10 * 1024 * 1024  # Scan in 10 MB chunks
    rollback_margin = 1024 * 1024  # 1 MB rollback for safety

    if not os.path.exists(file_path):
        print(f"Error: File '{file_path}' not found.")
        return

    total_size = os.path.getsize(file_path)
    print(f"Original file size: {format_size(total_size)} ({total_size:,} bytes)")

    with open(file_path, 'r+b') as f:
        pos = total_size
        while pos > 0:
            # Read backwards in chunks
            read_size = min(chunk_size, pos)
            pos -= read_size
            f.seek(pos)
            chunk = f.read(read_size)

            # Strip trailing null bytes (\x00)
            stripped_chunk = chunk.rstrip(b'\x00')

            if len(stripped_chunk) > 0:
                # We found the end of the real data!
                exact_end = pos + len(stripped_chunk)

                # Roll back by 1MB to ensure we overwrite any half-written blocks
                # from when the download was abruptly stopped.
                safe_resume_point = max(0, exact_end - rollback_margin)

                print(f"Detected end of data at: {format_size(exact_end)}")
                print(f"Rolling back {format_size(rollback_margin)} for safety...")
                print(f"Truncating file to:      {format_size(safe_resume_point)}")

                f.truncate(safe_resume_point)

                chopped_amount = total_size - safe_resume_point
                print(f"Success! Chopped off {format_size(chopped_amount)} of empty space.")
                print(f"\nNext step: run your aria2c command with the '-c' flag.")
                return

        # If the loop finishes without returning, the whole file is empty
        print("File contains only zeroes. Truncating to 0 bytes.")
        f.truncate(0)

if __name__ == '__main__':
    if len(sys.argv) < 2:
        print("Usage: python resume_fix.py <path_to_file>")
    else:
        fix_partially_downloaded_file(sys.argv[1])
