#!/usr/bin/env python3
##
import sys
import os
import re
import argparse
from pathlib import Path

def change_sni(filepath, new_sni):
    try:
        with open(filepath, 'r', encoding='utf-8') as f:
            content = f.read()
    except Exception as e:
        print(f"[-] Error reading {filepath}: {e}")
        return False

    original_content = content

    # Format dest (requires port, defaults to 443 if not provided by user)
    dest_val = new_sni if ":" in new_sni else f"{new_sni}:443"

    # 1. Update client configuration: "serverName": "..."
    content = re.sub(
        r'("serverName"\s*:\s*)"[^"]+"',
        rf'\g<1>"{new_sni}"',
        content
    )

    # 2. Update server configuration: "dest": "..."
    content = re.sub(
        r'("dest"\s*:\s*)"[^"]+"',
        rf'\g<1>"{dest_val}"',
        content
    )

    # 3. Update server configuration: "serverNames": [ ... ]
    def repl_serverNames(match):
        prefix = match.group(1)
        inner = match.group(2)
        suffix = match.group(3)

        # Simple heuristic to preserve multiline vs single-line formatting
        if '\n' in inner:
            return f'{prefix}\n            "{new_sni}"\n          {suffix}'
        else:
            return f'{prefix} "{new_sni}" {suffix}'

    content = re.sub(
        r'("serverNames"\s*:\s*\[)([^\]]*)(\])',
        repl_serverNames,
        content
    )

    if content != original_content:
        try:
            with open(filepath, 'w', encoding='utf-8') as f:
                f.write(content)
            print(f"[+] Updated SNI to '{new_sni}' in {filepath}")
            return True
        except Exception as e:
            print(f"[-] Error writing {filepath}: {e}")
            return False
    else:
        print(f"[i] No changes needed or fields not found in {filepath}")
        return False

def main():
    parser = argparse.ArgumentParser(description="Update SNI (Server Name) in Xray/V2Ray config files.")
    parser.add_argument('files', nargs='*', help="JSON config files to modify. Defaults to server.json and client.json in pwd.")
    parser.add_argument('-s', '--sni', default='snapp.ir', help="New SNI to set (default: snapp.ir)")

    args = parser.parse_args()

    files_to_process = args.files

    # Default behavior: look for server.json and client.json in pwd
    if not files_to_process:
        defaults = ['client.json', 'server.json']
        files_to_process = [f for f in defaults if os.path.isfile(f)]

        if not files_to_process:
            # Fallback to any JSON file in pwd
            json_files = list(Path('.').glob('*.json'))
            if json_files:
                files_to_process = [str(p) for p in json_files]
            else:
                print("[-] No JSON files found in the current directory.")
                sys.exit(1)

    success_count = 0
    for filepath in files_to_process:
        if not os.path.isfile(filepath):
            print(f"[-] File not found: {filepath}")
            continue
        if change_sni(filepath, args.sni):
            success_count += 1

    if success_count == 0:
        print("\n[!] No files were updated.")

if __name__ == '__main__':
    main()
