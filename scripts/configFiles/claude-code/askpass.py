#!/usr/bin/env python3
"""SUDO_ASKPASS helper: pinentry-mac dialog showing the full sudo command."""
import os, subprocess, sys, urllib.parse

PINENTRY = "/opt/homebrew/bin/pinentry-mac"

def esc(s):
    return "".join(f"%{ord(c):02X}" if c in "%\r\n" else c for c in s)

prompt = sys.argv[1] if len(sys.argv) > 1 else "Password:"

try:
    cmd = subprocess.run(["ps", "-o", "args=", "-p", str(os.getppid())],
                         capture_output=True, text=True, timeout=5).stdout.strip()
except Exception:
    cmd = ""

desc = f"{prompt}\n\n{cmd or '(command unknown)'}"

script = ("SETTITLE sudo\n"
          "SETPROMPT Password:\n"
          f"SETDESC {esc(desc)}\n"
          "GETPIN\nBYE\n")

out = subprocess.run([PINENTRY], input=script, capture_output=True, text=True).stdout

for line in out.splitlines():
    if line.startswith("D "):
        sys.stdout.write(urllib.parse.unquote(line[2:]))
        sys.exit(0)
sys.exit(1)
