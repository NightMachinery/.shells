#!/bin/sh
# Builds Screen Decor (see ScreenDecor.m) into $1, default
# ~/Applications/ScreenDecor.app, ad-hoc signed. core/screen-decor.lua runs
# this when the app is missing or older than its sources.
set -eu
here=$(cd "$(dirname "$0")" && pwd)
app=${1:-"$HOME/Applications/ScreenDecor.app"}
tmp="$app.building"
case "$app" in
    *.app) ;;
    *) echo "build.sh: refusing a target that is not a .app bundle: $app" >&2; exit 2 ;;
esac

command rm -rf "$tmp"
command mkdir -p "$tmp/Contents/MacOS"
command cp "$here/Info.plist" "$tmp/Contents/Info.plist"
command clang -fobjc-arc -O2 -Wall \
    -framework Cocoa -framework QuartzCore \
    -o "$tmp/Contents/MacOS/ScreenDecor" "$here/ScreenDecor.m"
command codesign --force --sign - "$tmp" >/dev/null 2>&1 || true
command rm -rf "$app"
command mv "$tmp" "$app"
echo "$app"
