#!/usr/bin/env python3
"""Install independent notification identities using one pinned build recipe.

Used by email/install-night-mail.zsh and the notif-app-install shell function.
Only the optional DND bypass uses a private API. See docs/notification-apps.md.
"""
import argparse
import fcntl
import hashlib
import json
import os
from pathlib import Path
import plistlib
import shutil
import subprocess
import sys
import tarfile
import tempfile

SOURCE_URL = "https://codeload.github.com/julienXX/terminal-notifier/tar.gz/2.0.0"
SOURCE_SHA256 = "6f22a7626e4e68e88df2005a5f256f7d3b432dbf4c0f8a0c15c968d9e38bf84c"
PROFILES = {"mail": ("Night Mail", "com.user.night-mail.notify"),
            "agents": ("Agent Bells", "com.user.night-agent-bells.notify")}
HERE = Path(__file__).resolve().parent
LSREGISTER = "/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister"


def run(args, timeout=120, **kwargs):
    return subprocess.run([str(a) for a in args], check=True, timeout=timeout, **kwargs)


def app_path(profile):
    return Path.home() / "Applications" / (PROFILES[profile][0] + ".app")


def binary(app):
    return app / "Contents/MacOS/terminal-notifier"


def fingerprint(profile):
    digest = hashlib.sha256()
    for name in ("build.py", "main.m", "icon.m"):
        digest.update((HERE / name).read_bytes())
    digest.update(profile.encode())
    return digest.hexdigest()


def owned_app(app, profile):
    try:
        manifest = json.loads((app / "Contents/Resources/night-build.json").read_text())
        info = plistlib.loads((app / "Contents/Info.plist").read_bytes())
        return info["CFBundleIdentifier"] == PROFILES[profile][1] and "fingerprint" in manifest
    except (OSError, ValueError, KeyError):
        return False


def valid_app(app, profile, fresh=True):
    try:
        manifest = json.loads((app / "Contents/Resources/night-build.json").read_text())
        info = plistlib.loads((app / "Contents/Info.plist").read_bytes())
        if info["CFBundleIdentifier"] != PROFILES[profile][1] or not binary(app).is_file():
            return False
        if fresh and manifest["fingerprint"] != fingerprint(profile):
            return False
        run(["/usr/bin/codesign", "--verify", "--deep", "--strict", app],
            stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        return True
    except (OSError, ValueError, KeyError, subprocess.SubprocessError):
        return False


def source(cache):
    archive = cache / "terminal-notifier-2.0.0.tar.gz"
    if not archive.is_file() or hashlib.sha256(archive.read_bytes()).hexdigest() != SOURCE_SHA256:
        partial = archive.with_suffix(".download")
        try:
            run(["/usr/bin/curl", "--fail", "--silent", "--show-error", "--location",
                 "--connect-timeout", "15", "--max-time", "60", SOURCE_URL, "--output", partial], timeout=65)
            if hashlib.sha256(partial.read_bytes()).hexdigest() != SOURCE_SHA256:
                raise ValueError("notifier source checksum mismatch")
            partial.replace(archive)
        finally:
            partial.unlink(missing_ok=True)
    return archive


def patch_delegate(text):
    # The stock default impersonates Terminal.app. Do not swizzle identity: the
    # real signed bundle supplies the icon, permissions, and dismissal boundary.
    needle = 'appDefaults = @{@"sender": @"com.apple.Terminal"};'
    if text.count(needle) != 1:
        raise ValueError("pinned notifier sender patch no longer applies")
    text = text.replace(needle, "appDefaults = @{};")
    # Clicking should simply dismiss; upstream logs all notification text.
    start = text.index("- (void)userActivatedNotification:")
    end = text.index("- (BOOL)activateAppWithBundleID:", start)
    text = text[:start] + '''- (void)userActivatedNotification:(NSUserNotification *)notification;
{
  [[NSUserNotificationCenter defaultUserNotificationCenter] removeDeliveredNotification:notification];
  exit(0);
}

''' + text[end:]
    return text


def build(profile, archive, work):
    name, bundle_id = PROFILES[profile]
    # Extract only the two source files we compile, never archive paths/symlinks.
    with tarfile.open(archive) as tar:
        for filename in ("AppDelegate.m", "AppDelegate.h"):
            member = tar.extractfile("terminal-notifier-2.0.0/Terminal Notifier/" + filename)
            text = member.read().decode()
            (work / filename).write_text(patch_delegate(text) if filename.endswith(".m") else text)
        (work / "LICENSE.md").write_bytes(tar.extractfile("terminal-notifier-2.0.0/LICENSE.md").read())
    app = work / (name + ".app")
    resources = app / "Contents/Resources"
    resources.mkdir(parents=True)
    binary(app).parent.mkdir()
    info = {"CFBundleIdentifier": bundle_id, "CFBundleName": name,
            "CFBundleDisplayName": name, "CFBundleExecutable": "terminal-notifier",
            "CFBundleIconFile": "icon", "CFBundlePackageType": "APPL",
            "CFBundleVersion": "1", "CFBundleShortVersionString": "2.0.0-night",
            "LSMinimumSystemVersion": "10.14", "LSUIElement": True,
            "NSUserNotificationAlertStyle": "banner"}
    (app / "Contents/Info.plist").write_bytes(plistlib.dumps(info))
    build_env = dict(os.environ, TMPDIR=str(work), CLANG_MODULE_CACHE_PATH=str(work / "modules"))
    run(["/usr/bin/xcrun", "clang", "-fobjc-arc", "-Wno-deprecated-declarations",
         "-mmacosx-version-min=10.14", "-framework", "Cocoa", "-framework", "ScriptingBridge",
         "-framework", "UserNotifications", "-I", work, HERE / "main.m", work / "AppDelegate.m",
         "-o", binary(app)], env=build_env)
    run(["/usr/bin/xcrun", "clang", "-fobjc-arc", "-framework", "Cocoa",
         HERE / "icon.m", "-o", work / "draw-icon"], env=build_env)
    run([work / "draw-icon", profile, work / "icon.png"], env=build_env)
    iconset = work / "icon.iconset"
    iconset.mkdir()
    for size in (16, 32, 128, 256, 512):
        for scale in (1, 2):
            label = f"icon_{size}x{size}" + ("@2x" if scale == 2 else "") + ".png"
            run(["/usr/bin/sips", "--resampleHeightWidth", size * scale, size * scale,
                 work / "icon.png", "--out", iconset / label], stdout=subprocess.DEVNULL)
    run(["/usr/bin/iconutil", "--convert", "icns", iconset, "--output", resources / "icon.icns"])
    shutil.copy2(work / "LICENSE.md", resources)
    (resources / "night-build.json").write_text(json.dumps({"fingerprint": fingerprint(profile),
                                                           "source_sha256": SOURCE_SHA256}))
    run(["/usr/bin/codesign", "--force", "--sign", "-", app])
    if not valid_app(app, profile):
        raise ValueError("built notification app failed verification")
    return app


def install(profile):
    app = app_path(profile)
    cache = Path.home() / ".cache/night-notification-apps"
    cache.mkdir(parents=True, exist_ok=True)
    # Serialize email setup and first-use agent setup, including final replacement.
    with (cache / "build.lock").open("a") as lock:
        fcntl.flock(lock, fcntl.LOCK_EX)
        if valid_app(app, profile):
            print(f"{app}: up to date")
            return
        if app.exists() and not owned_app(app, profile):
            raise ValueError(f"refusing to replace unrecognized app: {app}")
        first_install = not app.exists()
        archive = source(cache)
        app.parent.mkdir(parents=True, exist_ok=True)
        with tempfile.TemporaryDirectory(prefix=".night-notifier-", dir=app.parent) as scratch:
            candidate = build(profile, archive, Path(scratch))
            backup = Path(scratch) / "previous.app"
            if app.exists():
                app.rename(backup)
            try:
                candidate.rename(app)
                run([LSREGISTER, "-f", app], timeout=20, stdout=subprocess.DEVNULL)
            except BaseException:
                if app.exists():
                    shutil.rmtree(app)
                if backup.exists():
                    backup.rename(app)
                raise
        print(f"installed {app}")
    if first_install:
        try:
            run([binary(app), "--authorize"], timeout=50)
            run([binary(app), "-title", PROFILES[profile][0], "-message", "Notifications are ready."], timeout=10)
        except (OSError, subprocess.SubprocessError):
            print(f"warning: allow notifications for {PROFILES[profile][0]} in System Settings", file=sys.stderr)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("profile", choices=PROFILES)
    parser.add_argument("--check", action="store_true")
    args = parser.parse_args()
    if sys.platform != "darwin":
        parser.error("notification apps require macOS")
    try:
        if args.check:
            app = app_path(args.profile)
            if not valid_app(app, args.profile):
                print(f"{app}: missing, invalid, or needs rebuilding", file=sys.stderr)
                return 1
            print(f"{app}: signature and build inputs verified")
            return run([binary(app), "--diagnose"], timeout=10).returncode
        install(args.profile)
        return 0
    except (OSError, ValueError, subprocess.SubprocessError) as error:
        print(f"notification-apps: {error}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main())
