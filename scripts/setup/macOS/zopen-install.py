#!/usr/bin/env python3
"""Install Zopen.app and make it the default app for the types in Zopen.duti.

    python3 ~/scripts/setup/macOS/zopen-install.py

Copies the tracked app to /Applications (a copy, as Launch Services prefers), points its
launcher at this account's zopen.bash, registers it, and applies Zopen.duti. A row whose
extension has no declared type, such as .jl or .toml, cannot be set through `duti` (error
-50), so it is written to the Launch Services handler list by extension, the way Finder's
"Open With > Change All" stores it, after a backup; lsd then rereads the list. Safe to
rerun. Prints each extension's default app at the end.
"""
import plistlib
import shutil
import subprocess
import time
from pathlib import Path

HOME = Path.home()
SRC = HOME / 'scripts/zshlang/wrappers/Zopen.app'
APP = Path('/Applications/Zopen.app')
DUTI = HOME / 'scripts/zshlang/wrappers/Zopen.duti'
LAUNCHER = HOME / 'scripts/zshlang/wrappers/zopen.bash'
LSREGISTER = '/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister'
HANDLERS = HOME / 'Library/Preferences/com.apple.LaunchServices/com.apple.launchservices.secure.plist'
ROLES = {'editor': 'LSHandlerRoleEditor', 'all': 'LSHandlerRoleAll', 'viewer': 'LSHandlerRoleViewer'}
CHECK = ['pdf', 'org', 'md', 'txt', 'py', 'sh', 'lua', 'jl', 'toml', 'mp3', 'ogg', 'flac', 'epub', 'mobi', 'azw3']


def main():
    if not APP.exists():
        subprocess.run(['ditto', str(SRC), str(APP)], check=True)
    script = APP / 'Contents/Resources/script'
    if script.is_symlink() or script.exists():
        script.unlink()
    script.symlink_to(LAUNCHER)
    subprocess.run([LSREGISTER, '-f', str(APP)], check=True)

    rows = [line.split('\t') for line in DUTI.read_text().splitlines() if line and not line.startswith('#')]
    by_extension = [(bid, what[1:], role) for bid, what, role in rows if what.startswith('.')]
    for bid, what, role in rows:
        if not what.startswith('.'):
            subprocess.run(['duti', '-s', bid, what, role], capture_output=True)

    if by_extension:
        backup = HOME / f'.local/state/mac-bootstrap/launchservices-before-zopen-{time.strftime("%Y%m%dT%H%M%S")}.plist'
        backup.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy2(HANDLERS, backup)
        backup.chmod(0o600)
        d = plistlib.loads(HANDLERS.read_bytes())
        exts = {ext for _, ext, _ in by_extension}
        handlers = [h for h in d.get('LSHandlers', []) if not (h.get('LSHandlerContentTagClass') == 'public.filename-extension'
                                                               and h.get('LSHandlerContentTag') in exts)]
        for bid, ext, role in by_extension:
            handlers.append({'LSHandlerContentTag': ext, 'LSHandlerContentTagClass': 'public.filename-extension',
                             ROLES[role]: bid.lower(), 'LSHandlerPreferredVersions': {ROLES[role]: '-'}})
        d['LSHandlers'] = handlers
        HANDLERS.write_bytes(plistlib.dumps(d, fmt=plistlib.FMT_BINARY))
        subprocess.run(['killall', 'lsd'], capture_output=True)
        time.sleep(4)
        print(f'Launch Services list backed up to {backup}')

    for ext in CHECK:
        out = subprocess.run(['duti', '-x', ext], capture_output=True, text=True).stdout.splitlines()
        print(f'{ext}: {out[0] if out else "?"}')


if __name__ == '__main__':
    main()
