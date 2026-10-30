#!/usr/bin/env python3
"""Audit/apply the macOS links from setup/setup_links without its install steps."""
import argparse
import datetime
import json
import os
from pathlib import Path
import shutil


def plan(home, night, system=False):
    if system:
        names = ['zshplain.dash', 'bicon_zsh.dash', 'ffscrollback.sh',
                 'emacs_scrollback.sh', 'wait4user.sh', 'pbcopy.sh', 'fzf_mru.sh']
        pairs = [(Path('/usr/local/bin') / n, night / 'zshlang/wrappers' / n) for n in names]
        for n in ['brishz.dash', 'brishz2.dash', 'brishz_para.dash', 'brishzrb.dash', 'brishzq.zsh', 'bsh.dash']:
            pairs.append((Path('/usr/local/bin') / n, night / 'zshlang/wrappers/brishz' / n))
        pairs += [(Path('/usr/local/bin/zsh'), Path('/opt/homebrew/bin/zsh')),
                  # The shared ~/.gnupg/gpg-agent.conf names this path.
                  (Path('/usr/local/bin/pinentry-mac'), Path('/opt/homebrew/bin/pinentry-mac')),
                  (Path('/usr/local/libexec/claude-askpass'), night / 'configFiles/claude-code/askpass.py')]
        return pairs
    pairs = [
        (home / '.gitconfig', night / 'symlinkme/local/.gitconfig'),
        (home / '.config/nvim/init.vim', home / '.nvimrc'),
        (home / '.hammerspoon/init.lua', night / 'hammerspoon/boot.lua'),
        (home / '.config/ranger', night / 'configFiles/ranger'),
        (home / '.config/powershell/profile.ps1', night / 'symlinkme/all/power_shell/profile.ps1'),
        (home / '.config/zathura/zathurarc', night / 'symlinkme/all/zathura/zathurarc'),
        (home / '.gallery-dl.conf', home / 'notes/private/configs/gallery-dl/.gallery-dl.conf'),
        (home / 'bin/pandoc', Path('/opt/homebrew/bin/pandoc')),
        (home / 'my-music/d', home / 'Downloads'),
    ]
    for name in ['kitty.conf', 'diff.conf', 'open-actions.conf']:
        pairs.append((home / '.config/kitty' / name, night / 'configFiles/kitty' / name))
    theme = night / 'configFiles/kitty/themes/night-solarized-light.conf'
    pairs += [(home / '.kitty_custom_1.conf', night / 'configFiles/kitty/me.conf'),
              (home / '.config/kitty/kitty-themes/themes/night-solarized-light.conf', theme)]
    active = home / '.config/kitty/theme.conf'
    if not active.exists():
        pairs.append((active, theme))
    for seat, theme_name in [('.claude', 'personal'), ('.claude-work', 'work')]:
        pairs += [(home / seat / 'settings.json', night / 'configFiles/claude-code/settings.json'),
                  (home / seat / 'themes/profile.json', night / f'configFiles/claude-code/themes/{theme_name}.json')]
    pairs += [(home / '.codex/hooks.json', night / 'configFiles/codex/hooks.json'),
              (home / '.gemini/config/hooks.json', night / 'configFiles/antigravity/hooks.json')]
    # MPV has its own repository; never create a dangling config link.
    if (home / 'code/lua/mpv').is_dir():
        pairs.append((home / '.config/mpv', home / 'code/lua/mpv'))
    return pairs


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--home', type=Path, default=Path.home())
    parser.add_argument('--nightdir', type=Path)
    parser.add_argument('--system', action='store_true', help='Only /usr/local wrappers; run with appropriate privileges')
    parser.add_argument('--apply', action='store_true')
    parser.add_argument('--backup-dir', type=Path)
    args = parser.parse_args()
    home = args.home.expanduser().absolute()
    night = (args.nightdir or home / 'scripts').expanduser().absolute()
    backup = args.backup_dir or home / '.local/state/mac-bootstrap' / ('links-before-' + datetime.datetime.now().strftime('%Y%m%dT%H%M%S'))
    rows = []
    for dest, source in plan(home, night, args.system):
        row = {'destination': str(dest), 'source': str(source)}
        if not source.exists():
            row['state'] = 'source-missing'
        elif dest.is_symlink() and dest.resolve() == source.resolve():
            row['state'] = 'linked'
        else:
            row['state'] = 'conflict' if dest.exists() or dest.is_symlink() else 'missing'
            if args.apply:
                if row['state'] == 'conflict':
                    saved = backup / str(dest).lstrip('/')
                    saved.parent.mkdir(parents=True, exist_ok=True)
                    shutil.move(str(dest), str(saved))
                dest.parent.mkdir(parents=True, exist_ok=True)
                temp = dest.with_name(dest.name + '.night-link')
                temp.symlink_to(os.path.relpath(source, dest.parent))
                temp.replace(dest)
                assert dest.resolve() == source.resolve()
                row['state'] = 'linked'
        rows.append(row)
    print(json.dumps({'applied': args.apply, 'system': args.system, 'backup': str(backup), 'links': rows}, indent=2))
    return int(any(r['state'] != 'linked' for r in rows))


if __name__ == '__main__':
    raise SystemExit(main())
