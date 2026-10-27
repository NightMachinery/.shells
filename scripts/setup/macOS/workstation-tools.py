#!/usr/bin/env python3
"""Audit or install the CLI portion of the Night macOS workstation."""
import argparse
import datetime
import json
import os
from pathlib import Path
import shutil
import signal
import subprocess


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--apply', action='store_true', help='Install missing formulae; native helper recipes remain separate')
    parser.add_argument('--report-dir', type=Path)
    args = parser.parse_args()
    profile = json.loads(Path(__file__).with_suffix('.json').read_text())
    brew = shutil.which('brew')
    if not brew:
        parser.error('Homebrew is required')
    report = args.report_dir or Path.home() / '.local/state/mac-bootstrap' / ('tools-' + datetime.datetime.now().strftime('%Y%m%dT%H%M%S'))
    report.mkdir(parents=True, exist_ok=True)
    env = os.environ | {
        'HOMEBREW_NO_AUTO_UPDATE': '1', 'HOMEBREW_NO_INSTALL_CLEANUP': '1',
        'HOMEBREW_NO_INSTALL_UPGRADE': '1', 'HOMEBREW_NO_INSTALLED_DEPENDENTS_CHECK': '1',
        'NONINTERACTIVE': '1', 'GIT_TERMINAL_PROMPT': '0',
    }
    known_hosts = seed_github_known_hosts() if args.apply else None
    installed = set(subprocess.check_output([brew, 'list', '--formula', '-1'], text=True).splitlines())
    missing = [f for f in profile['formulae'] if f.rsplit('/', 1)[-1] not in installed]
    results = []
    if args.apply:
        for formula in missing:
            # Trust only the selected formula, on Homebrew versions that require it.
            if formula.count('/') == 2:
                tap = formula.rsplit('/', 1)[0]
                code = clone_tap(brew, tap, env)
                if code == 0 and subprocess.run([brew, 'help', 'trust'], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL).returncode == 0:
                    code = subprocess.run([brew, 'trust', '--formula', formula], env=env).returncode
                if code:
                    results.append({'formula': formula, 'status': code, 'stage': 'tap'})
                    continue
            argv = [brew, 'install']
            if formula in profile['head_formulae']:
                argv.append('--HEAD')
            argv.append(formula)
            print('Installing', formula, flush=True)
            with (report / (formula.replace('/', '_') + '.log')).open('w') as log:
                proc = subprocess.Popen(argv, env=env, stdin=subprocess.DEVNULL, stdout=log, stderr=subprocess.STDOUT, start_new_session=True)
                try:
                    code = proc.wait(timeout=3600)
                except subprocess.TimeoutExpired:
                    os.killpg(proc.pid, signal.SIGTERM)
                    try:
                        proc.wait(timeout=10)
                    except subprocess.TimeoutExpired:
                        os.killpg(proc.pid, signal.SIGKILL)
                        proc.wait()
                    code = 124
            results.append({'formula': formula, 'status': code})
            (report / 'package-results.json').write_text(json.dumps(results, indent=2) + '\n')
        installed = set(subprocess.check_output([brew, 'list', '--formula', '-1'], text=True).splitlines())
        missing = [f for f in profile['formulae'] if f.rsplit('/', 1)[-1] not in installed]
    commands = {name: shutil.which(name) for group in profile['command_groups'].values() for name in group}
    absent = sorted(name for name, path in commands.items() if not path)
    brightness = subprocess.run([brew, 'list', '--versions', 'brightness'], capture_output=True, text=True)
    head_ok = brightness.returncode == 0 and 'HEAD-' in brightness.stdout
    receipt = {'applied': args.apply, 'github_known_hosts_added': known_hosts,
               'missing_formulae': missing, 'commands': commands,
               'missing_commands': absent, 'brightness_head': head_ok, 'package_results': results}
    (report / 'audit.json').write_text(json.dumps(receipt, indent=2) + '\n')
    print(json.dumps({'report': str(report), 'missing_formulae': missing,
                      'missing_commands': absent, 'brightness_head': head_ok}, indent=2))
    return int(bool(missing or absent or not head_ok or any(r['status'] for r in results)))


def seed_github_known_hosts():
    """Add GitHub's published SSH host keys, fetched over TLS, to ~/.ssh/known_hosts.

    The shared .gitconfig rewrites https://github.com/ to SSH, so a fresh
    machine cannot fetch from GitHub until it knows GitHub's host keys.
    """
    meta = json.loads(subprocess.check_output(['curl', '-fsS', '--max-time', '30', 'https://api.github.com/meta']))
    path = Path.home() / '.ssh/known_hosts'
    path.parent.mkdir(mode=0o700, exist_ok=True)
    present = path.read_text().splitlines() if path.exists() else []
    known = {tuple(line.split()[1:3]) for line in present if line.split()[:1] == ['github.com']}
    added = [f'github.com {key}' for key in meta['ssh_keys'] if tuple(key.split()[:2]) not in known]
    if added:
        with path.open('a') as f:
            f.write(''.join(line + '\n' for line in added))
        path.chmod(0o600)
    return len(added)


def clone_tap(brew, tap, env):
    """Tap a public GitHub tap over HTTPS, bypassing the user's URL rewrites.

    Homebrew drops GIT_CONFIG_GLOBAL from git's environment, so `brew tap`
    alone obeys the global https->ssh rewrite and fails before the machine's
    SSH key is registered with GitHub. Cloning the tap ourselves avoids that.
    """
    user, repo = tap.split('/')
    target = Path(subprocess.check_output([brew, '--repository'], text=True).strip()) / 'Library/Taps' / user / f'homebrew-{repo}'
    if target.exists():
        return 0
    target.parent.mkdir(parents=True, exist_ok=True)
    return subprocess.run(['git', 'clone', '--quiet', f'https://github.com/{user}/homebrew-{repo}', str(target)],
                          env=env | {'GIT_CONFIG_GLOBAL': '/dev/null'}).returncode


if __name__ == '__main__':
    raise SystemExit(main())
