'''A config module for the Kitty Theme Changer Tool.'''

from pathlib import Path
from os import environ, getpid
from psutil import process_iter

conf_dir = Path('~/.config/kitty').expanduser()

theme_dir = Path('~/.config/kitty/kitty-themes/themes').expanduser()
# theme_dir = conf_dir.joinpath('themes')

theme_link = conf_dir.joinpath('theme.conf')
light_theme_link = conf_dir.joinpath('light-theme.conf')
dark_theme_link = conf_dir.joinpath('dark-theme.conf')

def kitty_pid():
  ps = {x.pid: x for x in process_iter(['name', 'pid', 'ppid'])}
  cp = ps[getpid()]
  while cp.name() != 'kitty':
     cp = cp.parent()
  return cp.pid

#: $NIGHT_SOCKETS_DIR, with the literal as a fallback: this module is imported
#: by a tool that kitty may launch itself, and kitty's own environment carries
#: no shell variables. Keep in step with `listen_on' in kitty.conf.
sockets_dir = Path(environ.get('NIGHT_SOCKETS_DIR') or '~/.local/state').expanduser()

socket = 'unix:' + str(sockets_dir / 'kitty-{}.sock'.format(kitty_pid()))
