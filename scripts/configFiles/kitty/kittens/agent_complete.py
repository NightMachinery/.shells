"""No-UI screen capture only. All computation and IO run in a detached worker."""
import json
import os
import re
import subprocess
import tempfile
from kittens.tui.handler import result_handler


def main(args):
    pass


def agent_name(window):
    # Titles select mappings; foreground binaries decide whether insertion is safe.
    for process in window.child.foreground_processes:
        names = [os.path.basename(x) for x in process.get('cmdline', [])[:3]]
        if any(re.fullmatch(r'claude(?:\.exe|-.*)?', x) for x in names):
            return 'claude'
        if any(x == 'codex' for x in names):
            return 'codex'
    return ''


@result_handler(no_ui=True)
def handle_result(args, answer, target_window_id, boss):
    if len(args) > 1 and args[1] == 'status':
        boss.show_error('Agent completion', args[2] if len(args) > 2 else 'Completion failed')
        return
    w = boss.window_id_map.get(target_window_id)
    if w is None:
        return
    agent = agent_name(w)
    if not agent:
        # A stale agent title must not consume zsh's own Alt-. widget.
        key = b'\x1b.' if len(args) > 1 and args[1] == 'fim' else b'\x1b/'
        w.write_to_child(key)
        return
    root = os.environ.get('NIGHTDIR', os.path.expanduser('~/scripts'))
    request = {
        'screen': {'agent': agent, 'cursor_x': w.screen.cursor.x, 'cursor_y': w.screen.cursor.y,
                   'lines': [{'text': str(w.screen.line(i)),
                              'wrapped': w.screen.line(i).last_char_has_wrapped_flag()}
                             for i in range(w.screen.lines)]},
        'others': [other.as_text() for other in boss.window_id_map.values()
                   if other.id != w.id and other.os_window_id == w.os_window_id],
        'source': 'kitty', 'target': str(w.id), 'kitty_pid': os.getpid(),
        'socket': 'unix:' + os.path.expanduser(f'~/.local/state/kitty-{os.getpid()}.sock'),
        'cwd': w.child.foreground_cwd or w.child.current_cwd,
        'kitten': os.path.join(root, 'configFiles/kitty/kittens/agent_complete.py'),
    }
    # A file descriptor avoids a blocking pipe write in kitty's main thread.
    with tempfile.TemporaryFile() as f:
        f.write(json.dumps(request).encode()); f.seek(0)
        subprocess.Popen([os.path.join(root, 'bin/agent-complete.zsh'),
                          args[1] if len(args) > 1 else 'dabbrev'],
                         stdin=f, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL,
                         start_new_session=True)
