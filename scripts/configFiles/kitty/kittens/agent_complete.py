"""No-UI screen capture only. All computation and IO run in a detached worker."""
import json
import os
import runpy
import subprocess
import tempfile
from kittens.tui.handler import result_handler

# Custom kittens are executed without __file__. Use the same repository root
# convention as the detached worker path below; run_path also avoids stale imports.
_ROOT = os.environ.get('NIGHTDIR', os.path.expanduser('~/scripts'))
agent_from_command = runpy.run_path(os.path.join(
    _ROOT, 'configFiles/kitty/kittens/agent_complete_command.py'))['agent_from_command']


def main(args):
    pass


def agent_name(window):
    # Foreground matching selects mappings; foreground binaries decide whether insertion is safe.
    for process in window.child.foreground_processes:
        agent = agent_from_command(process.get('cmdline', []))
        if agent:
            return agent, process['pid']
    return '', 0


def show_error(boss, window, message):
    # Remote kitten dispatch does not set the same context as a mapped key.
    # Keep its existing error overlay attached to the requested scratch/agent
    # window, even while another OS window has focus.
    previous = boss.window_for_dispatch
    try:
        boss.window_for_dispatch = window
        boss.show_error('Agent completion', message)
    finally:
        boss.window_for_dispatch = previous


@result_handler(no_ui=True)
def handle_result(args, answer, target_window_id, boss):
    w = boss.window_id_map.get(target_window_id)
    if w is None:
        return
    if len(args) > 1 and args[1] == 'status':
        show_error(boss, w, args[2] if len(args) > 2 else 'Completion failed')
        return
    agent, process_id = agent_name(w)
    if not agent:
        # A stale agent title must not consume zsh's own Alt-. widget.
        key = b'\x1b.' if len(args) > 1 and args[1] == 'fim' else b'\x1b/'
        w.write_to_child(key)
        return
    root = _ROOT
    request = {
        'screen': {'agent': agent, 'process_id': process_id, 'cursor_x': w.screen.cursor.x, 'cursor_y': w.screen.cursor.y, 'columns': w.screen.columns,
                   'lines': [{'text': str(w.screen.line(i)),
                              'wrapped': w.screen.line(i).last_char_has_wrapped_flag()}
                             for i in range(w.screen.lines)]},
        'others': [other.as_text() for other in sorted(boss.window_id_map.values(),
                   key=lambda other: other.tab_id != w.tab_id)
                   if other.id != w.id and other.os_window_id == w.os_window_id],
        'source': 'kitty', 'target': str(w.id), 'kitty_pid': os.getpid(),
        'socket': 'unix:' + os.path.expanduser(f'~/.local/state/kitty-{os.getpid()}.sock'),
        'cwd': w.child.foreground_cwd or w.child.current_cwd,
        'kitten': os.path.join(root, 'configFiles/kitty/kittens/agent_complete.py'),
    }
    # A file descriptor avoids a blocking pipe write in kitty's main thread.
    try:
        with tempfile.TemporaryFile() as f:
            f.write(json.dumps(request).encode()); f.seek(0)
            subprocess.Popen([os.path.join(root, 'bin/agent-complete.zsh'),
                              args[1] if len(args) > 1 else 'dabbrev'],
                             stdin=f, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL,
                             start_new_session=True)
    except OSError as error:
        show_error(boss, w, str(error))
