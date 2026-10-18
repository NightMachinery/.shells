"""Pure foreground argv recognition shared by kitty capture and its test harness."""
import os
import re


def agent_from_command(argv):
    """Recognize Claude/Codex launch commands, including the DECSET proxy."""
    argv = list(argv)
    if argv and os.path.basename(argv[0]) == 'decset-rewrite':
        argv = argv[1:]
        # Parse only the proxy prefix. A -- after the child executable
        # belongs to that child and must not make a later argument an agent.
        while argv and argv[0].startswith('-'):
            if argv[0] == '--':
                argv = argv[1:]
                break
            # Match the legacy option shapes in h-agent-session-cmd-unwrap.
            count = 2 if argv[0] in ('-map', '-trace') else 1
            argv = argv[count:]
    if not argv:
        return ''
    executable = os.path.basename(argv[0])
    if executable in ('node', 'nodejs', 'node.exe'):
        argv = argv[1:]
        # Preserve script launchers in the old first-three-argv window, but
        # never mistake eval/print/require values for the executed script.
        if argv and argv[0] in ('--', '--no-warnings', '--trace-warnings', '--no-deprecation'):
            argv = argv[1:]
        if not argv or argv[0].startswith('-'):
            return ''
        executable = os.path.basename(argv[0])
    if re.fullmatch(r'claude(?:\.exe|-.*)?', executable):
        return 'claude'
    if executable == 'codex':
        return 'codex'
    return ''
