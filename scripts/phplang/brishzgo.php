<?php
// Build a shell command with literal argv for the installed garden client.
function brishzgo_command(array $args): string {
    $binary = getenv('BRISHZGO_BIN') ?: getenv('HOME') . '/go/bin/brishzgo';
    return implode(' ', array_map('escapeshellarg', array_merge([$binary, '--'], $args)));
}
