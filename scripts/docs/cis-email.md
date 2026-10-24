# CIS mail helpers

These functions connect the shell to the separately installed private
`cis-email` skill. The skill owns account-specific instructions and the monitor
implementation. This repository contains no mail identity or notification target.

- [agfi:cis-email-monitor-start] starts a named watcher. Inside Paseo it targets
  the current agent; otherwise pass `--to PASEO_UUID`. Existing mail is baselined
  without notification. Default duration: 72 hours.
- [agfi:cis-email-monitor-status] shows the job, pending count and last check.
- [agfi:cis-email-monitor-stop] stops the named watcher and its subprocesses.
- [agfi:cis-email-monitor-check] scans once; `--no-notify` only collects arrivals.
- [agfi:cis-email-sync] requests the configured mail sync through its LaunchAgent.

```zsh
cis-email-monitor-start replies --hours 72
cis-email-monitor-status replies
cis-email-monitor-stop replies
```

The default skill directory is `~/.agents/skills/cis-email`; override
`cis_email_skill_dir` when installed elsewhere. Runtime state, pending headers
and logs stay under `~/.local/state/cis-email`, outside `$NIGHTDIR`. The helper
rejects a state directory inside this repository or the skill repository.

The monitor uses the existing mail sync. It reads only incoming-message headers,
not bodies or attachments, and notifies the selected agent only when mail arrives.
It defers delivery while the agent is busy. It never sends email. Sending requires
showing the user all recipients, subject, full text and attachments, followed by
explicit approval of that exact message.

Fresh shells load these functions automatically. After changing them, use
`brishz-restart` before testing through BrishGarden.
