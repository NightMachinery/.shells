# VPS
- When building/compiling, first check whether the machine is healthy enough for a build; do not wait for 0 load.
- Long-lived app/server processes do not count as active builds by themselves.
- go builds are okay even if server is busy; they are lightweight.

This is a cheap VPS with limited RAM and slow disk. Concurrent Node/npm builds can cause swap thrashing and system hangs. Treat build concurrency as forbidden by default.

Before running any build-related command (`npm`, `pnpm`, `yarn`, `node`, `tsc`, `webpack`, `vite`, `next build`, etc.):

- Check for already-running build/bundling processes.
- Check load average, RAM, and disk pressure.
- Briefly report the preflight result before starting the build.

Treat the machine as healthy enough to start one build only when all of the following are true:

- No other active build/bundling process is already running.
- CPU Load is reasonable:
  - 1-minute load <= 2
  - 5-minute load <= 2
- MemAvailable >= 400 MiB.
- SwapAvailable >= 600 MiB.
- Pressure is calm:
  - `/proc/pressure/io` full avg10 <= 4.0
  - `/proc/pressure/memory` full avg10 <= 1.5
- Disk space availavle >= 0.5GB

If another build is running, or if those thresholds are not met:

- Do not start the build.
- Wait 10 minutes, then check again.
- If repeated checks still fail, stop and explain which threshold(s) are blocking the build instead of forcing it.

- Never run multiple builds at the same time unless I explicitly authorize it.
- Prefer low-concurrency or single-worker modes whenever possible.


Important: Do NOT add this preflight checks into the build scripts; they are something we should run manually ourselves, not code into the build system.
