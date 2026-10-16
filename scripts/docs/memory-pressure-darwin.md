# memory-pressure-darwin

Run `memory-pressure-darwin` to print the current macOS memory pressure:

```text
Memory pressure: Normal
```

The helper lives beside `memory-free-get` in
`zshlang/auto-load/others/monitor/monitor.zsh`. It reads
`/usr/sbin/sysctl -n kern.memorystatus_vm_pressure_level`, without sudo or caching.
Values `1`, `2`, and `4` print `Normal`, `Warning`, and `Critical`, respectively.
These are [Apple's exported pressure constants](https://github.com/apple-oss-distributions/xnu/blob/main/bsd/sys/event_private.h).
The [sysctl handler](https://github.com/apple-oss-distributions/xnu/blob/main/bsd/kern/kern_memorystatus_notify.c)
converts internal pressure levels to these exported values.

Every recognized level returns zero, including critical: this is a reporting
command, not a health predicate. An unfamiliar value prints
`Memory pressure: Unknown (<value>)` and returns nonzero. Unsupported platforms
and failed queries report an error on stderr, return nonzero, and print no
pressure level. There are no options or background monitoring.
