function lock-hook {
    # @todo1/security Currently implemented via `deluna', which actually runs it when the computer is unlocked, and also runs it multiple times and when idle etc.
    ##
    killall gpg-agent || true
}
