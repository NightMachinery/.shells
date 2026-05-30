##
function deepseek-cc {
    (
        export ANTHROPIC_BASE_URL="https://api.deepseek.com/anthropic"
        export ANTHROPIC_AUTH_TOKEN="${deepseek_api_key}"

        export ANTHROPIC_MODEL="deepseek-v4-pro[1m]"
        export ANTHROPIC_DEFAULT_OPUS_MODEL="deepseek-v4-pro[1m]"
        export ANTHROPIC_DEFAULT_SONNET_MODEL="deepseek-v4-pro[1m]"
        export ANTHROPIC_DEFAULT_HAIKU_MODEL="deepseek-v4-flash"

        export CLAUDE_CODE_SUBAGENT_MODEL="deepseek-v4-flash"
        export CLAUDE_CODE_EFFORT_LEVEL="max"

        $proxyenv exec claude "$@"
    )
}
##
