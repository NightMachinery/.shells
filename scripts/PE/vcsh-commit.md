Use `vcsh night.sh` and commit changes atomically. Ignore untracked files outside `$NIGHTDIR`. Read the diff, group related changes in small meaningful units and stage and commit them in atomic groups. Sometimes changes in a single file should be split into multiple commits.

## Workflow

1. Check status: `vcsh night.sh status $NIGHTDIR`
2. Review all changes: `vcsh night.sh diff`  
3. Group related changes logically
4. Stage and commit each group with meaningful messages
5. Verify final status

## Commit Message Guidelines

- Use conventional commit format (feat:, fix:, refactor:, etc.)
- Focus on the "why" rather than the "what"
- Be concise but descriptive
- Group related changes in atomic commits

## Common Change Categories

- **Telegram/Communication**: Variable renames, function updates
- **Debug/Development**: Function aliases, debugging tools  
- **Configuration**: Proxy settings, app defaults
- **Features**: New functions, enhanced workflows
- **Refactoring**: Code organization, structure improvements
