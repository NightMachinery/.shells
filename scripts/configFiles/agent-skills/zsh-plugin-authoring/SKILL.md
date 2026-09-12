---
name: zsh-plugin-authoring
description: Create or extract independently loadable zsh plugins in the NightMachinary scripts repository, using its shared basics and subdirectory import pattern. Use when packaging shell code for reuse outside the full personal shell setup.
---

# Zsh plugin authoring

Treat paths below as relative to the scripts checkout (`${NIGHTDIR}` or
`~/scripts`). Read its `AGENTS.md` and `PE/Zsh.org` before editing.

1. Read `docs/zsh_plugins.md`, `zshlang/basic/README.md`, and the Paqet
   loader and README under `zshlang/plugins/paqet/`. The Zinit example in
   `setup/minimal_proxy/gen.org` shows the existing subdirectory import setup.
2. Trace dependencies before extracting. Keep shared behavior in one place;
   have existing callers use it. Keep personal configuration and unrelated
   integrations outside the public plugin. Reuse helpers exposed by
   `basic.plugin.zsh`; do not import the full shell just to satisfy a helper.
3. Use `zshlang/plugins/<name>/<name>.plugin.zsh` as a thin, idempotent loader
   with a namespaced guard. Resolve sibling imports with `${${(%):-%x}:A:h}`;
   source the minimal basic plugin when needed, then separate implementation
   files. Propagate load failures and allow a failed load to be retried.
4. Add the plugin to the explicit list in `zshlang/load-others.zsh`. Avoid
   duplicate definitions in auto-loaded files and preserve the full local
   basic stack's richer definitions. Keep sourcing free of runtime actions.
5. Follow the existing plugin README style: commands, dependencies, configuration,
   direct sourcing, and plugin-manager imports. For Zinit, retain `aliases`
   so global aliases expand during parsing; use `pick` and `multisrc` for
   selected entrypoints. Subdirectory distribution keeps one source of truth
   but remains an explicit installation dependency; do not fetch it at launch.
6. Verify direct loading in `zsh -f` without personal startup files, repeated
   sourcing, missing-dependency errors, and representative command behavior.
   Check local integration too; a successful source alone does not establish
   standalone operation. Update `docs/` and the repository README as needed.
