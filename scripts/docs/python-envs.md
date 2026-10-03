# Python environments: libraries in conda, tools in uv

Python lives in two separate places, split by what a package is used for.

- **Libraries** (anything a script or notebook imports) go into one conda env.
  On the Mac that is the `py314` env of a Miniforge install at `~/miniforge3`;
  the bootstrap creates the same kind of env on servers (stage
  [[NIGHTDIR:setup/bootstrap/stages/40-conda.sh]], knobs `NIGHT_PY_ENV` and
  `NIGHT_PY_VERSION`). Its `bin` comes first in `PATH`, so it is the default
  `python`. The manifest is `python/requirements.txt`, installed by
  [agfi:ins-pip].
- **Tools** (Python programs used as commands: `yq`, `gallery-dl`, `llm`,
  `brishgarden`, ...) each get their own venv through `uv tool install`, with
  their executables in `~/.local/bin`. The manifest is `python/uv-tools.txt`,
  one argument list per line, installed by [agfi:ins-uv-tools]; add one with
  [agfi:uvtadd].

## Why the split

When everything shared one env, a tool's pins fought every library's pins,
and a Python upgrade had to carry all of them at once. One tool could hold
the whole env back: `watchgod` pinned `anyio` below 4, which broke starlette
and streamlit. With isolated venvs a tool can even stay on an older Python
(`uv-tool-install --python 3.12 foo`) without affecting anything else.

Tool venvs use a uv-managed interpreter (`UV_PYTHON_PREFERENCE=only-managed`
in [agfi:uv-tool-install]), never the conda env, so rebuilding or removing
that env cannot break a tool.

## Conventions

- A tool whose plugins must share its venv takes them as `--with`: `llm`'s
  plugins, for example. A separate `uv tool install llm-foo` would create a
  second venv that `llm` never sees.
- Local checkouts are installed editable, with `~/` paths in the manifest
  (expanded by `ins-uv-tools`). A local tool that imports one of our own
  libraries gets it as `--with-editable`, otherwise uv would resolve it from
  PyPI: BrishGarden uses `--with-editable ~/code/python/brish`.
- A tool that Homebrew already ships (`ocrmypdf`, `googler`,
  `speedtest-cli`) stays a brew formula, not a uv tool.
- `pi` ([agfi:pip-install]) installs into the env that owns the `python3`
  on `PATH`, through [agfi:uv-pip].

## Upgrading Python

1. Create a new env for the new version and reinstall the libraries into it.
   Install them one at a time, so a single package without wheels for the new
   Python fails alone instead of aborting the whole batch.
2. Move the tools with `uv python install X.Y` and then
   `uv tool upgrade --all --python X.Y`, then run each tool once. A tool
   that cannot move gets `--python` pinned on its manifest line.
3. Point `PATH` at the new env in `~/.shared.sh`, run `brishz-restart`, and
   smoke-test the tools.

Check which interpreter uv actually used, with `uv python list
--only-installed`: during the 3.14 move it silently picked a stale managed
alpha (3.14.0a5) until a stable 3.14 was installed and the alpha removed.
