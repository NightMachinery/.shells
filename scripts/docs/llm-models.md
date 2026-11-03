# LLM Models for `llm-send`, `xz` and the Prompt Functions

The models are defined in `zshlang/auto-load/others/openai.zsh` (`## * Models`). Each `define-llm-model-v2` call takes a short name and generates `with-<short>` (prefix a command to use that model), `llm-<short>`, `llm-<short>-chat` and `reval-to-<short>`, plus the aliases listed with it. Prefix any prompt function to pick a model: `with-sol 2en '...'`.

- `flash`: Gemini 3.8 Flash on Vertex AI. **The default** (`llm_default_model`).
- `argon`: Gemini 4 Argon on Vertex AI.
- `luna`, `terra`, `sol`, `astra`: GPT-6 Luna, 6.1 Terra, 6.1 Sol and 6 Astra through the Codex subscription (`llm-openai-via-codex`). `codex-models-ls` lists what it offers.
- `opus`: Claude Opus 5.5 through OpenRouter, billed per token.

## Effort

Set `llm_effort` to choose how hard a model thinks: `llm_effort=low with-flash 2en '...'`. `llm-m` sends it as `thinking_level` to Vertex (`low`, `medium`, `high`; Flash 3.8 and Argon reject `minimal`) and as `reasoning_effort` to Codex (`low` to `xhigh`, plus `max`) and OpenRouter. Unset, nothing is sent and the provider's default applies: `medium` for Codex; for Opus, effort `high` with extended thinking off; for Gemini, roughly `high` on Flash, while Argon decides for itself. Unsupported values are passed through and rejected by the plugin, never clamped.

A model's `effort` field overrides the provider default, and an explicit `llm_effort` overrides that. `sol` and `argon` default to `high`. Its `effort_variants` add shortcuts: `with-luna-high`, `with-luna-max`, `with-sol-low`, `with-astra-low`. A variant sets `llm_effort` itself, so its name wins: `llm_effort=max with-luna-high` sends `high`.

For any model, `with-minimal`, `with-low`, `with-medium`, `with-high`, `with-xhigh` and `with-max` set `llm_effort` (`with-high with-luna xz ...`), and each `-maybe` form (`with-high-maybe`) sets it only when it is unset.

Codex's `max` needs [llm-openai-via-codex#6](https://github.com/simonw/llm-openai-via-codex/pull/6). Until it merges, install the fork: `llm install https://github.com/NightMachinery/llm-openai-via-codex/archive/refs/heads/reasoning-effort-max.zip`.

## Vertex

The Vertex models use [llm-vertex-genai](https://github.com/NightMachinery/llm-vertex-genai), our fork of `llm-vertex` that adds attachments (images for `xzz`, audio for STT), conversation history and any model id. Install it with:

```zsh
llm uninstall llm-vertex   #: same model ids, so remove it first
llm install https://github.com/NightMachinery/llm-vertex-genai/archive/refs/heads/main.zip
```

It authenticates with `gcloud auth application-default login`. `llm-m` sets `VERTEX_PROJECT_ID` to `gcp_pa_project` (the purple-aura project, from `~/.night-gcp/config.zsh`) unless it is already set. The `gemini-purple-aura` API key does not work here: it is restricted to the AI Studio API. Use `with-purple-aura-gemini` with an AI Studio model (`llm_model=gemini-3.8-flash`) for that route.

Add a new Vertex model either to the plugin's default list or with `LLM_VERTEX_MODELS=id1,id2`; each becomes `vertex-<id>`.
