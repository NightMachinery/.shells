# LLM Models for `llm-send`, `xz` and the Prompt Functions

The models are defined in `zshlang/auto-load/others/openai.zsh` (`## * Models`). Each `define-llm-model-v2` call takes a short name and generates `with-<short>` (prefix a command to use that model), `llm-<short>`, `llm-<short>-chat` and `reval-to-<short>`, plus the aliases listed with it. Prefix any prompt function to pick a model: `with-sol 2en '...'`.

- `flash`: Gemini 3.8 Flash on Vertex AI. **The default** (`llm_default_model`).
- `argon`: Gemini 4 Argon on Vertex AI.
- `luna`, `terra`, `sol`, `astra`: GPT-6 Luna, 6.1 Terra, 6.1 Sol and 6 Astra through the Codex subscription (`llm-openai-via-codex`). `codex-models-ls` lists what it offers.
- `opus`: Claude Opus 5.5 through OpenRouter, billed per token.

## Vertex

The Vertex models use [llm-vertex-genai](https://github.com/NightMachinery/llm-vertex-genai), our fork of `llm-vertex` that adds attachments (images for `xzz`, audio for STT), conversation history and any model id. Install it with:

```zsh
llm uninstall llm-vertex   #: same model ids, so remove it first
llm install https://github.com/NightMachinery/llm-vertex-genai/archive/refs/heads/main.zip
```

It authenticates with `gcloud auth application-default login`. `llm-m` sets `VERTEX_PROJECT_ID` to `gcp_pa_project` (the purple-aura project, from `~/.night-gcp/config.zsh`) unless it is already set. The `gemini-purple-aura` API key does not work here: it is restricted to the AI Studio API. Use `with-purple-aura-gemini` with an AI Studio model (`llm_model=gemini-3.8-flash`) for that route.

Add a new Vertex model either to the plugin's default list or with `LLM_VERTEX_MODELS=id1,id2`; each becomes `vertex-<id>`.
