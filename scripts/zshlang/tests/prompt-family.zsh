#!/usr/bin/env zsh
# Run in a fresh loaded shell. Clipboard and network boundaries are fixtures.
(
    setopt local_options pipefail
    unsetopt errexit
    local fixture
    fixture="$(command mktemp -d "${TMPDIR:-/tmp}/prompt-family.XXXXXX")" || exit 1
    trap 'command rm -rf -- "$fixture"' EXIT
    function fail { print -ru2 -- "FAIL: $*"; exit 1; }
    function cat-copy-if-tty { command cat; }
    function cat-paste-if-tty { command cat "$@"; }
    function cat-copy {
        print -r -- copy >> "${fixture}/copies"
        command cat > "${fixture}/clipboard"
        command cat "${fixture}/clipboard"
    }
    function fixture-pass { command cat; }
    function fixture-prefix { print -rn -- 'prefix:'; command cat; }
    function fixture-fail { print -rn -- partial; return 7; }
    local payload=$'  inert € $(printf sentinel)\n\n'
    print -rn -- "$payload" > "${fixture}/input"
    snippet-chain fixture-pass fixture-pass < "${fixture}/input" > "${fixture}/actual" || fail chain
    command cmp -s "${fixture}/input" "${fixture}/actual" || fail chain-whitespace
    snippet-chain < "${fixture}/input" > "${fixture}/actual" || fail empty-chain
    command cmp -s "${fixture}/input" "${fixture}/actual" || fail empty-chain-whitespace
    [[ ! -e "${fixture}/copies" ]] || fail chain-copied
    if snippet-chain fixture-fail fixture-pass < "${fixture}/input" > /dev/null; then fail chain-failure; fi
    snippet-chain-copy fixture-prefix fixture-pass < "${fixture}/input" > "${fixture}/actual" || fail wrapper
    [[ "$(<"${fixture}/copies")" == copy ]] || fail wrapper-copy-count
    command cmp -s "${fixture}/clipboard" "${fixture}/actual" || fail wrapper-output
    command rm -- "${fixture}/copies"
    if snippet-chain-copy fixture-fail fixture-pass < "${fixture}/input" > /dev/null; then fail wrapper-failure; fi
    [[ ! -e "${fixture}/copies" ]] || fail failed-wrapper-copied

    local prompt_input_images=() prompt_preambles=() prompt_input_prefix='' prompt_qa_p=''
    local mode
    for mode in '' none n NONE N NoNe; do
        prompt_input_mode="$mode" prompt-instruction-input INSTRUCTION payload > /dev/null || fail mode-$mode
    done
    for mode in python none-junk junkn; do
        if prompt_input_mode="$mode" prompt-instruction-input INSTRUCTION payload >/dev/null 2>&1; then fail accepted-mode-$mode; fi
    done
    payload=$'alpha\n```\nbeta\n``````\ngamma'
    print -rn -- "$payload" > "${fixture}/fence-input"
    prompt_input_mode=block prompt-instruction-input INSTRUCTION "$payload" > "${fixture}/fenced" || fail fence
    prompt-define-long example > "${fixture}/definition" || fail definition
    command python3 - "${fixture}" <<'PY'
import pathlib,re,json,sys
root=pathlib.Path(sys.argv[1]);text=root.joinpath('fenced').read_text();payload=root.joinpath('fence-input').read_text()
fence='`'*max(3,1+max(map(len,re.findall(r'`+',payload)),default=0))
assert fence+'\n'+payload+'\n'+fence in text
example=re.search(r'```json\n(.*?)\n```',root.joinpath('definition').read_text(),re.S)
parsed=json.loads(example[1]);assert isinstance(parsed['definitions'][0]['example_usages'],list)
PY
    (( $? == 0 )) || fail fence-or-json

    local builder
    for builder in prompt-imdb-recommend prompt-imdb-adaptations prompt-web-define-tex; do
        print -rn -- 'inert fixture request' | "$builder" > "${fixture}/stdin" || fail "$builder-stdin"
        "$builder" 'inert fixture request' > "${fixture}/args" || fail "$builder-args"
        command cmp -s "${fixture}/stdin" "${fixture}/args" || fail "$builder-parity"
        [[ "$(<"${fixture}/stdin")" == *'inert fixture request'* ]] || fail "$builder-input-lost"
    done
    function readmoz-md2 { print -r -- "Fetched fixture for $1"; }
    print -rn -- 'https://example.org/fixture' | prompt-summarize-url > "${fixture}/stdin" || fail url-stdin
    prompt-summarize-url 'https://example.org/fixture' > "${fixture}/args" || fail url-args
    command cmp -s "${fixture}/stdin" "${fixture}/args" || fail url-parity

    local nightNotesPublic="${fixture}/notes"
    command mkdir -p "${nightNotesPublic}/subjects/ML/vision/datasets/ImageNet"
    print -r -- '["fixture label"]' > "${nightNotesPublic}/subjects/ML/vision/datasets/ImageNet/imagenet-simple-labels.json"
    print -rn -- 'fixture elephant' | prompt-coco2imagenet > "${fixture}/stdin" || fail coco-stdin
    prompt-coco2imagenet 'fixture elephant' > "${fixture}/args" || fail coco-args
    command cmp -s "${fixture}/stdin" "${fixture}/args" || fail coco-parity

    local helper
    for helper in prompt-input-file prompt-instruction-input prompt-instruction-input-block prompt-instruction-input-coding \
        prompt-blockify snippet-input snippet-input-file snippet-chain snippet-chain-copy snippet-preamble-gen1 fragment-from-snippet; do
        [[ -z "$(omni-prompter-functions "$helper")" ]] || fail "helper-visible-$helper"
    done
    [[ "$(omni-prompter-functions translate2en)" == *prompt-translate2en* ]] || fail public-builder-hidden
    # Observe only the shared helper's input boundary, with image handling inert.
    function prompt-instruction-input {
        shift
        if (( $# )); then print -r -- "$*"; else command cat; fi
    }
    prompt_input_images=(fixture-image)
    local prompt='fixture image instruction'
    print -rn -- 'image text fixture' | h-prompt-image-v1 > "${fixture}/image-text" || fail image-stdin
    [[ "$(<"${fixture}/image-text")" == 'image text fixture' ]] || fail image-text-lost

    print -r -- 'Prompt-family stdin, pipeline failure, copy boundary, modes, fences, JSON and catalog exclusions passed'
)
