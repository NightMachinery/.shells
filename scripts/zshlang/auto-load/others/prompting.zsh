##
function prompt-instruction-input {
    local instruction="$1" ; shift
    assert-args instruction @RET
    local code_block_p="${prompt_code_block_p}"

    local input
    input="$(in-or-args "$@")" @RET

    if bool "$code_block_p" ; then
        input='```'$'\n'"$input"$'\n''```'
    fi

    ec "$instruction"$'\n\n'"$input" |
        cat-copy-if-tty
}
##
function prompt-missing-spaces {
    prompt-instruction-input "Add the missing spaces in the following text:" "$@"
}
##
function prompt-tex {
    prompt-instruction-input 'Write the following snippet using inline LaTeX.' "$@"
    # @bad 'with `(` and `)`. Do not use `$`!'
}

function prompt-tex-yc {
    prompt-instruction-input 'Write the following snippet in LaTeX code.' "$@"
    # ' Separate different parts of the code with newlines.'
}
##
function prompt-correct-grammar {
    prompt-instruction-input 'Correct grammatical and spelling mistakes in the following text:' "$@"
}

function prompt-fluent {
    prompt-instruction-input 'Make the following text more fluent:' "$@"
}
##
function prompt-idea-rewrite {
    prompt-instruction-input 'Rewrite the following ML research proposal more clearly. Be concise.' "$@"
}

function prompt-idea-feedback {
    prompt-instruction-input "What's your feedback on this research proposal?" "$@"
}

function prompt-idea-dataset {
    prompt-instruction-input "Recommend some datasets that are revelevant to the following research proposal." "$@"
}
##
function prompt-translate2en {
    prompt-instruction-input "Translate the following to English. Preserve the original's style and tone. Do not include any additional commentary." "$@"
}

function prompt-translate2per {
    prompt-instruction-input "Translate the following to Persian. Preserve the original's style and tone. Do not include any additional commentary." "$@"
}
##
function prompt-summarize-text {
    prompt-instruction-input "Summarize the following document:" "$@"
}

function prompt-summarize-url {
    #: @seeAlso [agfi:summarize-url]
    ##
    local url="$1"
    assert-args url @RET

    readmoz-md2 "$url" |
        prompt-summarize-text
}
##
function prompt-meaning {
    prompt_code_block_p=y prompt-instruction-input 'What does the following text mean?' "$@"
}

function prompt-define {
    prompt_code_block_p=y prompt-instruction-input "Define the following, and include its IPA pronunciation." "$@"
    #
    # prompt_code_block_p=y prompt-instruction-input "Define the following, and include its IPA pronunciation in American English." "$@"
}

function prompt-define-long {
    prompt_code_block_p=y prompt-instruction-input 'Define the following using the following output format:
```json
{
  "word": "the word to be defined",
  "definitions": [{"definition": "...", "detailed_definition": "...", "IPA_American": "...", "IPA_British": "...", "example_usages": [...]}, ...],
}
```

The `detailed_definition` field should be at least a paragraph long.
The `IPA_American` should be the pronunciation of this word in American English using IPA notation.
The `example_usages` field should include example sentences that use this word in the defined sense.

The following:' "$@"
}
##
