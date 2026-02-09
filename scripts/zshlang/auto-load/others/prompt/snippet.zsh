##
function snippet-input-file {
    local file="$1"
    assert-args file @RET
    shift @RET

    ##
    #: @duplicateCode/1df1db85fd5d5e5e106ad354237f763b
    #: We are losing the whitespace at the end of files deliberately, as keeping the whitespace at the end correct is difficult. emacs can delete trailing whitespace and add unwanted new lines at the end.
    local instruction
    instruction="$(h-read-prompt-file "${file}")" @TRET
    # instruction="${$(h-read-prompt-file "${file}" ; ecn .)[1,-2]}" @TRET
    ##

    snippet-input "${instruction}" "${@}"
}

function snippet-input {
    local snippet="${1}"
    shift @RET

    prompt_preambles=() prompt_qa_p= prompt_input_mode="${snippet_input_mode:-none}" prompt-instruction-input "${snippet}" "$@"
}
##
function snippet-preamble-gen1 {
    local field="$1"
    shift @RET

    if test -n "$field" ; then
        field=" in ${field}"
    fi

    snippet-input "You are an experienced expert${field}. You always answer questions to the best of your knowledge, but you NEVER provide answers that you are not sure about, or that are not backed up by high-quality sources; instead you say that you can't provide a good answer. Giving trustworthy and correct answers is much more important to you than always having something to say. You keep your answers concise, on-topic, free of boilerplate, and exclude basic instructions that most developers will be familiar with anyway, unless the user asks for more details." "$@"
}

function snippet-preamble-coding {
    local snippet
    snippet="$(
    ec "You keep your answers concise, on-topic, free of boilerplate, and exclude basic instructions that most developers will be familiar with anyway. You write clean, performant code in a functional style. Most of your code is in small functions which take any needed inputs as possibly optional arguments. You use abstractions to DRY. You use dependecy injection when possible. You include type hints when possible. You write docstrings and put example usages in the docstrings. You're an expert in Python, Rust, Go, Haskell, Clojure, Elisp, and Scala."

    ec "In Python, you use \`*, \` style for functions with multiple arguments to force the caller to explicitly name any used argument for more readable and robust code. You always end your argument lists with a comma so that the linter formats the code correctly."

    ec
    # snippet-preamble-coding-rewriter ''
    snippet-whole-code ''
    )"

    snippet-input "${snippet}" "$@"
}

function prompt-coding-rewriter {
    fnswap snippet-preamble-coding-rewriter true \
        prompt-instruction-input-coding "You refactor the functions if a lower-level sub-function can be extracted with a more general API. You optimize the code to make it run faster." "$@"
    # You remove useless comments.
}

function snippet-preamble-coding-rewriter {
    local snippet
    snippet="After writing a code snippet, you always rewrite it to make it better in the following ways. $(prompt-coding-rewriter '')" @TRET

    snippet-input "${snippet}" "$@"
}
##
function snippet-stt-coding {
    snippet-input-file "${night_prompt_dir}/STT_coding.md" "$@"
}
##
function snippet-whole-code {
    snippet-input "Output the whole code without eliding any parts." "$@"
}

function snippet-programming {
    snippet-input-file "${night_prompt_dir}/programming.org" "$@"
}

function snippet-python {
    snippet-input-file "${night_prompt_dir}/Python.org" "$@" |
        snippet-programming
}

function snippet-elisp {
    snippet-input-file "${night_prompt_dir}/elisp.org" "$@" |
        snippet-programming
}

function snippet-zsh {
    snippet-input-file "${night_prompt_dir}/Zsh.org" "$@" |
        snippet-programming
}

function snippet-PE {
    snippet-input-file "${night_prompt_dir}/Zsh_PE.org" "$@" |
        snippet-zsh
}

function snippet-sop {
    snippet-input-file "${night_prompt_dir}/SOP.org" "$@"
}
##
function snippet-night-namespace {
    snippet-input "Prefix all of our new function names' with \`night/\` to namespace them properly." "$@"
}
##
function snippet-another-llm {
    snippet-input "You don't know the answer. Write the question completely so I can ask a better LLM. Only output the question in Markdown, as I'll directly copy your message to the other LLM." "$@"
}

function snippet-another-llm-v2 {
    snippet-input "Can you summarize the context and my concerns in a markdown code block that I can directly copy and ask another LLM for advice? Include all needed context, length is not an issue." "$@"
}

function snippet-replace-files {
    snippet-input "You have given me one big block of text which is hard to apply to all these files. Perhaps you can give Zsh code with 'EOF' that overwrites these files instead?" "$@"
}

function snippet-multi {
    snippet-input "Give 5 different answers, with different trade-offs in each." "$@"
}

function snippet-ask-questions {
    snippet-input "You can ask me any questions you have first." "$@"
}
function snippet-ask-questions-v1 {
    snippet-input "Before responding, ask me any clarifying questions about information you need." "$@"

    # snippet-input "First ask me questions to help you give me the best response." "$@"
}

function snippet-latex-labels {
    snippet-input "Add proper labels to anything we might want to refer to. Don't change anything else." "$@"
}

function snippet-disable-artifacts {
    snippet-input "Do not use artifacts, directly output in a code block." "$@"
}
##
function snippet-zsh-heredoc-file-updater {
    snippet-input "Give the Zsh code to update the changed files using a literal heredoc. Do not use artifacts." "$@"
}
##
function fragment-from-snippet {
    local snippet_name="${1}"
    if ! (( $+functions[${snippet_name}] )) ; then
        ecgray "$0: Snippet not defined: ${snippet_name}"
        return 1
    fi

    local fragment_name="fragment-${snippet_name#snippet-}"
    functions[${fragment_name}]="${snippet_name} ''"
}


local snippets_to_fragmentize=(
    'another-llm'
    'replace-files'
    'multi'
    'latex-labels'
    'ask-questions'
    'ask-questions-v1'
    'snippet-disable-artifacts'
    snippet-sop
    snippet-zsh-heredoc-file-updater
)

local snippet_name
for snippet_name in "${snippets_to_fragmentize[@]}"; do
    if ! [[ "${snippet_name}" =~ '^snippet-' ]] ; then
        snippet_name="snippet-${snippet_name}"
    fi

    fragment-from-snippet "${snippet_name}"
done
##
