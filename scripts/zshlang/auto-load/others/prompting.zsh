##
function prompt-instruction-input {
    local instruction="$1" ; shift
    local code_block_p="${prompt_code_block_p}"
    local qa_p="${prompt_qa_p}"
    local preambles=(${prompt_preambles[@]})

    local input
    input="$(in-or-args "$@" | erase-ansi)" @RET

    if test -n "$input" ; then
        if bool "$code_block_p" ; then
            input='```'$'\n'"$input"$'\n''```'
        fi
        if bool "${qa_p}" ; then
            input='Q:'$'\n'"$input"$'\n\n''A:'
        fi
    fi

    {
        for preamble in "${preambles[@]}" ; do
            reval "${preamble}"
        done

        ec "$instruction"

        if test -n "$input" ; then
            ec $'\n'"$input"
        fi
    } |
        strip-blank-lines-start-end |
        cat-copy-if-tty
}

function prompt-instruction-input-coding {
    prompt_code_block_p="${prompt_code_block_p:-y}" \
    prompt_preambles=(${prompt_preambles[@]} preamble-coding) \
        prompt-instruction-input "$@"
}

function prompt-blockify {
    : "Use our Hammerspoon function 'pasteBlockified' instead"

    prompt_code_block_p=y prompt-instruction-input ''
}
alias xb='prompt-blockify'
##
function preamble-gen1 {
    local field="$1"

    if test -n "$field" ; then
        field=" in ${field}"
    fi

    ec "You are an experienced expert{field}. You always answer questions to the best of your knowledge, but you NEVER provide answers that you are not sure about, or that are not backed up by high-quality sources; instead you say that you can't provide a good answer. Giving trustworthy and correct answers is much more important to you than always having something to say. You keep your answers concise, on-topic, free of boilerplate, and exclude basic instructions that most developers will be familiar with anyway, unless the user asks for more details." |
        cat-copy-if-tty
}

function preamble-coding {
    {
    ec "You keep your answers concise, on-topic, free of boilerplate, and exclude basic instructions that most developers will be familiar with anyway. You write clean, performant code in a functional style. Most of your code is in small functions which take any needed inputs as possibly optional arguments. You include type hints when possible. You write docstrings and put example usages in the docstrings. You're an expert in Python, Clojure, Elisp, and Scala."

    ec
    preamble-coding-rewriter
    } |
        cat-copy-if-tty
}

function prompt-coding-rewriter {
    fnswap preamble-coding-rewriter true \
        prompt-instruction-input-coding "You refactor the functions if a lower-level sub-function can be extracted with a more general API. You optimize the code to make it run faster. You remove useless comments." "$@"
}

function preamble-coding-rewriter {
    ec "After writing a code snippet, you always rewrite it to make it better in the following ways. $(prompt-coding-rewriter '')" |
        cat-copy-if-tty
}
##
function prompt-coding-rewrite-performant {
    prompt_code_block_p=y prompt-instruction-input 'Rewrite the following code to make it faster, optimized and performant. Use best practices.'
}
##
function prompt-coding-correct-mistakes {
    prompt_code_block_p=y prompt-instruction-input "Correct any mistakes in the following code and suggest ways it can be optimized to run faster or be written more idiomatically and cleanly." "$@" |
        cat-copy-if-tty
}
##
function prompt-missing-spaces {
    prompt-instruction-input "Add the missing spaces in the following text:" "$@"
}
##
function html4latex-clean {
    in_or_args_html_p=y in-or-args "$@" |
        perl -CS -ple 's/\x{FFFD}//g' |
        html4latex_clean.py |
        cat-copy-if-tty
    #: U+FFFD: replacement character appears when a Unicode character can't be represented in the text's current encoding
}


function prompt-html2mdtex {
    #: @GPT3.5 mostly works
    ##
    html4latex-clean "$@" |
        prompt_code_block_p=y prompt-tex
}

function prompt-html2orgtex {
    #: This doesn't work with GPT3.5, but it works with GPT4.
    #: It might work with the examples added.
    ##
    local examples=""

    #: This example has links stripped out.
    examples+=$'\n\n''Q:
```html
<p>the<span> </span><b>singular values</b>, or<span> </span><b><i>s</i>-numbers</b><span> </span>of a<span> </span><a href="https://en.wikipedia.org/wiki/Compact_operator" title="Compact operator">compact operator</a><span> </span><span><span><math alttext="{\displaystyle T:X\rightarrow Y}"><semantics><mrow><mstyle><mi></mi><mo>:</mo><mi></mi><mo>→</mo><mi></mi></mstyle></mrow></semantics></math></span><img/></span><span> </span>acting between<span> </span><a href="https://en.wikipedia.org/wiki/Hilbert_space" title="Hilbert space">Hilbert spaces</a><span> </span><span><span><math alttext="{\displaystyle X}"><semantics><mrow><mstyle><mi></mi></mstyle></mrow></semantics></math></span><img/></span><span> </span>and<span> </span><span><span><math alttext="{\displaystyle Y}"><semantics><mrow><mstyle><mi></mi></mstyle></mrow></semantics></math></span><img/></span>, are the square roots of the (necessarily non-negative)<span> </span><a href="https://en.wikipedia.org/wiki/Eigenvalue" title="Eigenvalue">eigenvalues</a><span> </span>of the self-adjoint operator<span> </span><span><span><math alttext="{\displaystyle T^{*}T}"><semantics><mrow><mstyle><msup><mi></mi><mrow><mo>∗</mo></mrow></msup><mi></mi></mstyle></mrow></semantics></math></span><img/></span><span> </span>(where<span> </span><span><span><math alttext="{\displaystyle T^{*}}"><semantics><mrow><mstyle><msup><mi></mi><mrow><mo>∗</mo></mrow></msup></mstyle></mrow></semantics></math></span><img/></span><span> </span>denotes the<span> </span><a href="https://en.wikipedia.org/wiki/Adjoint_operator" title="Adjoint operator">adjoint</a><span> </span>of<span> </span><span><span><math alttext="{\displaystyle T}"><semantics><mrow><mstyle><mi></mi></mstyle></mrow></semantics></math></span><img/></span>).</p><p>The singular values are non-negative<span> </span><a href="https://en.wikipedia.org/wiki/Real_number" title="Real number">real numbers</a>, usually listed in decreasing order (<i>σ</i><sub>1</sub>(<i>T</i>),<span> </span><i>σ</i><sub>2</sub>(<i>T</i>), …). The largest singular value<span> </span><i>σ</i><sub>1</sub>(<i>T</i>) is equal to the<span> </span><a href="https://en.wikipedia.org/wiki/Operator_norm" title="Operator norm">operator norm</a><span> </span>of<span> </span><i>T</i><span> </span>(see<span> </span><a href="https://en.wikipedia.org/wiki/Min-max_theorem#Min-max_principle_for_singular_values" title="Min-max theorem">Min-max theorem</a>).</p>
```

A:
```org
The *singular values*, or *s*-numbers of a compact operator \(T:X\rightarrow Y\) acting between Hilbert spaces \(X\) and \(Y\), are the square roots of the (necessarily non-negative) eigenvalues of the self-adjoint operator \(T^{*}T\) (where \(T^{*}\) denotes the adjoint of \(T\)).

The singular values are non-negative real numbers, usually listed in decreasing order (\(\sigma_1(T), \sigma_2(T), \dots\)). The largest singular value \(\sigma_1(T)\) is equal to the operator norm of \(T\) (see Min-max theorem).
```

Q:
```
<span>Even if<span> </span></span><span></span><span><nobr><span><span><span><span><span>𝐴</span><span>,</span><span>𝐵</span></span><span></span></span></span><span></span></span></nobr><span><math><mi></mi><mo>,</mo><mi></mi></math></span></span><span><span> </span>are<span> </span></span><span></span><span><nobr><span><span><span><span><span>𝑛</span><span>×</span><span>𝑛</span></span><span></span></span></span><span></span></span></nobr><span><math><mi></mi><mo>×</mo><mi></mi></math></span></span><span><span> </span>identity matrices,<span> </span></span><span></span><span><nobr><span><span><span><span><span>det</span><span>(</span><span>𝐴</span><span>+</span><span>𝐵</span><span>)</span><span>=</span><span><span><span><span>2</span><span></span></span><span><span>𝑛</span><span></span></span></span></span></span><span></span></span></span><span></span></span></nobr><span><math><mo>det</mo><mo>(</mo><mi></mi><mo>+</mo><mi></mi><mo>)</mo><mo>=</mo><msup><mn>2</mn><mi></mi></msup></math></span></span><span><span> </span>while<span> </span></span><span></span><span><nobr><span><span><span><span><span>det</span><span>(</span><span>𝐴</span><span>)</span><span>=</span><span>det</span><span>(</span><span>𝐵</span><span>)</span><span>=</span><span>1</span></span></span></span></span></nobr></span>
```

A:
```org
Even if \(A\), \(B\) are \(n \times n\) identity matrices, \(\text{det}(A + B) = 2^n\) while \(\text{det}(A) = \text{det}(B) = 1\).
```
'$'\n\n'

    html4latex-clean "$@" |
        prompt_code_block_p=y prompt_qa_p=y prompt-instruction-input "Write the following HTML snippet as org-mode with inline LaTeX. Exclude citation links.${examples}" "$@"
}

function prompt-tex {
    prompt-instruction-input 'Write the following snippet using inline LaTeX.' "$@"
    # @bad 'with `(` and `)`. Do not use `$`!'
}

function prompt-tex-yc {
    prompt-instruction-input 'Write the following snippet in LaTeX code.' "$@"
    # ' Separate different parts of the code with newlines.'
}

function prompt-web-define-tex {
    local url="$1" query="$2"

    ec "Using the URL [${url}], write the definition of ${query} using inline LaTeX." |
        cat-copy-if-tty
}

function prompt-tex2plain {
    prompt_code_block_p=y prompt-instruction-input 'Convert the following document from LaTeX into plain text.'
}
##
function prompt-correct-grammar {
    prompt-instruction-input 'Correct grammatical and spelling mistakes in the following text:' "$@"
}

function prompt-rewrite-fluent {
    prompt-instruction-input 'Make the following text more fluent:' "$@"
}

function prompt-rewrite-fluent-orgmode {
    prompt_code_block_p=y prompt-instruction-input 'Make the following org-mode text more fluent:' "$@"
}

function prompt-rewrite-fluent-orgbeamer {
    prompt_code_block_p=y prompt-instruction-input 'Make the following org-mode beamer presentation more fluent:' "$@"
}

function prompt-rewrite-fluent-selective {
    prompt-instruction-input 'Make the following text more fluent by editing the part marked with `{ ... }`:' "$@"
}
##
function prompt-issue-rewrite {
    prompt-instruction-input 'Rewrite the following issue more clearly. Suggest a title for the issue.' "$@"
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
function prompt-pronunciation {
    prompt_code_block_p=y prompt-instruction-input "How is the following pronounced? Include IPA." "$@"
    #
    # prompt_code_block_p=y prompt-instruction-input "Define the following, and include its IPA pronunciation in American English." "$@"
}
aliasfn prompt-say prompt-pronunciation

function prompt-explain-meaning {
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
function prompt-explain-code {
    prompt_code_block_p=y prompt-instruction-input 'Explain the following code:' "$@"
}

function prompt-simplify-code {
    prompt_code_block_p=y prompt-instruction-input 'Simplify the following code:' "$@"
}
##
function prompt-refactor-to-functions {
    prompt-instruction-input-coding "Refactor the following code and break it up into smaller functions, each with a clear purpose." "$@"
}
##
function prompt-shell-long-args {
    prompt_code_block_p=y prompt-instruction-input 'Rewrite the following shell code using long argument forms instead of short ones. E.g.,

Q: `curl -I ...`
A: `curl --head ...`

Only output the rewritten code.' "$@"
}
##
function prompt-explain-stacktrace {
    prompt_code_block_p=y prompt-instruction-input "The stacktrace of an error follows. What might be the problem?" "$@"
}

aliasfn xss prompt-explain-stacktrace
##
function prompt-code-complete {
    prompt_code_block_p=y prompt-instruction-input "Give three likely completions for the following code snippet. Output each candidate completion in a code block. Don't output anything else. Your completions should be complete, production-ready code." "$@"
}

function prompt-py-to-kwargs {
    prompt_code_block_p=y prompt-instruction-input "Convert all arguments to keyword arguments." "$@"
}
##
function prompt-rewrite-as-script {
    prompt-instruction-input "Rewrite the following as a noninteractive Zsh script. Your reply should be a single code block contaning the Zsh script." "$@"
}
##
function prompt-docstring-write {
    prompt_code_block_p=y prompt-instruction-input "Add or update the docstrings of important things to the following." "$@"
}
##
function prompt-proofread-email {
    prompt-instruction-input "Proofread the following email. Make sure it's polite and professional." "$@"
}
##
function prompt-learn-papers {
    prompt-instruction-input "I want to learn about the following concept(s). Which papers do you recommend?" "$@"
}
##
function prompt-rewrite-formal {
    prompt_code_block_p=y prompt-instruction-input "Rewrite the following in a formal manner."
}
##
function prompt-2json {
    prompt_code_block_p=y prompt-instruction-input "Convert the following data into JSON:"
}
##
function prompt-2jax {
    prompt_code_block_p=y prompt-instruction-input "Rewrite the following code to use JAX (Flax) instead."
}
##
function prompt-tests-gen {
    prompt_code_block_p=y prompt-instruction-input "Write tests for the following code."
}

function prompt-tests-gen-human {
    prompt_code_block_p=y prompt-instruction-input "Write tests for the following code. These tests should print the computed results along with the expected result. The tests are intended for manual human inspection."
}

function prompt-examples-gen {
    prompt_code_block_p=y prompt-instruction-input "Write examples of using the following code."
}
#
##
function prompt-org2md {
    prompt_code_block_p=y prompt-instruction-input "Convert the following from org-mode to markdown:"
}
##
function prompt-2bash {
    prompt_code_block_p=y prompt-instruction-input "Convert the following code into Bash:"
}

function prompt-zsh2bash {
    prompt_code_block_p=y prompt-instruction-input "Convert the following code from Zsh into Bash:"
}
##
function prompt-code-rewrite-idiomatic {
    prompt_code_block_p=y prompt-instruction-input 'Rewrite the following code to make it more idiomatic and optimized:' "$@"
}
##
function snippet-night-namespace {
    ec "Prefix all of our new function names' with \`night/\` to namespace them properly." |
        cat-copy-if-tty
}
##
function snippet-debug-add-prints {
    ec "Add print statements for debugging purposes." |
        cat-copy-if-tty
}
##
function prompt-slide-complete-orgbeamer {
    prompt_code_block_p=y prompt-instruction-input 'Complete the following org-mode beamer presentation:' "$@"
}
##
function prompt-t2i-expand {
    prompt_code_block_p=y prompt-instruction-input 'Create a detailed description for a text2image prompt from this preliminary prompt:' "$@"
}
##
