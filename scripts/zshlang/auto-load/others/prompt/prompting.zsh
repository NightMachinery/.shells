##
alias 2per='\noglob llm-run prompt-translate2per'
alias 2fa='2per'
alias 2en='\noglob llm-run prompt-translate2en'
##
alias porg='prompt_input_mode=org'
alias pblk='prompt_input_mode=block'

function prompt-instruction-input {
    local instruction="$1"
    shift @RET

    ensure-array prompt_input_images #: @global/input
    local input_mode="${prompt_input_mode}"
    local qa_p="${prompt_qa_p}"
    local preambles=(${prompt_preambles[@]})

    local input
    # input="$(in-or-args "$@")" @RET
    if (( $#@ > 0 )) ; then
        input="$*"
    else
        input="$(cat-paste-if-tty)" @RET
    fi

    input="$(ec "$input" | erase-ansi)" @TRET

    if test -n "$input" ; then
        if [[ "${input_mode}" == 'block' ]] ; then
            input='```'$'\n'"$input"$'\n''```'

        elif [[ "${input_mode}" =~ '^org(?:2md)?$' ]] ; then
            input="$(ec "$input" |
            org-remove-inline-images |
            org2md)" @TRET

        elif test -z "${input_mode}" || [[ "${input_mode}" =~ '(?i)^none|n$' ]] ; then
            #: do nothing

        else
            ecerr "$0: unknown input mode: ${input_mode}"
            return 1
        fi

        if bool "${qa_p}" ; then
            input='Q:'$'\n'"$input"$'\n\n''A:'
        fi
    fi

    local prompt_text
    prompt_text="$( {
        for preamble in "${preambles[@]}" ; do
            prompt_preambles=() prompt_qa_p= prompt_input_mode= assert reval "${preamble}" '' @RET
            #: Preambles are often snippets. Snippets might decide to read from stdin or the clipboard if we do not explicitly supply their input, hence the empty =''=.
        done

        ec "$instruction"

        if test -n "$input" ; then
            ec $'\n'"$input"
        fi
    } |
        strip-blank-lines-start-end)" @RET

    local input_files=()
    if (( ${#prompt_input_images} >= 1 )) ; then
        local input_image processed_image tmp_file
        for input_image in "${prompt_input_images[@]}" ; do
            if [[ "${input_image}" == "MAGIC_CLIPBOARD" ]] ; then
                tmp_file="$(gmktemp --suffix ".png")" @TRET
                assert pngpaste "${tmp_file}" @RET

                processed_image="${tmp_file}"
            else
                assert test -e "${input_image}" @RET

                processed_image="${input_image}"
            fi

            input_files+=("${processed_image}")
            icat_v=n icat "${processed_image}" @STRUE
        done

        if isOutTty ; then
            assert copy_files_with_text.swift "${prompt_text}" "${input_files}" @RET

            ec "$prompt_text"
        else
            ec "${prompt_text}"
            ecgray "$0: attached images ignored as the output is not a TTY"
        fi
    else
        ec "${prompt_text}" |
            cat-copy-if-tty
    fi
}

function prompt-instruction-input-block {
    prompt_input_mode="${prompt_input_mode:-block}" prompt-instruction-input "$@"
}
alias cat-pblk='prompt-instruction-input-block'

function prompt-instruction-input-coding {
    prompt_input_mode="${prompt_input_mode:-block}" \
    prompt_preambles=(${prompt_preambles[@]} snippet-preamble-coding) \
        prompt-instruction-input "$@"
}

function prompt-blockify {
    : "Use our Hammerspoon function 'pasteBlockified' instead"

    prompt_input_mode="${prompt_input_mode:-block}" prompt-instruction-input ''
}
alias xb='prompt-blockify'
##
function prompt-coding-rewrite-performant {
    prompt_input_mode="${prompt_input_mode:-block}" prompt-instruction-input 'Rewrite the following code to make it faster, optimized and performant. Use best practices.' "$@"
}
##
function prompt-coding-correct-mistakes {
    prompt_input_mode="${prompt_input_mode:-block}" prompt-instruction-input "Correct any mistakes in the following code and suggest ways it can be optimized to run faster or be written more idiomatically and cleanly." "$@" |
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
        prompt_input_mode="${prompt_input_mode:-block}" prompt-tex
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
        prompt_input_mode="${prompt_input_mode:-block}" prompt_qa_p=y prompt-instruction-input "Write the following HTML snippet as org-mode with inline LaTeX. Exclude citation links.${examples}" "$@"
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
    prompt_input_mode="${prompt_input_mode:-block}" prompt-instruction-input 'Convert the following document from LaTeX into plain text.' "$@"
}
##
function prompt-correct-grammar {
    prompt_input_mode="${prompt_input_mode:-block}" prompt-instruction-input 'Correct grammatical and spelling mistakes in the following text,  without changing the wording:' "$@"
}

function prompt-correct-grammar-list {
    local prompt='Please review the given text and list each spelling or grammar mistake found. For each error, provide potential corrections in this format:

1. [Quote the sentence containing incorrect text]: Brief explanation of the error
Possible corrections:
- [Correction option 1]
- [Correction option 2] (if applicable)

2. [Quote the sentence containing incorrect text]: Brief explanation of the error
Possible corrections:
- [Correction option 1]
- [Correction option 2] (if applicable)

Note: Please preserve original wording and only address spelling and grammar issues. Do not make changes to formatting or spacing.'

    prompt_input_mode="${prompt_input_mode:-block}" prompt-instruction-input "${prompt}" "$@"
}
##
function prompt-rewrite-polite {
    local prompt
    prompt="Rewrite the given text to be more polite and professional. Preserve all information in the given text."

    prompt_input_mode="${prompt_input_mode:-block}" prompt-instruction-input "${prompt}" "$@"
}
##
function prompt-rewrite-fluent-list {
    local prompt
    prompt="List all sentences that need improvement or need to be moved for better flow, along with suggested changes and explanations."

    prompt_input_mode="${prompt_input_mode:-block}" prompt-instruction-input "${prompt}" "$@"
}

typeset -g prompt_fluent='Rewrite the text more fluently and with correct grammar.'
typeset -g prompt_concise='Rewrite the text more fluently and concisely. All information must be retained, only redundancies and fluff can be omitted.'
typeset -g prompt_concise_v2='Rewrite in my own language but with better flow and more concise. Output in a code block. Use LaTeX if the source is in LaTeX. Preserve my own voice!'

function prompt-rewrite-fluent {
    local prompt
    prompt="${prompt_fluent}"
    # prompt='Make the following text more fluent:'

    prompt_input_mode="${prompt_input_mode:-block}" prompt-instruction-input "${prompt}" "$@"
}
function prompt-rewrite-fluent-multi {
    prompt_input_mode="${prompt_input_mode:-block}" prompt-instruction-input "${prompt_fluent} Give 5 different rewrites, with different trade-offs in each. You can move sentences around for better flow. If the original text is in a particular format, e.g., LaTeX, also output in that format in a code block." "$@"
}
function prompt-rewrite-concise-multi {
    prompt_input_mode="${prompt_input_mode:-block}" prompt-instruction-input "${prompt_concise} Give 5 different rewrites, with different trade-offs in each." "$@"
}
function prompt-rewrite-concise-multi-v2 {
    prompt_input_mode="${prompt_input_mode:-block}" prompt-instruction-input "${prompt_concise_v2} Give 5 different rewrites, with different trade-offs in each." "$@"
}

function prompt-rewrite-fluent-latex {
    prompt_input_mode="${prompt_input_mode:-block}" prompt-instruction-input 'Make the following text more fluent (Output LaTeX in a code block).' "$@"
}

function prompt-rewrite-as-orgmode-latex {
    prompt_input_mode="${prompt_input_mode:-block}" prompt-instruction-input 'Rewrite as org-mode with LaTeX enabled. First review the syntax rules of this format (VERY IMPORTANT!).' "$@"
}

function prompt-rewrite-fluent-orgmode {
    prompt_input_mode="${prompt_input_mode:-block}" prompt-instruction-input 'Make the following org-mode text more fluent:' "$@"
}

function prompt-rewrite-fluent-orgbeamer {
    prompt_input_mode="${prompt_input_mode:-block}" prompt-instruction-input 'Make the following org-mode beamer presentation more fluent:' "$@"
}

function prompt-rewrite-fluent-selective {
    prompt_input_mode="${prompt_input_mode:-block}" prompt-instruction-input 'Make the following text more fluent by editing the part marked with `{ ... }`:' "$@"
}

function prompt-rewrite-question-stackoverflow {
    prompt_input_mode="${prompt_input_mode:-block}" prompt-instruction-input 'Rewrite the following question for Stackoverflow. Be concise, to the point, but detailed.' "$@"
}

function prompt-rewrite-abstract {
    prompt_input_mode="${prompt_input_mode:-block}" prompt-instruction-input 'Rewrite the given abstract to be more clear.' "$@"
}
function prompt-rewrite-abstract-concise {
    prompt_input_mode="${prompt_input_mode:-block}" prompt-instruction-input 'Rewrite the given abstract to be more clear, engaging, and concise.' "$@"
}

function prompt-rewrite-fluent-graduate {
   local prompt_input_mode="${prompt_input_mode:-block}"
   local prompt="Rewrite the following text at a postgraduate academic reading level - sophisticated and precise but not flowery. Use natural, fluid language while maintaining technical rigor and depth. Keep core meaning and details intact while making expression more direct:"

   prompt-instruction-input "${prompt}" "$@"
}
##
function prompt-write-exercise-descpription {
    prompt_input_mode="${prompt_input_mode:-block}" prompt-instruction-input 'The following is a practical exercise for a course. Write its description for the students.' "$@"
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
function prompt-translate2en-orgbeamer {
    prompt-instruction-input 'Rewrite the below latex snippet as English org-mode org-beamer slides. You should put citation inside `@@latex:\cite{...}@@`.' "$@"
}

function prompt-translate2en {
    prompt-instruction-input "Translate the following to English. Do not include any additional commentary." "$@"
}

function prompt-translate2en-v1 {
    prompt-instruction-input "Translate the following to English. Preserve the original's style and tone. Do not include any additional commentary." "$@"
}

function prompt-translate2per {
    prompt-instruction-input "Translate the following to Persian. Preserve the original's style and tone. Do not include any additional commentary." "$@"
}
##
function prompt-summarize-text {
    # prompt-instruction-input "Summarize the following document." "$@"
    prompt-instruction-input "Provide a summary of the following document, starting with a concise overview in the initial paragraph. Then, proceed with a detailed breakdown across four additional paragraphs." "$@"
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
    prompt_input_mode="${prompt_input_mode:-block}" prompt-instruction-input "How is the following pronounced? Include IPA." "$@"
    #
    # prompt_input_mode="${prompt_input_mode:-block}" prompt-instruction-input "Define the following, and include its IPA pronunciation in American English." "$@"
}
aliasfn prompt-say prompt-pronunciation

function prompt-explain-meaning {
    prompt_input_mode="${prompt_input_mode:-block}" prompt-instruction-input 'What does the following text mean?' "$@"
}

function prompt-define {
    prompt_input_mode="${prompt_input_mode:-block}" prompt-instruction-input "Define the following, and include its IPA pronunciation." "$@"
    #
    # prompt_input_mode="${prompt_input_mode:-block}" prompt-instruction-input "Define the following, and include its IPA pronunciation in American English." "$@"
}

function prompt-define-long {
    prompt_input_mode="${prompt_input_mode:-block}" prompt-instruction-input 'Define the following using the following output format:
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
function prompt-explain-joke {
    prompt_input_mode="${prompt_input_mode:-block}" prompt-instruction-input 'Explain the following joke:' "$@"
}
##
function prompt-explain-code {
    prompt_input_mode="${prompt_input_mode:-block}" prompt-instruction-input 'Explain the following code:' "$@"
}

function prompt-simplify-code {
    prompt_input_mode="${prompt_input_mode:-block}" prompt-instruction-input 'Simplify the following code:' "$@"
}
##
function prompt-refactor-to-functions {
    prompt-instruction-input-coding "Refactor the following code and break it up into smaller functions, each with a clear purpose." "$@"
}
##
function prompt-shell-long-args {
    prompt_input_mode="${prompt_input_mode:-block}" prompt-instruction-input 'Rewrite the following shell code using long argument forms instead of short ones. E.g.,

Q: `curl -I ...`
A: `curl --head ...`

Only output the rewritten code.' "$@"
}
##
function prompt-explain-stacktrace {
    prompt_input_mode="${prompt_input_mode:-block}" prompt-instruction-input "The stacktrace of an error follows. What might be the problem?" "$@"
}

aliasfn xss prompt-explain-stacktrace
##
function prompt-code-complete {
    prompt_input_mode="${prompt_input_mode:-block}" prompt-instruction-input "Complete the given code with complete, production-ready, code. Output in a code block. Don't output anything else." "$@"
}

function prompt-code-complete-3 {
    prompt_input_mode="${prompt_input_mode:-block}" prompt-instruction-input "Give three likely completions for the following code snippet. Output each candidate completion in a code block. Don't output anything else. Your completions should be complete, production-ready code." "$@"
}

function prompt-py-to-kwargs {
    prompt_input_mode="${prompt_input_mode:-block}" prompt-instruction-input "Convert all arguments to keyword arguments." "$@"
}
##
function prompt-rewrite-as-script {
    prompt_input_mode="${prompt_input_mode:-block}" prompt-instruction-input "Rewrite the following as a noninteractive Zsh script. Your reply should be a single code block contaning the Zsh script." "$@"
}

function prompt-rewrite-zsh-as-standalone {
    prompt_input_mode="${prompt_input_mode:-block}" prompt-instruction-input "Rewrite the above Zsh functions to make the usable without the special functions and aliases defined in my setup." "$@"
}
##
function prompt-docstring-write {
    prompt_input_mode="${prompt_input_mode:-block}" prompt-instruction-input "Add or update the docstrings of important things to the following." "$@"
}
##
function prompt-proofread-email {
    prompt-instruction-input "Proofread the following email. Make sure it's polite and professional." "$@"
}

function prompt-find-email {
    prompt-instruction-input "Find the email addresses of the following people. You can search the web. Back your findings with citations. Output the email in standard form, but show the exact quote from which this standard form was decoded." "$@"
}
##
function prompt-learn-papers {
    prompt-instruction-input "I want to learn about the following concept(s). Which papers do you recommend?" "$@"
}
##
function prompt-rewrite-formal {
    prompt_input_mode="${prompt_input_mode:-block}" prompt-instruction-input "Rewrite the following in a formal manner." "$@"
}
##
function prompt-2json {
    prompt_input_mode="${prompt_input_mode:-block}" prompt-instruction-input "Convert the following data into JSON:" "$@"
}
##
function prompt-2jax {
    prompt_input_mode="${prompt_input_mode:-block}" prompt-instruction-input "Rewrite the following code to use JAX (Flax) instead." "$@"
}
##
function prompt-tests-gen {
    prompt_input_mode="${prompt_input_mode:-block}" prompt-instruction-input "Write tests for the following code." "$@"
}

function prompt-tests-gen-human {
    prompt_input_mode="${prompt_input_mode:-block}" prompt-instruction-input "Write tests for the following code. These tests should print the computed results along with the expected result. The tests are intended for manual human inspection." "$@"
}

function prompt-examples-gen {
    prompt_input_mode="${prompt_input_mode:-block}" prompt-instruction-input "Write examples of using the following code." "$@"
}
#
##
function prompt-org2md {
    prompt_input_mode="${prompt_input_mode:-block}" prompt-instruction-input "Convert the following from org-mode to markdown:" "$@"
}
##
function prompt-2bash {
    prompt_input_mode="${prompt_input_mode:-block}" prompt-instruction-input "Convert the following code into Bash:" "$@"
}

function prompt-zsh2bash {
    prompt_input_mode="${prompt_input_mode:-block}" prompt-instruction-input "Convert the following code from Zsh into Bash:" "$@"
}
##
function prompt-py-use-kwargs {
    prompt_input_mode="${prompt_input_mode:-block}" prompt-instruction-input 'Always use `function (*, ` (unless it takes no arguments or the function already has a `*` in its argument list) and kwargs calling.' "$@"
}
##
function prompt-code-rewrite-idiomatic {
    prompt_input_mode="${prompt_input_mode:-block}" prompt-instruction-input 'Rewrite the following code to make it more idiomatic and optimized:' "$@"
}
##
function snippet-debug-add-prints {
    snippet-input "Add print statements for debugging purposes." "$@"
}

function prompt-debug-find-bugs {
    # prompt_input_mode="${prompt_input_mode:-block}" prompt-instruction-input 'Find bugs in the following code. Then fix the found bugs.' "$@"
    prompt_input_mode="${prompt_input_mode:-block}" prompt-instruction-input 'Review the following code for any issues and bugs. Then fix the found problems. For ambiguities, make reasonable assumptions, but list them explicitly so I can override them if necessary. If some function from a library is imported, assume the library exists even if you do not know about it.' "$@"
    # If you some functions are missing, assume they are defined elsewhere, but list them afterwards.
}
##
function prompt-slide-complete-orgbeamer {
    prompt_input_mode="${prompt_input_mode:-block}" prompt-instruction-input 'Complete the following org-mode beamer presentation:' "$@"
}
##
function prompt-coco2imagenet {
    local coco_label="$1"
    assert-args coco_label @RET

    {
        ec "Here are the ImageNet labels:"
        cat "${nightNotesPublic}/subjects/ML/vision/datasets/ImageNet/imagenet-simple-labels.json" | jqm '.[]' | prompt-blockify

        cat << EOF
I want to map MS-COCO labels to ImageNet labels.

For example, for the MS-COCO label "elephant":
\`\`\`
1. Indian elephant, Elephas maximus
2. African elephant, Loxodonta africana
3. tusker
\`\`\`

What are the top 10 (or more if available) nearest ImageNet labels for the COCO label "${coco_label}"?

EOF

        # Add the mapping for the COCO label "${coco_label}".
        # I have attached the file "ilsvrc2012_wordnet_lemmas.txt" that contains ImageNet labels.
    } |  cat-copy-if-tty
}
##
function prompt-imdb-recommend {
    ec "Recommend $*. Include their IMDB ratings (to the best of your knowledge), the year of publication, a plot hook, the reason for recommending/disrecommending it (pros/cons)." |
        cat-copy-if-tty
    # "Try to keep the IMDB ratings above 7."
    #: Asking it for high IMDB scores seem to bias the model in a bad way.

    # ec "Recommend $*. Include their IMDB ratings (to the best of your knowledge) and the year of publication. Include a summary of each one's production, plot, and pros/cons." |
}

function prompt-imdb-adaptations {
    ec "List all adaptations of $* with their IMDB ratings and year of publication. Include a summary of each one's production and pros/cons." |
        cat-copy-if-tty
}
##
function prompt-ocr-correct {
    prompt_input_mode="${prompt_input_mode:-block}" prompt-instruction-input 'Please find below the output of an OCR. Correct any errors that the OCR may have introduced into the following output text. Correct whitespace mistakes, too. Only output the corrected text in a code block.' "$@"
}
##
function prompt-2en-solve-exercise {
    prompt_input_mode="${prompt_input_mode:-block}" prompt-instruction-input 'First translate the following problem statement to English. Then solve it.' "$@"
}
##
function text-split-letters {
    in-or-args "$@" |
        perl -lpe 's/(.)/$1 /g' |
        cat-copy-if-tty
}
##
function prompt-commit-msg {
    prompt_input_mode="${prompt_input_mode:-block}" prompt-instruction-input 'Suggest concise commit messages. Put each suggestion into its own code block.' "$@"
}
##
function prompt-emoji-name {
    prompt_input_mode="${prompt_input_mode:-block}" prompt-instruction-input 'What are the names of the emojis related to the following:' "$@"
}
##
function latex-inline-inputs {
    ##
    revaldbg latex_inline_inputs.py "${@}" |
        strip-duplicate-subsequent-blank-whitespace-lines |
        cat-copy-if-tty
    ##
    # local input="${1}"
    # assert-args input @RET
    # shift

    # local opts=("${@}")
    # revaldbg latex_inline_inputs.py "${opts[@]}" -- "${input}"
    ##
}

function pbcopy-file-as-md {
    #: @duplicateCode/2f9a53358e9a32d7c5642a4a9842dd63
    ##
    local fs=($@)
    ensure-array copy_as_md_latex_inline_opts
    local latex_inline_opts=("${copy_as_md_latex_inline_opts[@]}")

    if ! array-contains latex_inline_opts --no-rm-comments ; then
        latex_inline_opts+=('--rm-comments')

        # ecgray "Added '--rm-comments' to latex_inline_opts"
    else
        # ecgray "Option '--no-rm-comments' is present. No changes made."
    fi

    local f retcode=0
    {
        for f in "${fs[@]}" ; do
            if ! test -e "$f" ; then
                ecerr "$0: File does not exist: $f"
                retcode=1
                continue
            fi

            ec "File: $f"
            ec '``````````'
            if isDeus && [[ "${f}" =~ '\.tex$' ]] ; then
                latex-inline-inputs "${f}" "${latex_inline_opts[@]}" @RET
            else
                cat "$f"
            fi
            ec $'\n''``````````'
            ec $'\n'
        done

        ec '---'$'\n'
    } | {
        if isLocal ; then
            cat-copy-streaming
        else
            pbcopy-remote
        fi
    }

    return $retcode
}
aliassafe cx='pbcopy-file-as-md'
##
function prompt-rewrite-telegram {
    prompt_input_mode="${prompt_input_mode:-block}" prompt-instruction-input 'Rewrite the following text as a post for a Telegram channel. You can use emojis, etc. You can use `**bold text**`, `__italic text__`, ~~strikethrough~~, and Markdown literals and code blocks using backticks, e.g., \`CONSTANT_IN_CODE\`. You can use Markdown links, `[label](url)`. End the post by including a link to our channel, `EMOJI [@SUTCSE](https://t.me/sutcse)` with a random nice creative emoji. This line starts with an emoji and ends with our link, no other text needed.'" (To ensure randomness, use the current date as a seed for your decision: $(date)."' Be concise, but easy to understand. The target audience is AI PhD students studying in Sharif University of Technology. Begin your post with a headline that summarizes the main point (the lede). Write in Farsi (Persian), as the students are Iranians.' "$@"
}

function run-prompt-rewrite-telegram {
    ensure-array tsend_opts
    local tsend_opts=("${tsend_opts[@]}" --link-preview)

    llm_copy_p="${llm_copy_p:-n}" \
        llm-run prompt-rewrite-telegram "$@" > >(md2tlg) |
        cat-rtl-streaming-if-tty
}
##
function prompt-cs-phd-apply {
    prompt_input_mode="${prompt_input_mode:-block}" prompt-instruction-input 'CS PhD application requirements (GRE, LOR count) and deadline' "$@"
}
##
function prompt-text2img-for-bot {
    prompt_input_mode="${prompt_input_mode:-block}" prompt-instruction-input 'Give a text2image prompt to generate a great profile picture for this Telegram bot. I want a white background and minimalist design BUT using the intricate capabilities of modern models. E.g., a symbol drawn using glush rainforest textures on a simple white background. Give various suggestions with different themes and complexities. Put each suggested prompt inside a code block.' "$@"
}
##
function prompt-apply-find-interp-papers {
    prompt_input_mode="${prompt_input_mode:-block}" prompt-instruction-input 'List the papers related to interpretability and explainable AI (xAI).' "$@"
}

function prompt-apply-find-rel25-papers {
    local prompt='For each topic, find relevant papers from the list given:

-   interpretability and explainable AI (xAI) (separate found papers into three subcategories: technical interpretability, end user-focused xAI, and using interpretability techniques to increase model capabilities, such as mitigating hallucinations)
-   adversarial robustness
-   Foundation models
    -   understanding how models work
    -   reasoning
    -   agents
    -   VLM
    -   LLM
    -   prompt engineering
'

    prompt_input_mode="${prompt_input_mode:-block}" prompt-instruction-input "${prompt}" "$@"
}
##
function prompt-clean2md {
    prompt_input_mode="${prompt_input_mode:-block}" prompt-instruction-input 'Clean up the copied text. The output should be in Markdown format (in a code block).
- Include all text content in a structured way
- Maintain the hierarchical organization (headings, subheadings)
- Preserve the dates and time periods
- Do not include any image links or placeholders
- Use proper Markdown formatting (headings with #, line breaks, etc.)' "$@"
}

function prompt-clean2org {
    prompt_input_mode="${prompt_input_mode:-block}" prompt-instruction-input 'Clean up the copied text. The output should be in org-mode format (in a code block).
- Include all text content in a structured way
- Use proper org-mode formatting (headings with *, lists with -, formatting with /italic/ and *bold*, etc.)
- Maintain the hierarchical organization (use * for headings, ** for subheadings, etc.)
- Use lists with "-" for related items at the same level that are not headings
- Preserve the dates and time periods
- Do not include any image links or placeholders
' "$@"
}

function prompt-clean2org-v2 {
    prompt_input_mode="${prompt_input_mode:-block}" prompt-instruction-input '
Format the copied text into org-mode structure:

1. Convert all content into a proper org-mode, and output only the converted text.

2. Structure elements using:
   - Headings with * (e.g., * Main Heading, ** Subheading)
   - Lists with - and proper indentation
   - Prefer lists for a short list of items with no children, and prefer headings for hierarchical organization.
   - Use list items (- A\n- B) for related details under a heading rather than consecutive lines
   - Text formatting with /italic/ and *bold*
   - No manual indentation needed after headings. Org-mode automatically handles heading indentation, but you must indent list items.
   - Remember that in org-mode, a single line break is treated as a space when exported

3. Important content to preserve:
   - All text content
   - Hierarchical organization
   - Dates and time periods

4. Content to exclude:
   - Image links
   - Image placeholders

' "$@"
}

function run-prompt-clean2org-v2 {
    local llm_copy_p="${llm_copy_p:-n}"

    {
        if isInTty ; then
            pbpaste-html |
                html4latex-clean
        else
            cat
        fi
    } |
        llm-run prompt-clean2org-v2 "$@" |
        cat-streaming-copy-rtl-if-tty
        # cat-rtl-streaming-if-tty
}
alias '2org'='run-prompt-clean2org-v2'
##
