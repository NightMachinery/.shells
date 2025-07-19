##
function h-prompt-image-v1 {
    #: @globalInput `prompt`
    ##
    ensure-array prompt_input_images @RET
    local prompt_input_images=("${prompt_input_images[@]}")
    if (( ${#prompt_input_images} == 0 )) && ! (( ${+llm_attachments} && ${+llm_attachments[(I)MAGIC_CLIPBOARD]} )) ; then
        #: [[id:66869733-383c-44fc-8d69-65d456c755e0][check if an item exists in a Zsh array]]
        ##
        prompt_input_images+=("MAGIC_CLIPBOARD")
    fi

    prompt_input_mode="${prompt_input_mode:-none}" \
        prompt-instruction-input "${prompt}" "$*"
}

##
function prompt-image-to-org {
    local prompt
    # prompt='Summarize this slide in one sentence and include a series of inline LaTeX equations. For inline LaTeX, use `\[ LATEX HERE \]`. When the entire paragraph or line is in LaTeX, use `\begin{equation}` and other appropriate LaTeX commands.'
    # prompt='Summarize the important points of this slide for a cheatsheet. For inline LaTeX, use `\[ LATEX HERE \]`. When the entire paragraph or line is in LaTeX, use `\begin{equation}` and other appropriate LaTeX commands.'
    prompt='Summarize the important points of this slide for a cheatsheet. For equations, use `\begin{equation}` and other appropriate LaTeX environments.'
    prompt+=" Avoid mentioning 'slide', 'summary', etc."

    h-prompt-image-v1 "$@"
}

function prompt-image-ocr-latex {
    local prompt
    prompt='OCR the image to Markdown. You can use LaTeX inside Markdown when needed.'
    h-prompt-image-v1 "$@"
}

function run-prompt-image-ocr-latex {
    llm_copy_p="${llm_copy_p:-y}" \
        with-llm-attach-clipboard \
        llm-run prompt-image-ocr-latex "$@" |
        cat-rtl-streaming-if-tty
}
aliasfn 4o-run-prompt-image-ocr-latex with-4o run-prompt-image-ocr-latex

function prompt-image-ocr-IR-Shaba {
    local prompt
    prompt='OCR the given IR Shaba number. First OCR in the original letters used. Then, if the letters are in Persian, change them to English number letters. Finally, double-check the result. Put the final, correct version inside a markdown code block.'
    h-prompt-image-v1 "$@"
}

function run-prompt-image-ocr-IR-Shaba {
    llm_copy_p="${llm_copy_p:-y}" \
        with-llm-attach-clipboard \
        llm-run prompt-image-ocr-IR-Shaba "$@" |
        cat-rtl-streaming-if-tty
}
##
function prompt-image2md {
    local prompt
    prompt='Convert the image to Markdown format text.
- Include all text content in a structured way
- Maintain the hierarchical organization (headings, subheadings)
- Preserve the dates and time periods
- Do not include any image links or placeholders
- Use proper Markdown formatting (headings with #, line breaks, etc.)'
    h-prompt-image-v1 "$@"
}
##
