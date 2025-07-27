##
function h-prompt-image-v1 {
    #: @globalInput `prompt`
    ##
    ensure-array prompt_input_images @RET
    local prompt_input_images=("${prompt_input_images[@]}")
    if (( ${#prompt_input_images} == 0 )) && ! (( ${+llm_attachments} && ${+llm_attachments[(I)MAGIC_CLIPBOARD]} )) ; then
        #: If no inputs given and llm_attachments does NOT contain MAGIC_CLIPBOARD, we'll add it to `prompt_input_images`. I don't remember why. It seems `prompt_input_images` is used in `prompt-instruction-input` to copy the attached images to the system clipboard along with the prompt text, while `llm_attachments` will be used by `llm-m` etc. to attach the file when sending the request.
        #:
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
function prompt-image-ocr {
    local prompt
    # prompt='OCR the given images.'
    prompt="$(cat << 'EOF'
You will be given a series of images that are part of a single, related sequence. Your task is to perform OCR and combine the text from all images into one final, coherent output, following these specific rules:

*Combine Text:* Transcribe and merge the text from all images into a single, continuous document. Ensure the text flows in the correct sequence from one image to the next.

*No Commentary:* The final output must not contain any of your own commentary, explanations, or headers like "OCR Result" or "Image 1." It should only be the transcribed text itself.

*Consolidate Recurring Information:* Identify any information that is repeated across multiple images, such as author names, social media handles, logos, advertisements, or contact details. Remove these repetitions from the main body of the text.

*Create a Single Footer:* Place all the consolidated, recurring information you identified in the previous step just once at the very end of the document, creating a clean footer.

The goal is to produce a single, clean document as if it were the original, without the page breaks and repeated headers or footers from the images.
EOF
)"
    h-prompt-image-v1 "$@"
}

function run-prompt-image-ocr {
    llm_copy_p="${llm_copy_p:-y}" \
        with-llm-attach-clipboard \
        llm-run prompt-image-ocr "$@" |
        cat-rtl-streaming-if-tty
}

function jocr {
    jej

    local inputs=(*(.DN))
    if ((${#inputs[@]} == 0 )); then
        ecgray "$0: no inputs"
        return 0
    fi

    llm_attachments=("${inputs[@]}") llm-run prompt-image-ocr 2> >(erase-ansi > log_stderr.txt) @RET
}
##
