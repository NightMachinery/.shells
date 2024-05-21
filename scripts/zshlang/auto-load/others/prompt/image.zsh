##
function prompt-image-to-org {
    local prompt
    # prompt='Summarize this slide in one sentence and include a series of inline LaTeX equations. For inline LaTeX, use `\[ LATEX HERE \]`. When the entire paragraph or line is in LaTeX, use `\begin{equation}` and other appropriate LaTeX commands.'
    # prompt='Summarize the important points of this slide for a cheatsheet. For inline LaTeX, use `\[ LATEX HERE \]`. When the entire paragraph or line is in LaTeX, use `\begin{equation}` and other appropriate LaTeX commands.'
    prompt='Summarize the important points of this slide for a cheatsheet. For equations, use `\begin{equation}` and other appropriate LaTeX environments.'
    prompt+=" Avoid mentioning 'slide', 'summary', etc."

    prompt_input_images=("MAGIC_CLIPBOARD") \
        prompt_input_mode="${prompt_input_mode:-none}" \
        prompt-instruction-input "${prompt}" "$*"
}
##
