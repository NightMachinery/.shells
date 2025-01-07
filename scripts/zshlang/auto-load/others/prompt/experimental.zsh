##
function prompt-add-emojis {
    local prompt_input_mode="${prompt_input_mode:-block}"
    local prompt="Add appropriate emojis to enhance this text. Preserve the exact original language, tone, and style.
Place emojis naturally where they fit best - usually after key nouns, at the end of sentences, or to emphasize emotions.
Don't overdo it - use emojis sparingly and meaningfully.
Don't add any explanations or comments, just return the enhanced text."

    prompt-instruction-input "${prompt}" "$@"
}
##
