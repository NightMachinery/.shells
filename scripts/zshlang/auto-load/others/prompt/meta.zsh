##
function metaprompt-rewrite {
    prompt_input_mode="${prompt_input_mode:-block}" prompt-instruction-input 'Rewrite this prompt to make it clearer and easier to understand and follow:' "$@"
}
##
function metaprompt-t2i-expand {
    prompt_input_mode="${prompt_input_mode:-block}" prompt-instruction-input 'Create a detailed description for a text2image prompt from this preliminary prompt:' "$@"
}
##
