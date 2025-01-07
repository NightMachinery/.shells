##
function metaprompt-expand-query-to-question {
    prompt_input_mode="${prompt_input_mode:-block}" prompt-instruction-input 'Convert the above search query to a proper question.' "$@"
}
##
function metaprompt-rewrite {
    prompt_input_mode="${prompt_input_mode:-block}" prompt-instruction-input 'Rewrite this prompt to make it clearer and easier to understand and follow:' "$@"
}
##
function metaprompt-t2i-expand {
    prompt_input_mode="${prompt_input_mode:-block}" prompt-instruction-input 'Create a detailed description for a text2image prompt from this preliminary prompt:' "$@"
}
##
function metaprompt-podcastify-expand {
    prompt_input_mode="${prompt_input_mode:-block}" prompt-instruction-input 'Create detailed Show Notes instructions for an AI model which creates two-hosts podcasts from some given input. The summary of what we want is as follows. Note that the AI will be given a book which the podcast will be about. You do not have access to this book, so your instructions should be content-neutral. Also, as the hosts are AI, they do not need any introductions etc. Make sure the podcast becomes information rich, dense, and suitable for a STEM PhD listener in its sophistication. The final prompt should be less than 500 characters.' "$@"
}
##
