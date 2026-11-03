##
alias 2per='\noglob llm-run prompt-translate2per'
alias 2fa='2per'
alias 2en='\noglob llm-run prompt-translate2en'
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

function prompt-translate2per-technical-jargon {
    prompt-instruction-input "Give several Persian translations for the following machine learning jargon. Use web-search and look at Wikipedia etc. to find the common translations:" "$@"
}

function h-define-translate-prompt {
    #: Usage: h-define-translate-prompt <code> <Language>
    #: Defines `prompt-translate2<code>` and the alias `2<code>`.
    local code="$1" lang="$2"
    assert-args code lang @RET

    local instruction="Translate the following to ${lang}. Preserve the original's style and tone. Do not include any additional commentary."
    #: Not `aliasfn`: it mis-quotes the apostrophe in "original's".
    functions[prompt-translate2${code}]="prompt-instruction-input ${(q+)instruction} \"\$@\""
    revaldbg aliassafe "2${code}=\\noglob llm-run prompt-translate2${code}" @RET
}
#: `2ko` is taken by kindle.zsh.
h-define-translate-prompt de German
h-define-translate-prompt fr French
h-define-translate-prompt es Spanish
h-define-translate-prompt it Italian
h-define-translate-prompt pt Portuguese
h-define-translate-prompt nl Dutch
h-define-translate-prompt ar Arabic
h-define-translate-prompt tr Turkish
h-define-translate-prompt ru Russian
h-define-translate-prompt zh 'Simplified Chinese'
h-define-translate-prompt ja Japanese
h-define-translate-prompt hi Hindi
