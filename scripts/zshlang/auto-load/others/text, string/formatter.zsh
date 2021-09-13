##
function beautifier-js {
    unibeautify -l JavaScript | cat-copy
}
alias pretty-js='beautifier-js'
alias formatter-js='beautifier-js'
##
function bookmarklet-from-js {
    ecgray "$0: do not have any comments in your code! We can't remove them with simple regexes ..."

    sd '^\s*javascript : ' 'javascript:' | newline2space
}

function p-bookmarklet-from-js {
    pbpaste | bookmarklet-from-js | cat-copy
}
alias pbj='p-bookmarklet-from-js'
##
