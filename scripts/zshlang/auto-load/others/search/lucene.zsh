##
function nt-lucene() {
    local q="$*"
    # @globalInputs ntLines
    ## https://lucene.apache.org/core/2_9_4/queryparsersyntax.html
    # '+mustInclude'
    # 'wildcard*'
    # 'this is or'
    # 'me AND you' 'me && you'
    # '(jakarta OR apache) AND website'
    # '"grouped words"'
    ##

    local files opts=()

    # files=("$nightNotes"/**/${~noteglob})
    # files=("$nightNotes"/**/${~textglob})

    files=("${nightNotes%%/}/**/*.{${(j.,.)text_formats}}")
    # glob syntax: https://docs.oracle.com/javase/8/docs/api/java/nio/file/FileSystem.html#getPathMatcher-java.lang.String-

    if ! bool "$ntLines" ; then
        opts+=( --no-split )
    fi
    revaldbg lmgrep '--case-sensitive?' false --hyperlink "$opts[@]" -q "$q" "$files[@]"
}
alias ntc='\noglob nt-lucene'

function nt-lucene. {
    nightNotes="$PWD" nt-lucene "$@"
}
##
