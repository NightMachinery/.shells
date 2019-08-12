permute-case() {
    eval "printf '%s\0' $(echo "$@" | gsed 's/./{\U&,\L&}/g')"
}
NUL2NL() sd '\x00' '\n'
NL2NUL() sd '\n' '\x00'
NUL2RS() sd '\x00' ''
RS2NUL() sd '' '\x00'

