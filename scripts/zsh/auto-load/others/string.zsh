permute-case() {
    eval "printf '%s\0' $(echo "$@" | gsed 's/./{\U&,\L&}/g')"
}
##
# tr is faster
# NUL2NL() sd '\x00' '\n'
# NL2NUL() sd '\n' '\x00'
# NUL2RS() sd '\x00' ''
# RS2NUL() sd '' '\x00'

NUL2NL() tr '\0' '\n'
NL2NUL() tr '\n' '\0'
NUL2RS() tr '\0' '\36'
RS2NUL() tr '\36' '\0'
##
