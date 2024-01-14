##
function dns-cache-clear-darwin {
    reval-ec sudo dscacheutil -flushcache
    reval-ec sudo killall -HUP mDNSResponder
}
##
