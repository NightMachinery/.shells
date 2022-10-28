##
function curl-stdout {
    curlm -o /dev/stdout "$@"
}
aliasfn gurl curl-stdout
##
function curl-remotename {
    #: -O, --remote-name          Write output to a file named as the remote file
    #: -J, --remote-header-name   Use the header-provided filename

    curlm --remote-name --remote-header-name "$@"
}
##
