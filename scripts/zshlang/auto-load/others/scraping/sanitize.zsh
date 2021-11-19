##
function sanitize-css-external {
    perl -pe 's|(href="https?://\S+\.css)|${1}.disabled|g'
}
##
