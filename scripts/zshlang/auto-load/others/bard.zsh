##
function bard-unofficial-key1 {
    getcookies https://bard.google.com/ | cookies2json | jqm '.[] | select(.name=="__Secure-1PSID") | .value'
}
##
