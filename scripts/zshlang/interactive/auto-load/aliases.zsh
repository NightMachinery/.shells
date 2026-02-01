##
function alias-copy {
    local alias_name="$1"
    assert-args alias_name @RET

    ec-copy "${aliases[$alias_name]}"
}
##
# aliasfn pd proxy-off
aliasfn '-' proxy-off

# aliasfn pu proxy-on
aliasfn '=' proxy-on
# @bug this causes `ec ======== ` to act weirdly
##
