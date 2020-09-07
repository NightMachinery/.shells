# https://github.com/ahmedsaadxyzz/matrixcli
# https://matrix.org/clients/
###
function matrix-send-self() {
    matrixcli -c $nightNotes/private/configs/matrixcli/config.py send -r '!sCJIlZxICIvTJWNsrQ:matrix.org' "$*"
}