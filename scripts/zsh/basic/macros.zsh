function aliasfn() {
    : "ruu might be needed. Example: aliasfn hi ruu someVar=12"
    local name="$1"
    local body="$@[2,-1]"
    functions[$name]="$body "'"$@"'
}
