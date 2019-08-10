insables="$NIGHTDIR"/setup/installables
pipables="$NIGHTDIR"/python/requirements.txt
inslables="$NIGHTDIR"/setup/installables-linux
nodables="$NIGHTDIR"/setup/node.g
brewables="$NIGHTDIR"/setup/brewables
ins_go="$NIGHTDIR/setup/ins_go"
ins_gem="$NIGHTDIR/setup/ins_gem"

clean-deps() {
    re clean-dups "$insables" "$inslables" "$nodables" "$brewables" "$pipables" "$ins_go" "$ins_gem"
}
alias bnu='HOMEBREW_NO_AUTO_UPDATE=1'

insladd() {
    ec "$1" >> "$inslables"
    test -z "$noi" ||
        ins "$1"
}
insadd() {
    ec "$1" >> "$insables"
    test -z "$noi" ||
        ins "$1"
}
npmadd() {
    ec "$1" >> "$nodables"
    test -z "$noi" ||
        npm install -g "$1"
}
brew-bundle() { ec "$1 \"$2\"" >> "$brewables" }
btadd() {
    brew-bundle tap "$1"
    test -z "$noi" ||
        brew tap "$1"
}
biadd() {
    brew-bundle brew "$1"
    test -z "$noi" ||
        bi "$1"
}
piadd() {
    ec "$1" >> "$pipables"
    test -z "$noi" ||
        pi "$1"
}
goi() {
    comment -u update -v verbose
    test -z "$noi" ||
        go get -u -v "$@"
}
gmi() {
    gem install "$@"
}
gmiadd() {
    ec "$1" >> "$ins_gem"
    test -z "$noi" ||
        gmi "$1"
}
goiadd() {
    ec "$1" >> "$ins_go"
    test -z "$noi" ||
        goi "$1"
}
ins-npm() {
    zargs -l 1 -- $(cat "$nodables") -- npm install -g
}
ins-pip() {
    zargs -l 1 -- "$NIGHTDIR"/python/**/requirements.txt -- pip install -U -r
}
ins-ins() {
    zargs -n 1 -- $(cat "$insables") -- ins #Don't quote the inputs, it makes zargs treat them as one monolithic input.
}
ins-linux() {
    zargs -n 1 -- $(cat "$inslables") -- ins #Don't quote the inputs, it makes zargs treat them as one monolithic input.
}
ins-brew() {
    brew bundle install --file="$brewables"
}
ins-go() {
    zargs -n 1 -- $(< "$ins_go") -- goi #Don't quote the inputs, it makes zargs treat them as one monolithic input.
}
ins-go() {
    zargs -n 1 -- $(< "$ins_gem") -- gmi #Don't quote the inputs, it makes zargs treat them as one monolithic input.
}
ins-all() {
    ins-brew
    ins-linux
    ins-ins
    ins-pip
    ins-npm
    ins-go
    ins-gem
}
