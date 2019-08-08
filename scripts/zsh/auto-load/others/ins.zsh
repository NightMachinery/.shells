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
bnu() {
    # brew-no-update
    export HOMEBREW_NO_AUTO_UPDATE=1
}
insladd() {
    ec "$1" >> "$inslables"
    ins "$1"
}
insadd() {
    ec "$1" >> "$insables"
    ins "$1"
}
npmadd() {
    ec "$1" >> "$nodables"
    npm install -g "$1"
}
biadd() {
    ec "brew \"$1\"" >> "$brewables"
    bi "$1"
}
piadd() {
    ec "$1" >> "$pipables"
    pi "$1"
}
goi() {
    comment -u update -v verbose
    go get -u -v "$@"
}
gmi() {
    gem install "$@"
}
gmiadd() {
    ec "$1" >> "$ins_gem"
    gmi "$1"
}
goiadd() {
    ec "$1" >> "$ins_go"
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
