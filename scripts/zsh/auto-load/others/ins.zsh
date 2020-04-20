insables="$NIGHTDIR"/setup/installables
pipables="$NIGHTDIR"/python/requirements.txt
inslables="$NIGHTDIR"/setup/installables-linux
nodables="$NIGHTDIR"/setup/node.g
brewables="$NIGHTDIR"/setup/brewables
brewables_mac="$NIGHTDIR"/setup/brewables_mac
ins_go="$NIGHTDIR/setup/ins_go"
ins_gem="$NIGHTDIR/setup/ins_gem"
### Aliases
alias bmac='brewables="$brewables_mac" '
###

clean-deps() {
    re clean-dups "$insables" "$inslables" "$nodables" "$brewables" "$brewables_mac" "$pipables" "$ins_go" "$ins_gem"
}
alias bnu='HOMEBREW_NO_AUTO_UPDATE=1'

insladd() {
    ec "$1" >> "$inslables"
    test -n "$noi" ||
        ins "$1"
}
insadd() {
    ec "$1" >> "$insables"
    test -n "$noi" ||
        ins "$1"
}
npmadd() {
    ec "$1" >> "$nodables"
    test -n "$noi" ||
        npm install -g "$1"
}
brew-bundle() { ec "$1 \"$2\"" >> "$brewables" }
btadd() {
    brew-bundle tap "$1"
    test -n "$noi" ||
        brew tap "$1"
}
biadd() {
    brew-bundle brew "$1"
    test -n "$noi" ||
        bi "$1"
}
piadd() {
    ec "$1" >> "$pipables"
    test -n "$noi" ||
        pi "$1"
}
goi() {
    comment -u update -v verbose
    test -n "$noi" ||
        go get -u -v "$@"
}
gmi() {
    gem install "$@"
}
gmiadd() {
    ec "$1" >> "$ins_gem"
    test -n "$noi" ||
        gmi "$1"
}
goiadd() {
    ec "$1" >> "$ins_go"
    test -n "$noi" ||
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
ins-gem() {
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
function ins() {
    isDarwin && brew install $1 || sudo apt install -y $1
}

jins() {
    mdoc Add Julia package MAGIC
    julia --startup-file=no -e 'using Pkg; Pkg.add("'"$1"'"); Pkg.precompile()' # using '"$1"
    # No longer necessary: We import the newly installed package to precompile it.
}
pig() { pi "git+$(git2http "$1")" }
reify pig
