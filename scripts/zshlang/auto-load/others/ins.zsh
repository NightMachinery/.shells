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
##
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
aliasfn npmi npm install -g

npmiadd() {
    ec "$1" >> "$nodables"
    test -n "$noi" ||
        npmi "$1"
}

brew-bundle() {
    ec "$1 \"$2\"" >> "$brewables"
}

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

function piadd() {
    ec "$1" >> "$pipables"
    test -n "$noi" ||
        pi "$1"
}
noglobfn piadd

goi() {
    comment -u update -v verbose
    test -n "$noi" ||
        reval-ec go get -u -v "$@"
}

gmi() {
    gem install "$@"
}

gmiadd() {
    ec "$1" >> "$ins_gem"
    test -n "$noi" ||
        gmi "$1"
}

function goiadd() {
    ec "$1" >> "$ins_go"
    test -n "$noi" ||
        goi "$1"
}

function ins-npm() {
    zargs -l 1 -- $(cat "$nodables") -- npm install -g
}

function ins-pip() {
    pip-install pip # forces pip to be the latest version

    local f
    for f in "$NIGHTDIR"/python/**/requirements.txt ; do
        reval-ec pip install -U --use-deprecated=legacy-resolver --use-feature=fast-deps -r "$f"
        # fast-deps seems to download the packages metadata without downloading the packages completely (no good docs found).
        # fast-deps has no effect when used with the legacy resolver.

        pip-install pip # forces pip to be the latest version
    done
}

function ins-ins() {
    zargs -n 1 -- $(cat "$insables") -- ins #Don't quote the inputs, it makes zargs treat them as one monolithic input.
}

ins-linux() {
    assert isLinux @RET

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
    if isLinux; then
        ins-linux
    fi
    ins-ins
    ins-pip
    ins-npm
    ins-go
    ins-gem
}

function ins() {
    isDarwin && brew install $1 || sudo apt install -y $1
}

function jins() {
    mdoc Add Julia package MAGIC
    julia --startup-file=no -e 'using Pkg; Pkg.add("'"$1"'"); using Dates ; Pkg.gc(collect_delay=Day(0)) ; Pkg.precompile()' # using '"$1"
    # No longer necessary: We import the newly installed package to precompile it.
}
reify jins
##
function pig() {
    local pkg="$1"

    if [[ "$pkg" != 'git+'* ]] ; then
        pkg="git+$(git2http "$pkg")"
    fi

    reval-ec pi "$pkg"
    reval-ec pi --no-deps --force-reinstall "$pkg"
}
renog pig
##
function npm-update-all-g() {
    : "@alt npm-check -u -g"

    # npm outdated -g --depth=0 produces output according to the header → current | wanted | latest
    # npm outdated -g --depth=0 --parseable produces output in a different order → wanted | current | latest
    local package
    for package in $(npm -g outdated --parseable --depth=0 | cut -d: -f4)
    do
        ec npm -g install "$package"
    done
}

function npm-reinstall-all-g {
    local pkg
    for pkg in ${(@f)"$(npm ls -g --depth=0 --parseable | gsed 1d)"} ; do
        reval-ec npm install -g "${pkg:t}"
    done
}
##
function gnu-prefix-fix {
    if isLinux ; then
        local cmd
        for cmd in sed grep ; do
            if true || ! (( ${+commands[g$cmd]} )) ; then
                local cmd_path
                cmd_path="${brew_bin_dir}/${cmd}"
                if ! test -e "$cmd_path" ; then
                    cmd_path="/bin/${cmd}"
                fi
                if ! test -e "$cmd_path" ; then
                    cmd_path="$(realpath2 "$cmd")" || {
                        ecerr "$0: cmd $(gquote-sq "$cmd") not found"
                        continue
                    }
                fi

                local dest=~/bin/g"$cmd"
                silent trs "$dest"
                reval-ec ln -s "$cmd_path" "$dest"
            fi
        done
    fi
}
##
