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
function deps-clean {
    re duplicates-clean-sort-file-inplace "$insables" "$inslables" "$nodables" "$brewables" "$brewables_mac" "$pipables" "$ins_go" "$ins_gem"
}
aliasfn clean-deps deps-clean
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
##
function go-install-local {
    local d="${1:?}"
    pushf "$d" && {
        ecbold "$0: $d"
        go install
    } always { popf }
}

function go-install {
    comment -u update -v verbose
    test -n "$noi" ||
        {
            ##
            reval-ec go install "${*}@latest"
            ##
            #: no longer works
            # reval-ec go get -u -v "$@"
            ##
        }
}
aliasfn goi go-install

function goiadd {
    ec "$1" >> "$ins_go"
    test -n "$noi" ||
        goi "$1"
}
##
function gem-install {
    gem install "$@"
}
aliasfn gemi gem-install
aliasfn gmi gem-install

function gem-install-add {
    ec "$1" >> "$ins_gem"
    test -n "$noi" ||
        gmi "$1"
}
aliasfn gemiadd gem-install-add
aliasfn gmiadd gemiadd
##
function npm-install {
    #: @duplicateCode/b00b656b70d11d5d26d81a77f2d2b970
    ##
    local pkg
    for pkg in $@ ; do
        if test -n "${commands[pnpm]}" ; then
            reval-ecgray pnpm install -g "${pkg}" --include=optional --loglevel=silly
        else
            reval-ecgray npm install -g "$pkg" --progress=true --loglevel=verbose
        fi
    done
}

function ins-npm {
    local packages
    packages=(${(f)"$(cat $nodables)"}) @TRET

    for pkg in ${packages[@]} ; do
        npm-install "${pkg}"
    done
}

function ins-pip {
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
    #: @note This manifest holds *apt* names, so on a sudoless host most of it
    #: will miss -- [agfi:ins-sudoless] looks the same name up in mise and
    #: conda-forge, which spell several of them differently (fd-find vs fd).
    #: There, bootstrap stages 20 and 45 are the right path, not this.
    ##
    assert isLinux @RET

    zargs -n 1 -- $(cat "$inslables") -- ins #Don't quote the inputs, it makes zargs treat them as one monolithic input.
}

ins-brew() {
    brew bundle install --file="$brewables"
}

function ins-go {
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

##
function pkg-manager {
    #: Prints this host's system package manager, or fails if there is none we
    #: know. Override with NIGHT_PKG_MANAGER.
    #: @duplicateCode/3b7d90c4 [[NIGHTDIR:setup/bootstrap/lib.sh::pkg_manager]]
    #: -- that copy is tier 0 POSIX sh and may not depend on zshlang.
    ##
    #: An explicit override wins unconditionally, without a `have' check --
    #: otherwise it is not an override, and there would be no way to say so on a
    #: host where the binary is somewhere we do not look.
    if test -n "${NIGHT_PKG_MANAGER}" ; then
        ec "${NIGHT_PKG_MANAGER}"
        return 0
    fi

    local pm
    for pm in apt-get dnf yum pacman zypper apk ; do
        if (( ${+commands[$pm]} )) ; then
            ec "$pm"
            return 0
        fi
    done

    return 1
}

function ins-sudo {
    #: Install system packages through the OS package manager, as root.
    #: The explicit half of [agfi:install]; see [agfi:ins-sudoless] for the other.
    ##
    local pkgs=("$@")
    assert-args pkgs @RET

    local pm
    pm="$(pkg-manager)" || {
        ecerr "$0: no known system package manager on this host"
        return 127
    }

    local mode="$(sudo-mode)"
    local sudo_cmd=()
    case "$mode" in
        root) sudo_cmd=() ;;
        nopass) sudo_cmd=(command sudo -n) ;;
        password)
            if ! isI ; then
                #: A password prompt with nobody watching is a hang, not a question.
                ecerr "$0: sudo needs a password and we are not interactive; use [agfi:ins-sudoless]"
                return 1
            fi
            sudo_cmd=(command sudo)
            ;;
        *)
            ecerr "$0: no root access here (sudo-mode: ${mode}); use [agfi:ins-sudoless]"
            return 1
            ;;
    esac

    #: @duplicateCode/6e21af5b [[NIGHTDIR:setup/bootstrap/lib.sh::pkg_install]]
    case "$pm" in
        apt-get)
            reval-ec "${sudo_cmd[@]}" env DEBIAN_FRONTEND=noninteractive \
                apt-get install -y "${pkgs[@]}"
            ;;
        dnf|yum) reval-ec "${sudo_cmd[@]}" "$pm" install -y "${pkgs[@]}" ;;
        pacman)  reval-ec "${sudo_cmd[@]}" pacman -S --noconfirm --needed "${pkgs[@]}" ;;
        zypper)  reval-ec "${sudo_cmd[@]}" zypper --non-interactive install "${pkgs[@]}" ;;
        apk)     reval-ec "${sudo_cmd[@]}" apk add "${pkgs[@]}" ;;
        *)
            ecerr "$0: unsupported package manager: ${pm}"
            return 127
            ;;
    esac
}
##
function h-ins-sudoless-mise {
    #: Tier 1 of the ladder: static single binaries into $NIGHT_BIN.
    #: @duplicateCode/9a4c15d2 [[NIGHTDIR:setup/bootstrap/stages/20-tools-static.sh]]
    ##
    local pkg="${1:?}"

    (( ${+commands[mise]} )) || return 1

    if silent mise which "$pkg" ; then
        ecgray "$0: ${pkg}: already installed"
        return 0
    fi

    #: GIT_TERMINAL_PROMPT=0 is not optional: a credential prompt from a
    #: backend that fetches over git is exactly how this hangs.
    reval-ec env MISE_YES=1 GIT_TERMINAL_PROMPT=0 \
        mise use -g --yes "${pkg}@latest"
}

function h-ins-sudoless-conda-path {
    #: Appended, never prepended: mise owns any binary both channels provide.
    #: @duplicateCode/2f8b6e17 [[NIGHTDIR:setup/bootstrap/stages/45-cli-extras.sh]]
    ##
    local env_name="${1:?}"

    local bin="${NIGHT_TOOLS_BIN}"
    if test -z "$bin" ; then
        bin="${MAMBA_ROOT_PREFIX:-${HOME}/micromamba}/envs/${env_name}/bin"
    fi
    test -d "$bin" || return 0

    if (( ! ${path[(Ie)$bin]} )) ; then
        path+=("$bin")
    fi
}

function h-ins-sudoless-conda {
    #: Tier 2 of the ladder: the conda-forge catalog, for what has no static
    #: release binary.
    ##
    local pkg="${1:?}"

    (( ${+commands[micromamba]} )) || return 1

    local env_name="${NIGHT_TOOLS_ENV:-tools}"

    #: `micromamba install -n X` errors outright when X does not exist, so the
    #: first call must be `create`. Getting this wrong makes every package fail
    #: for the same reason.
    if ! micromamba env list 2>/dev/null |
            command grep -qE "(^| )${env_name}[ /]" ; then
        reval-ec micromamba create --yes -n "${env_name}" -c conda-forge @RET
    fi

    reval-ec micromamba install --yes -n "${env_name}" -c conda-forge "${pkg}" @RET

    h-ins-sudoless-conda-path "${env_name}"
}

function ins-sudoless {
    #: Install without root, into $HOME. The interactive one-package front end
    #: to the ladder [[NIGHTDIR:setup/bootstrap/stages/optional/junest.sh]]
    #: commits to; we cover its first two tiers, which is nearly everything.
    ##
    local pkgs=("$@")
    assert-args pkgs @RET

    if (( ! ${+commands[mise]} )) && (( ! ${+commands[micromamba]} )) ; then
        #: Deliberately no curl-pipe install from here. On a bootstrapped host
        #: these exist; on one that is not, running the bootstrap is the answer.
        ecerr "$0: neither mise nor micromamba found; run ${NIGHTDIR%/}/setup/bootstrap/bootstrap-sudoless.sh first"
        return 127
    fi

    local pkg failed=()
    for pkg in ${pkgs[@]} ; do
        h-ins-sudoless-mise "$pkg" ||
            h-ins-sudoless-conda "$pkg" ||
            failed+=("$pkg")
    done

    rehash

    if (( ${#failed} )) ; then
        ecerr "$0: could not install: ${failed[*]}"
        ecerr "$0: the remaining tiers are manual -- apt-get download + dpkg-deb -x, AppImage, then junest. See ${NIGHTDIR%/}/setup/bootstrap/stages/optional/junest.sh"
        return 1
    fi
}
##
function install() {
    local pkgs=("$@")
    assert-args pkgs @RET

    if isDarwin ; then
        #: brew needs no root, so the sudo question never arises here.
        install-latest "${pkgs[@]}"
    elif isLinux ; then
        if sudo-usable-p && silent pkg-manager ; then
            ins-sudo "${pkgs[@]}"
        else
            ins-sudoless "${pkgs[@]}"
        fi
    else
        return 127
    fi
}
aliasfn ins install


function install-latest {
    local pkgs=("$@") strict="${install_latest_strict}"

    if (( $+commands[brew] )) ; then
        brew install "${pkgs[@]}"
    else
        if test -n "$strict" ; then
            return 127
        else
            install "${pkgs[@]}"
        fi
    fi
}

function install-head {
    local pkgs=("$@") strict="${install_head_strict}"

    if (( $+commands[brew] )) ; then
        brew install --head "${pkgs[@]}"
    else
        if test -n "$strict" ; then
            return 127
        else
            install-latest "${pkgs[@]}"
        fi
    fi
}
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
