# -*- mode: sh; sh-shell: zsh; -*-
###
# * Psiphon client wrapper
#
# ** Usage
#
# Source this file, then run:
#
#   psiphon-install
#   psiphon-up-us96
#
# Useful commands:
#
#   psiphon-install
#   psiphon-init-config
#   psiphon-up
#   psiphon-up-us
#   psiphon-up-us96
#   psiphon-down
#   psiphon-toggle
#   psiphon-status
#   psiphon-logs
#   psiphon-test
#
# ** Configuration
#
# The module looks for a base client config in this order:
#
#   ${psiphon_base_config}
#   ${psiphon_config_dir}/psiphon.config.base
#   ${psiphon_fallback_config}
#   ${psiphon_config_dir}/psiphon.config
#
# If none exists, it writes and uses:
#
#   ${psiphon_config_dir}/psiphon.config.builtin
#
# Common overrides:
#
#   psiphon_config_dir
#   psiphon_base_config
#   psiphon_fallback_config
#   psiphon_builtin_base_config
#   psiphon_builtin_base_config_json
#   psiphon_use_builtin_base_config_p
#   psiphon_builtin_base_config_force_p
#
# Runtime overrides:
#
#   psiphon_region
#   psiphon_upstream_host
#   psiphon_upstream_socks_port
#   psiphon_upstream_proxy_url
#   psiphon_local_socks_port
#   psiphon_local_http_port
#   psiphon_session_name
#   psiphon_state_dir
#   psiphon_data_dir
#   psiphon_last_config_file
#
# Install overrides:
#
#   psiphon_bin_dir
#   psiphon_binary
#   psiphon_src_parent
#   psiphon_src_dir
#   psiphon_repo_url
#   psiphon_branch
#   psiphon_install_force_p
#   psiphon_install_download_first_p
#   psiphon_binary_base_url
#   psiphon_proxyenv
#   psiphon_go_toolchain
#
# Shortcut generation:
#
#   psiphon_good_regions=(US GB DE NL CA SG JP FR)
#   psiphon_upstream_port_specs=(...)
#
# ** Go toolchain handling
#
# Psiphon vendors packages that can depend on Go's internal TLS struct layout.
# Building with the wrong Go version can cause startup panics. The installer
# therefore checks the checked-out repo's go.mod and builds with:
#
#   1. ${psiphon_go_toolchain}, if set
#   2. the go.mod `toolchain` directive, if present
#   3. the go.mod `go` directive, converted to a Go toolchain name
#   4. auto
#
# Example override:
#
#   psiphon_go_toolchain=go1.24.4 psiphon_install_force_p=y psiphon-install
###

function h-psiphon-config-dir {
    local config_dir="${psiphon_config_dir:-${HOME}/.config/psiphon}"

    ec "${config_dir}"
}

function h-psiphon-state-dir {
    local state_dir="${psiphon_state_dir:-${HOME}/.local/state/psiphon}"

    ec "${state_dir}"
}

function h-psiphon-data-dir {
    local state_dir="${psiphon_state_dir:-$(h-psiphon-state-dir)}"
    local data_dir="${psiphon_data_dir:-${state_dir}/data}"

    ec "${data_dir}"
}

function h-psiphon-bin-dir {
    local bin_dir="${psiphon_bin_dir:-${HOME}/bin}"

    ec "${bin_dir}"
}

function h-psiphon-binary {
    local bin_dir="${psiphon_bin_dir:-$(h-psiphon-bin-dir)}"
    local binary="${psiphon_binary:-${bin_dir}/psiphon-cli}"

    ec "${binary}"
}

function h-psiphon-profiles-dir {
    local config_dir="${psiphon_config_dir:-$(h-psiphon-config-dir)}"
    local profiles_dir="${psiphon_profiles_dir:-${config_dir}/profiles}"

    ec "${profiles_dir}"
}

function h-psiphon-src-parent {
    local src_parent="${psiphon_src_parent:-${HOME}/.local/src}"

    ec "${src_parent}"
}

function h-psiphon-src-dir {
    local src_parent="${psiphon_src_parent:-$(h-psiphon-src-parent)}"
    local src_dir="${psiphon_src_dir:-${src_parent}/psiphon-tunnel-core}"

    ec "${src_dir}"
}

function h-psiphon-base-config-candidates {
    local config_dir="${psiphon_config_dir:-$(h-psiphon-config-dir)}"
    local base_config="${psiphon_base_config:-${config_dir}/psiphon.config.base}"
    local fallback_config="${psiphon_fallback_config:-${config_dir}/psiphon.config}"

    ec "${base_config}"
    ec "${fallback_config}"
}

function h-psiphon-builtin-base-config-path {
    local config_dir="${psiphon_config_dir:-$(h-psiphon-config-dir)}"
    local builtin_base_config="${psiphon_builtin_base_config:-${config_dir}/psiphon.config.builtin}"

    ec "${builtin_base_config}"
}

function h-psiphon-builtin-base-config {
    local builtin_base_config_json="${psiphon_builtin_base_config_json:-}"

    if test -n "${builtin_base_config_json}" ; then
        ec "${builtin_base_config_json}"
        return 0
    fi

    command cat <<'EOF'
{
  "LocalHttpProxyPort": 2080,
  "LocalSocksProxyPort": 1080,
  "PropagationChannelId": "FFFFFFFFFFFFFFFF",
  "RemoteServerListSignaturePublicKey": "MIICIDANBgkqhkiG9w0BAQEFAAOCAg0AMIICCAKCAgEAt7Ls+/39r+T6zNW7GiVpJfzq/xvL9SBH5rIFnk0RXYEYavax3WS6HOD35eTAqn8AniOwiH+DOkvgSKF2caqk/y1dfq47Pdymtwzp9ikpB1C5OfAysXzBiwVJlCdajBKvBZDerV1cMvRzCKvKwRmvDmHgphQQ7WfXIGbRbmmk6opMBh3roE42KcotLFtqp0RRwLtcBRNtCdsrVsjiI1Lqz/lH+T61sGjSjQ3CHMuZYSQJZo/KrvzgQXpkaCTdbObxHqb6/+i1qaVOfEsvjoiyzTxJADvSytVtcTjijhPEV6XskJVHE1Zgl+7rATr/pDQkw6DPCNBS1+Y6fy7GstZALQXwEDN/qhQI9kWkHijT8ns+i1vGg00Mk/6J75arLhqcodWsdeG/M/moWgqQAnlZAGVtJI1OgeF5fsPpXu4kctOfuZlGjVZXQNW34aOzm8r8S0eVZitPlbhcPiR4gT/aSMz/wd8lZlzZYsje/Jr8u/YtlwjjreZrGRmG8KMOzukV3lLmMppXFMvl4bxv6YFEmIuTsOhbLTwFgh7KYNjodLj/LsqRVfwz31PgWQFTEPICV7GCvgVlPRxnofqKSjgTWI4mxDhBpVcATvaoBl1L/6WLbFvBsoAUBItWwctO2xalKxF5szhGm8lccoc5MZr8kfE0uxMgsxz4er68iCID+rsCAQM=",
  "RemoteServerListURL": "https://s3.amazonaws.com//psiphon/web/mjr4-p23r-puwl/server_list_compressed",
  "SponsorId": "FFFFFFFFFFFFFFFF",
  "UseIndistinguishableTLS": true
}
EOF
  # "RemoteServerListURLs": [
  #   "https://s3.amazonaws.com//psiphon/web/mjr4-p23r-puwl/server_list_compressed"
  # ],
}

function h-psiphon-write-json-file {
    local output="${1}"
    local json="${2}"
    local tmp="${output}.tmp"

    ec "${json}" > "${tmp}" @RET

    if ! command jq empty -- "${tmp}" > /dev/null ; then
        trs-rm "${tmp}" || true
        return 1
    fi

    assert command mv -- "${tmp}" "${output}" @RET
}

function h-psiphon-write-builtin-base-config {
    local config_dir="${psiphon_config_dir:-$(h-psiphon-config-dir)}"
    local builtin_base_config="${psiphon_builtin_base_config:-$(h-psiphon-builtin-base-config-path)}"
    local force_p="${psiphon_builtin_base_config_force_p:-n}"

    ensure-cmd jq @RET
    mkdir-m "${config_dir}" @RET

    if test -f "${builtin_base_config}" && ! bool "${force_p}" ; then
        ec "${builtin_base_config}"
        return 0
    fi

    local json
    json="$(h-psiphon-builtin-base-config)" @TRET

    h-psiphon-write-json-file "${builtin_base_config}" "${json}" @RET
    ec "${builtin_base_config}"
}

function h-psiphon-base-config {
    local use_builtin_p="${psiphon_use_builtin_base_config_p:-y}"

    local candidates_s
    candidates_s="$(h-psiphon-base-config-candidates)" @TRET

    local -a candidates
    candidates=("${(@f)candidates_s}")

    local config
    for config in "${candidates[@]}" ; do
        if test -f "${config}" ; then
            ec "${config}"
            return 0
        fi
    done

    if bool "${use_builtin_p}" ; then
        h-psiphon-write-builtin-base-config @RET
        return 0
    fi

    ecerr "$0: no base config found"
    ecerr "Put a working Psiphon client config at:"

    for config in "${candidates[@]}" ; do
        ecerr "  ${config}"
    done

    ecerr "or enable the bundled fallback with:"
    ecerr "  psiphon_use_builtin_base_config_p=y"
    return 1
}

function h-psiphon-binary-url {
    local platform="${1}"
    local arch="${2}"
    local base_url="${psiphon_binary_base_url:-https://raw.githubusercontent.com/Psiphon-Labs/psiphon-tunnel-core-binaries/master}"

    case "${platform}:${arch}" in
        linux:x86_64)
            ec "${base_url}/linux/psiphon-tunnel-core-x86_64"
            ;;
        *)
            return 1
            ;;
    esac
}

function h-psiphon-curl-download {
    local url="${1}"
    local output="${2}"
    local proxyenv="${psiphon_proxyenv:-${proxyenv:-}}"

    $proxyenv command curl --fail --location --output "${output}" -- "${url}"
}

function h-psiphon-download-binary {
    local platform="${1}"
    local arch="${2}"
    local binary="${3}"

    local url
    url="$(h-psiphon-binary-url "${platform}" "${arch}")" || {
        ecerr "$0: no prebuilt binary for ${platform}/${arch}"
        return 1
    }

    ensure-cmd curl @RET

    local tmp_binary="${binary}.tmp"

    ec "trying prebuilt binary: ${url}"

    if ! h-psiphon-curl-download "${url}" "${tmp_binary}" ; then
        trs-rm "${tmp_binary}" || true
        ecerr "$0: no usable prebuilt binary for ${platform}/${arch}"
        return 1
    fi

    assert command chmod +x "${tmp_binary}" @RET
    assert command mv -- "${tmp_binary}" "${binary}" @RET

    ec "installed prebuilt: ${binary}"
}

function h-psiphon-platform {
    local uname_s
    uname_s="$(command uname -s)" @TRET

    case "${uname_s}" in
        Darwin)
            ec darwin
            ;;
        Linux)
            ec linux
            ;;
        *)
            ecerr "$0: unsupported platform: ${uname_s}"
            return 1
            ;;
    esac
}

function h-psiphon-arch {
    local uname_m
    uname_m="$(command uname -m)" @TRET

    case "${uname_m}" in
        arm64|aarch64)
            ec arm64
            ;;
        x86_64|amd64)
            ec x86_64
            ;;
        *)
            ecerr "$0: unsupported arch: ${uname_m}"
            return 1
            ;;
    esac
}

function h-psiphon-port {
    local spec="${1}"

    if [[ "${spec}" =~ '^[0-9][0-9]$' ]] ; then
        ec "10${spec}"
    elif [[ "${spec}" =~ '^[0-9]{4,5}$' ]] ; then
        ec "${spec}"
    else
        ecerr "$0: invalid port spec: '${spec}'"
        return 1
    fi
}

function h-psiphon-profile-name {
    local region="${1}"
    local upstream_socks_port="${2}"

    local country="${region:l}"
    if test -z "${country}" ; then
        country="any"
    fi

    local port_suffix="${upstream_socks_port#10}"
    ec "${country}${port_suffix}"
}

function h-psiphon-render-profile-config {
    local base_config="${1}"
    local output="${2}"
    local region="${psiphon_region:-}"

    local upstream_host="${psiphon_upstream_host:-127.0.0.1}"

    local upstream_socks_port="${psiphon_upstream_socks_port}"
    local local_socks_port="${psiphon_local_socks_port:-1080}"
    local local_http_port="${psiphon_local_http_port:-2080}"
    local upstream_proxy_url="${psiphon_upstream_proxy_url}"
    if test -z "${upstream_proxy_url}" && test -n "${upstream_socks_port}" ; then
        upstream_proxy_url="socks5://${upstream_host}:${upstream_socks_port}"
    fi

    local jq_filter
    jq_filter='
        .LocalSocksProxyPort = $local_socks_port
        | .LocalHttpProxyPort = $local_http_port
        | if $upstream_proxy_url != "" then .UpstreamProxyURL = $upstream_proxy_url end
        | if $region == "" then del(.EgressRegion) else .EgressRegion = $region end
    '

    local tmp_output="${output}.tmp"

    if ! command jq \
        --arg upstream_proxy_url "${upstream_proxy_url}" \
        --arg region "${region}" \
        --argjson local_socks_port "${local_socks_port}" \
        --argjson local_http_port "${local_http_port}" \
        "${jq_filter}" \
        -- "${base_config}" > "${tmp_output}" ; then
        trs-rm "${tmp_output}" || true
        return 1
    fi

    assert command mv -- "${tmp_output}" "${output}" @RET
}

function h-psiphon-build-config {
    local region="${psiphon_region:-}"
    local upstream_socks_port="${psiphon_upstream_socks_port}"
    local profiles_dir="${psiphon_profiles_dir:-$(h-psiphon-profiles-dir)}"

    ensure-cmd jq @RET
    mkdir-m "${profiles_dir}" @RET

    local base_config
    base_config="$(h-psiphon-base-config)" @TRET

    local profile_name
    profile_name="$(h-psiphon-profile-name "${region}" "${upstream_socks_port}")" @TRET

    local profile_config="${profiles_dir}/${profile_name}.config"

    psiphon_region="${region}" \
        psiphon_upstream_socks_port="${upstream_socks_port}" \
        h-psiphon-render-profile-config "${base_config}" "${profile_config}" @RET

    ec "${profile_config}"
}

function h-psiphon-tmux-alive {
    local session_name="${psiphon_session_name:-psiphon-client}"

    command tmux has-session -t "${session_name}" &> /dev/null
}

function h-psiphon-go-mod-directive {
    local go_mod="${1}"
    local directive="${2}"

    if ! test -f "${go_mod}" ; then
        return 1
    fi

    case "${directive}" in
        go|toolchain)
            ;;
        *)
            ecerr "$0: unsupported go.mod directive: ${directive}"
            return 1
            ;;
    esac

    command perl -ne '
        BEGIN {
            our $directive = shift @ARGV;
        }

        our $directive;

        if (/^\s*\Q$directive\E\s+(\S+)\s*(?:\/\/.*)?$/) {
            print "$1\n";
            exit 0;
        }
    ' -- "${directive}" "${go_mod}"
}

function h-psiphon-normalize-go-toolchain {
    local version="${1}"

    if test -z "${version}" ; then
        return 1
    fi

    if [[ "${version}" =~ '^go[0-9]+[.][0-9]+([.][0-9]+)?([a-z]+[0-9]+)?$' ]] ; then
        ec "${version}"
    elif [[ "${version}" =~ '^[0-9]+[.][0-9]+[.][0-9]+([a-z]+[0-9]+)?$' ]] ; then
        ec "go${version}"
    elif [[ "${version}" =~ '^[0-9]+[.][0-9]+$' ]] ; then
        ec "go${version}.0"
    else
        return 1
    fi
}

function h-psiphon-go-toolchain-from-go-mod {
    local src_dir="${1}"
    local go_mod="${src_dir}/go.mod"

    local toolchain
    toolchain="$(h-psiphon-go-mod-directive "${go_mod}" toolchain)" || true

    if test -n "${toolchain}" ; then
        h-psiphon-normalize-go-toolchain "${toolchain}" @RET
        return 0
    fi

    local go_version
    go_version="$(h-psiphon-go-mod-directive "${go_mod}" go)" || true

    if test -n "${go_version}" ; then
        h-psiphon-normalize-go-toolchain "${go_version}" @RET
        return 0
    fi

    ec auto
}

function h-psiphon-go-toolchain {
    local src_dir="${1}"
    local go_toolchain="${psiphon_go_toolchain:-}"

    if test -n "${go_toolchain}" ; then
        ec "${go_toolchain}"
        return 0
    fi

    h-psiphon-go-toolchain-from-go-mod "${src_dir}" @RET
}

function h-psiphon-go-build {
    local src_dir="${1}"
    local binary="${2}"

    local go_toolchain
    go_toolchain="$(h-psiphon-go-toolchain "${src_dir}")" @TRET

    ec "go toolchain: ${go_toolchain}"

    (
        builtin cd "${src_dir}" || exit $?
        GOTOOLCHAIN="${go_toolchain}" command go build -o "${binary}" ./ConsoleClient
    ) @RET
}

function h-psiphon-run-config {
    local binary="${psiphon_binary:-$(h-psiphon-binary)}"
    local session_name="${psiphon_session_name:-psiphon-client}"
    local state_dir="${psiphon_state_dir:-$(h-psiphon-state-dir)}"
    local data_dir="${psiphon_data_dir:-$(h-psiphon-data-dir)}"
    local last_config_file="${psiphon_last_config_file:-${state_dir}/last-config}"
    local config="${1}"
    shift

    ensure-cmd tmux @RET

    if ! test -x "${binary}" ; then
        ecerr "$0: missing executable: ${binary}"
        ecerr "Run psiphon-install first."
        return 1
    fi

    mkdir-m "${state_dir}" "${data_dir}" @RET
    ec "${config}" > "${last_config_file}" @RET

    if h-psiphon-tmux-alive ; then
        assert command tmux kill-session -t "${session_name}" @RET
    fi

    local -a cmd
    cmd=(
        "${binary}"
        --config "${config}"
        --dataRootDirectory "${data_dir}"

        --listenInterface='any'
        #: ListenInterface specifies which interface to listen on. If no interface is provided then listen on 127.0.0.1. If 'any' is provided then use 0.0.0.0. If there are multiple IP addresses on an interface use the first IPv4 address.
        #: also settable in JSON: `"ListenInterface": "any",`

        --formatNotices
        "$@"
    )

    assert reval-ecgray tmuxnew "${session_name}" "${cmd[@]}" @RET

    if h-psiphon-tmux-alive ; then
        ec "psiphon up: ${session_name}"
        ec "config: ${config}"
    else
        ecerr "$0: tmux session did not start: ${session_name}"
        return 1
    fi
}

function psiphon-install {
    local bin_dir="${psiphon_bin_dir:-$(h-psiphon-bin-dir)}"
    local binary="${psiphon_binary:-$(h-psiphon-binary)}"
    local src_parent="${psiphon_src_parent:-$(h-psiphon-src-parent)}"
    local src_dir="${psiphon_src_dir:-$(h-psiphon-src-dir)}"
    local repo_url="${psiphon_repo_url:-https://github.com/Psiphon-Labs/psiphon-tunnel-core.git}"
    local branch="${psiphon_branch:-staging-client}"
    local force_p="${psiphon_install_force_p:-n}"
    local download_first_p="${psiphon_install_download_first_p:-y}"

    local platform
    platform="$(h-psiphon-platform)" @TRET

    local arch
    arch="$(h-psiphon-arch)" @TRET

    ec "platform: ${platform}"
    ec "arch: ${arch}"

    mkdir-m "${bin_dir}" "${src_parent}" @RET

    if test -x "${binary}" && ! bool "${force_p}" ; then
        ec "already installed: ${binary}"
        return 0
    fi

    if bool "${download_first_p}" ; then
        if h-psiphon-download-binary "${platform}" "${arch}" "${binary}" ; then
            return 0
        fi
        ecerr "$0: falling back to source build"
    fi

    ensure-cmd git go @RET

    if test -d "${src_dir}/.git" ; then
        assert git --git-dir="${src_dir}/.git" --work-tree="${src_dir}" fetch --all --prune @RET
        assert git --git-dir="${src_dir}/.git" --work-tree="${src_dir}" checkout "${branch}" @RET
        assert git --git-dir="${src_dir}/.git" --work-tree="${src_dir}" pull --ff-only @RET
    else
        assert git clone --depth 1 --branch "${branch}" -- "${repo_url}" "${src_dir}" @RET
    fi

    h-psiphon-go-build "${src_dir}" "${binary}" @RET
    assert command chmod +x "${binary}" @RET

    ec "installed: ${binary}"
}

function psiphon-init-config {
    local force_p="${psiphon_builtin_base_config_force_p:-y}"

    psiphon_builtin_base_config_force_p="${force_p}" h-psiphon-write-builtin-base-config @RET
}

function psiphon-up {
    local region="${psiphon_region:-}"
    local upstream_socks_port="${psiphon_upstream_socks_port}"

    local config
    config="$(psiphon_region="${region}" psiphon_upstream_socks_port="${upstream_socks_port}" h-psiphon-build-config)" @TRET

    h-psiphon-run-config "${config}" "$@" @RET
}

function psiphon-down {
    local session_name="${psiphon_session_name:-psiphon-client}"

    ensure-cmd tmux @RET

    if h-psiphon-tmux-alive ; then
        assert command tmux kill-session -t "${session_name}" @RET
        ec "psiphon down: ${session_name}"
    else
        ec "psiphon already down: ${session_name}"
    fi
}

function psiphon-toggle {
    local state_dir="${psiphon_state_dir:-$(h-psiphon-state-dir)}"
    local last_config_file="${psiphon_last_config_file:-${state_dir}/last-config}"

    if h-psiphon-tmux-alive ; then
        psiphon-down @RET
        return 0
    fi

    if test -f "${last_config_file}" ; then
        local config
        config="$(< "${last_config_file}")" @TRET
        h-psiphon-run-config "${config}" "$@" @RET
    else
        psiphon-up "$@" @RET
    fi
}

function psiphon-status {
    local session_name="${psiphon_session_name:-psiphon-client}"

    local state_dir="${psiphon_state_dir:-$(h-psiphon-state-dir)}"
    local last_config_file="${psiphon_last_config_file:-${state_dir}/last-config}"

    if h-psiphon-tmux-alive ; then
        ec "up: ${session_name}"
        while IFS= read -r line ; do
            ec "  ${line}"
        done < "${last_config_file}" || true
    else
        ec "down: ${session_name}"
        return 1
    fi
}

function psiphon-logs {
    local session_name="${psiphon_session_name:-psiphon-client}"

    ensure-cmd tmux @RET

    if ! h-psiphon-tmux-alive ; then
        ecerr "$0: not running: ${session_name}"
        return 1
    fi

    assert command tmux attach-session -t "${session_name}" @RET
}

function psiphon-test {
    local local_socks_port="${psiphon_local_socks_port:-1080}"
    local url="${psiphon_test_url:-https://ipinfo.io}"

    ensure-cmd curl @RET
    assert command curl --socks5-hostname "127.0.0.1:${local_socks_port}" -- "${url}" @RET
}

function h-psiphon-default-regions {
    ec US
    ec GB
    ec DE
    ec NL
    ec CA
    ec SG
    ec JP
    ec FR
}

function h-psiphon-default-port-specs {
    local ports=(75 81 2089 93 96)

    arrnn ${ports[@]}
}

function h-psiphon-regions {
    ensure-array psiphon_good_regions

    if (( ${#psiphon_good_regions[@]} > 0 )) ; then
        ec "${(@F)psiphon_good_regions}"
    else
        h-psiphon-default-regions
    fi
}

function h-psiphon-port-specs {
    ensure-array psiphon_upstream_port_specs

    if (( ${#psiphon_upstream_port_specs[@]} > 0 )) ; then
        ec "${(@F)psiphon_upstream_port_specs}"
    else
        h-psiphon-default-port-specs
    fi
}

function h-psiphon-valid-function-name-p {
    local name="${1}"

    [[ "${name}" =~ '^[A-Za-z_][A-Za-z0-9_-]*$' ]]
}

function h-psiphon-def-up-shortcut {
    local name="${1}"
    local region="${2}"
    local port="${3}"

    if ! h-psiphon-valid-function-name-p "${name}" ; then
        ecerr "$0: invalid function name: ${name}"
        return 1
    fi

    local body

    if test -n "${region}" && test -n "${port}" ; then
        body="psiphon_region=${(qq)region} psiphon_upstream_socks_port=${(qq)port} psiphon-up \"\$@\""
    elif test -n "${region}" ; then
        body="psiphon_region=${(qq)region} psiphon-up \"\$@\""
    elif test -n "${port}" ; then
        body="psiphon_upstream_socks_port=${(qq)port} psiphon-up \"\$@\""
    else
        return 1
    fi

    functions[${name}]="${body}"
}

function h-psiphon-def-up-shortcuts {
    local regions_s
    regions_s="$(h-psiphon-regions)" @TRET

    local port_specs_s
    port_specs_s="$(h-psiphon-port-specs)" @TRET

    local -a regions
    regions=("${(@f)regions_s}")

    local -a port_specs
    port_specs=("${(@f)port_specs_s}")

    local region
    local port_spec
    local port
    local region_l

    for region in "${regions[@]}" ; do
        region_l="${region:l}"

        h-psiphon-def-up-shortcut "psiphon-up-${region_l}" "${region}" "" @RET

        for port_spec in "${port_specs[@]}" ; do
            port="$(h-psiphon-port "${port_spec}")" @TRET
            h-psiphon-def-up-shortcut "psiphon-up-${region_l}${port_spec}" "${region}" "${port}" @RET
        done
    done

    for port_spec in "${port_specs[@]}" ; do
        port="$(h-psiphon-port "${port_spec}")" @TRET
        h-psiphon-def-up-shortcut "psiphon-up-${port_spec}" "" "${port}" @RET
    done
}

h-psiphon-def-up-shortcuts
