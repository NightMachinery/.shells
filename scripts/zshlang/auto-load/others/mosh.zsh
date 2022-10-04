function mosh-jump {
    #: `sudo apt-get install -y socat mosh`
    ##
    local orig_args=("$@")
    local mosh_server_args=()
    local proxy
    local proxy_port
    local target

    while [[ $# -gt 0 ]]; do
        case "$1" in
            --jump-server=*)
                proxy=${1#--jump-server=}
                ;;
            -J | --jump-server)
                shift
                proxy=$1
                ;;
            --proxy-port=*)
                proxy_port=${1#--proxy-port=}
                ;;
            -P | --proxy-port)
                shift
                proxy_port=$1
                ;;
            -p)
                mosh_server_args+=("$1" "$2")
                shift
                ;;
            -*)
                mosh_server_args+=("$1")
                ;;
            *)
                if [[ -z $target ]]; then
                    target=$1
                else
                    mosh_server_args+=("$1")
                fi
                ;;
        esac
        shift
    done

    # if we're not using a jump server, let's just call mosh directly
    if [[ -z $proxy ]]; then
        mosh "${orig_args[@]}"
        return $?
    fi

    # we may be dealing with host aliases, so let's have SSH resolve that first so
    # that we can get the host. however, we'll continue using $proxy and $target for
    # SSH itself.
    proxy_host="$(ssh -G "$proxy" | perl -anE '/^hostname (.+)/ && print "$1"')" @TRET

    # proxy_ip="$(dig "$proxy_host" +short)" @TRET
    proxy_ip="$proxy_host"

    target_host="$(ssh -G "$target" | perl -anE '/^hostname (.+)/ && print "$1"')" @TRET

    ecbold "proxy: $proxy"
    ecbold "proxy host: $proxy_host"
    ecbold "proxy ip address: $proxy_ip"
    ecbold "target: $target"
    ecbold "target host: $target_host"

    # start up the mosh server via the proxy
    mosh_server_out="$(ssh -J "$proxy" "$target" -- mosh-server new -c 256 -s -l LANG=en_US.UTF-8 -l LANGUAGE=en_US "${mosh_server_args[@]}")" @TRET

    # parse out the port and key; we'll need the port for proxying
    connect_line="$(ggrep -m1 '^MOSH CONNECT ' <<< "$mosh_server_out")" @TRET
    target_port="$(perl -anE 'print $F[2]' <<< "$connect_line")" @TRET
    key="$(perl -anE 'print $F[3]' <<< "$connect_line")" @TRET

    if [[ -z "$proxy_port" ]]; then
        # we'll need find a free port on the proxy, which might be different from the target
        find_free_port_cmd="$(
            cat << EOF
for (( p=$target_port ; p < $target_port + 10; p++ )); do
  if [[ -z \$(ss -Hlu sport = \$p) ]]; then
    echo "\$p"
    exit
  fi
done
exit 1
EOF
                          )" @TRET
        proxy_port="$(ssh -n "$proxy" bash -c "${(qqq@)find_free_port_cmd}")" @TRET
    fi

    ecbold "proxy port: $proxy_port"
    ecbold "target port: $target_port"

    # set up UDP proxying
    # overall, we have client <--> proxy:$proxy_port <--> target:$target_port
    tmuxdaemon ssh -tt -n "$proxy" socat udp-l:"$proxy_port",fork udp:"$target_host":"$target_port"

    MOSH_KEY=$key mosh-client "$proxy_ip" "$proxy_port"
}
