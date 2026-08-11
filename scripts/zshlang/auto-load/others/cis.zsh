##
#: The CIS LMU cluster. Host aliases themselves live in ~/.ssh/config (one
#: `Host` block mapping every short name to <name>.cis.lmu.de as feraidoon), so
#: `ssh beta` works without any of this. What lives here is the *set*
#: membership that ssh_config cannot express.
#:
#: Reachability probed 2026-08-11. Not listed at all: seceda, puez and grancir
#: reject the key (Barbara's Team; seceda is the only H200 box), and tau, lambda
#: and kappa time out.
##
typeset -ga night_cis_hosts night_cis_gpu_hosts

night_cis_hosts=(
    beta rho1 rho2 zeta1 zeta2
    epsilon1 epsilon2 epsilon3 epsilon4 epsilon5 epsilon6 epsilon7
    alpha omega pi sigma delta
)

#: Hosts worth asking about GPUs. alpha, pi, sigma and delta have none at all,
#: and omega's Tesla K20Xm has no driver -- including them only burns a timeout
#: each. epsilon5 *is* kept: it has hardware with a broken driver, and
#: `gpu-status` reports that as `driver-missing` rather than hiding it.
night_cis_gpu_hosts=(
    beta rho1 rho2 zeta1 zeta2
    epsilon1 epsilon2 epsilon3 epsilon4 epsilon5 epsilon6 epsilon7
)
##
function cis-gpus {
    #: Passing the hosts via --hosts rather than positionally keeps every user
    #: flag working, e.g. `cis-gpus --format yaml --min-free 40G`.
    gpu-status --hosts "${(j:,:)night_cis_gpu_hosts}" "$@"
}

function cis-hosts {
    arrn "${night_cis_hosts[@]}"
}
##
