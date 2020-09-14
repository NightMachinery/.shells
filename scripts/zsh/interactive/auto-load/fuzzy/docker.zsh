# Get more commands from https://github.com/mnowotnik/extra-fzf-completions/blob/master/zsh/docker-fzf-completion.zsh
###
# FZF_DOCKER_PS_FORMAT="table {{.ID}}\t{{.Names}}\t{{.Image}}\t{{.Ports}}"
# FZF_DOCKER_PS_FORMAT="table {{.ID}}     {{.Names}}     {{.Image}}     {{.Ports}}     {{.Status}}     {{.RunningFor}}"
FZF_DOCKER_PS_FORMAT="table {{.ID}}\t{{.Names}}\t{{.Image}}\t{{.Ports}}\t{{.Status}}\t{{.RunningFor}}"
# FZF_DOCKER_PS_START_FORMAT="table {{.ID}}\t{{.Names}}\t{{.Status}}\t{{.Image}}"

function ffdocker-ps() {
    fz --header-lines=1 --with-nth '2..' "$@" <<<"$(docker ps --all --format "${FZF_DOCKER_PS_FORMAT}")" | awk '{print $1}'
}

function ffdocker-start() {
    local ps
    ps="$(ffdocker-ps)" || return 1
    rgeval docker start "$ps"
}
function ffdocker-stop() {
    local ps
    ps="$(ffdocker-ps)" || return 1
    rgeval docker stop "$ps"
}

function ffdocker-rm() {
    local ps
    ps="$(ffdocker-ps)" || return 1
    rgeval docker rm "$ps"
}
