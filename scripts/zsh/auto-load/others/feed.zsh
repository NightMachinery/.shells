rss-ctitle() {
    ggrep -P --silent "$rc_t" <<< "$2"
}
rss-tsend() {
mkdir -p ~/logs/
local log=~/logs/rss-tsend.log
local engine=("${rt_e[@]:-tl}")
local conditions=( ${rt_c[@]} )
local notel="${rt_notel}"
local c
local id="${rt_id:--1001293952668}"

while :
do
    python -m rsstail -n 0 --striphtml --nofail --interval $((60*15)) --format '{title}
{link}
' "$@" |& tee -a $log | while read -d $'\n' -r t; do
    read -d $'\n' -r l
    t="$(<<<"$t" recode html..utf8)"
    for c in $conditions[@]
    do
        reval "$c" "$l" "$t" || continue 2
    done
    ec "$t
    $l
    "
    test -n "$notel" || ensurerun "150s" tsend -- "${id}" "$t
$l"
    sleep 120 #because wuxia sometimes sends unupdated pages
    reval "$engine[@]" "$l" "$t"
done
echo restarting "$0 $@" | tee -a $log
sleep 1 # allows us to terminate the program
done
}
