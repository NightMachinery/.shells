rss-tsend() {
mkdir -p ~/logs/
local log=~/logs/rss-tsend.log

while :
do
local engine=("${rt_e[@]:-tl}")
rss-notifier.zsh "$1" "$2"|& tee -a $log | while read -d "" -r t; do                                
    read -d "" -r l
    ec "$t
    $l
    "
    ensure-run.zsh "150s" tsend -- "${3:-me}" "$t
$l"
    sleep 120 #because wuxia sometimes sends unupdated pages
    reval "$engine[@]" "$l" "$t"
done
echo restarting rss-tsend for "$2" | tee -a $log
done
}
