sumgensim() {
	# text="$(wread "$1" text)"
	# lynx -dump -nolist
	# elinks can be used for this too, as it also has a -dump option (and has -no-references to omit the list of links)
     text="$(w3m -dump "$1")" word_count="${2:-150}" serr python -c 'from gensim.summarization import summarize ; import os; print(summarize(os.environ["text"], word_count=int(os.environ["word_count"])))'
}
sumym () {
    # https://pypi.org/project/sumy/
    # text-rank and kl 
    sumy "${2:-lex-rank}" --length=7 --url="$1"
}
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
local url
local urls=()
for url in "$@"
do
	urls+="-u"
	urls+="$url"
done

while :
do
    # Use git+https://github.com/s0hv/rsstail.py .
    # python -m rsstail -n 0 --striphtml --nofail --interval $((60*15)) --format '{title}
# {link}
# ' "$@"
    # python's rsstail sucks

    # https://github.com/flok99/rsstail
    rsstail -i 15 -l -n 0 -N "${urls[@]}" 2>> $log | tee -a $log | while read -d $'\n' -r t; do
    read -d $'\n' -r l
    t="$(<<<"$t" html2utf.py)"
    for c in $conditions[@]
    do
        reval "$c" "$l" "$t" || continue 2
    done
    ec "$t
    $l
    "
    test -n "$notel" || ensurerun "150s" tsend --link-preview -- "${id}" "$t
$l

gensim: $(sumgensim "$l")"

# Lex-rank: $(sumym "$l")"

# kl: $(sumym "$l" kl)"
    sleep 120 #because wuxia sometimes sends unupdated pages
    reval "$engine[@]" "$l" "$t"
done
echo restarting "$0 $@" | tee -a $log
sleep 1 # allows us to terminate the program
done
}
