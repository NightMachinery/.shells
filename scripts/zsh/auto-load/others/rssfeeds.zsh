sumgensim() {
    doc "Not recced. Doesn't respect sentence boundaries. We are also feeding it via env vars which is also really bad because env vars have a limited size."

    # text="$(wread "$1" text)"
    # lynx -dump -nolist
    # elinks can be used for this too, as it also has a -dump option (and has -no-references to omit the list of links)
    text="$(w3m -dump "$1")" word_count="${2:-150}" serr python -c 'from gensim.summarization import summarize ; import os; print(summarize(os.environ["text"], word_count=int(os.environ["word_count"])))'
}
sumym () {
    local url="$1"

    # https://pypi.org/project/sumy/
    # lex-rank, text-rank
    # kl can be crazy on CPU. It also is probably worse than lex-rank.
    sumy "${2:-lex-rank}" --length=7 --file =(readmoz-txt "$url") --format=plaintext
}
rss-ctitle() {
    ggrep -P --silent "$rc_t[@]" <<< "$2"
}
rss-tsend() {
    ensure-redis || return 1
    mkdir -p ~/logs/
    local log=~/logs/rss-tsend.log
    local engine=("${rt_e[@]:-tl}")
    local conditions=( ${rt_c[@]} )
    local notel="${rt_notel}"
    local c
    local id="${rt_id:--1001293952668}"
    local url
    local urls=()
    local rssurls="rssurls $*"
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

            ! (( $(redism SISMEMBER $rssurls "$l") )) || { ec "Duplicate link: $l"$'\n'"Skipping ..." ; continue }

            t="$(<<<"$t" html2utf.py)"
            for c in $conditions[@]
            do
              reval "$c" "$l" "$t" || { ecdate "Skipping $t $l" ; continue 2 }
            done
            ec "$t"

            labeled redism SADD $rssurls "$l"
        ecdate test2 sumy start
        sumym https://github.com/LisaDziuba/Marketing-for-Engineers
        ecdate test2 sumy end
            # ensurerun "150s" tsend ... 
            ( test -n "$notel" || dbg revaldbg tsend --link-preview -- "${id}" "$t"$'\n'"${l}"$'\n'"Lex-rank: $(dbg revaldbg sumym "$l")" )
            sleep 120 #because wuxia sometimes sends unupdated pages
            dbg revaldbg "$engine[@]" "$l" "$t"
        done
        ecdate restarting "$0 $@ (exit: $?)" | tee -a $log
        ecdate test sumy start
        sumym https://github.com/LisaDziuba/Marketing-for-Engineers
        ecdate test sumy end
        sleep 1 # allows us to terminate the program
    done
    ecdate Exiting. This is a bug.
}
